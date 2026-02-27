# Tests for motis_server functions
library(testthat)

# Helper to get the real .motis_state from the namespace at call time.
# We look it up dynamically each time rather than caching, because
# devtools::test() reloads the package between test files.
get_motis_state <- function() {
  get(".motis_state", envir = asNamespace("rmotis"))
}

# Helper: temporarily replace .motis_state contents in-place, restore on exit.
# We modify the real environment's contents rather than replacing it, because
# R's bytecode compiler caches environment references.
with_clean_state <- function(registry = list(),
                             session_id = "test-session-123",
                             code) {
  state <- get_motis_state()
  old_registry <- state$registry
  old_session_id <- state$session_id
  state$registry <- registry
  state$session_id <- session_id
  on.exit({
    state$registry <- old_registry
    state$session_id <- old_session_id
  }, add = TRUE)
  force(code)
}

# Helper to create a mock processx::process
create_mock_process <- function(pid = 12345, alive = TRUE) {
  structure(
    list(
      get_pid = function() pid,
      is_alive = function() alive,
      kill = function() TRUE,
      read_output_lines = function() character(0),
      read_error_lines = function() character(0)
    ),
    class = c("process", "list")
  )
}

# Custom mocking helper for internal *functions* only
# (do NOT use this for .motis_state — use with_clean_state instead)
with_internal_mocks <- function(mocks, code) {
  ns <- asNamespace("rmotis")
  original_values <- list()

  for (name in names(mocks)) {
    if (exists(name, envir = ns, inherits = FALSE)) {
      original_values[[name]] <- get(name, envir = ns)
      if (bindingIsLocked(name, ns)) unlockBinding(name, ns)
      assign(name, mocks[[name]], envir = ns)
    }
  }

  on.exit({
    for (name in names(original_values)) {
      if (bindingIsLocked(name, ns)) unlockBinding(name, ns)
      assign(name, original_values[[name]], envir = ns)
    }
  }, add = TRUE)

  force(code)
}

test_that("motis_start_server registers a new server", {
  skip_if_not_installed("processx")
  skip_if(packageVersion("testthat") < "3.2.0", "Requires testthat >= 3.2.0")

  tmp_dir <- normalizePath(tempdir())
  data_dir <- file.path(tmp_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  file.create(file.path(data_dir, "config.yml"))

  MockProcess <- list(
    new = function(command, args, wd, stdout, stderr, ...) {
      create_mock_process()
    }
  )

  with_mocked_bindings({
    with_clean_state(registry = list(), session_id = "test-session-123", {
      with_internal_mocks(
        list(
          resolve_motis_cmd = function(...) "mock-motis",
          read_motis_config = function(...) list(),
          motis_set_server_address = function(...) TRUE,
          .motis_registry_dir = function() tmp_dir
        ),
        {
          server <- motis_start_server(
            work_dir = tmp_dir, port = 9999, echo_cmd = FALSE
          )

          expect_s3_class(server, "motis_server")
          expect_equal(attr(server, "motis_port"), 9999)
          expect_equal(attr(server, "motis_dir"), tmp_dir)

          # Check registry was updated in the real state (in-place)
          expect_length(get_motis_state()$registry, 1)
          entry <- get_motis_state()$registry[[1]]
          expect_equal(entry$port, 9999)
          expect_equal(entry$pid, 12345)
        }
      )
    })
  },
  process = MockProcess,
  .package = "processx"
  )
})

test_that("motis_servers lists registered servers", {
  mock_registry <- list(
    "id1" = list(
      id = "id1", pid = 1001, port = 8081,
      work_dir = "/tmp/1", proc = create_mock_process(1001)
    ),
    "id2" = list(
      id = "id2", pid = 1002, port = 8082,
      work_dir = "/tmp/2", proc = NULL
    )
  )

  with_clean_state(registry = mock_registry, code = {
    with_internal_mocks(
      list(
        .motis_pid_is_running = function(pid) FALSE,
        .motis_registry_dir = function() tempdir()
      ),
      {
        df <- motis_servers(include_all = FALSE)
        expect_equal(nrow(df), 2)
        expect_equal(df$port, c(8081, 8082))
        expect_true(df$alive[1])
      }
    )
  })
})

test_that("motis_stop_server stops by ID", {
  mock_proc <- create_mock_process(1001)
  mock_registry <- list(
    "id1" = list(
      id = "id1", pid = 1001, port = 8081,
      work_dir = "/tmp/1", proc = mock_proc
    )
  )

  with_clean_state(registry = mock_registry, code = {
    with_internal_mocks(
      list(.motis_registry_dir = function() tempdir()),
      {
        res <- motis_stop_server(id = "id1")
        expect_true(res)
        expect_length(get_motis_state()$registry, 0)
      }
    )
  })
})

test_that("registry persistence works (manual JSON)", {
  tmp_reg_dir <- file.path(tempdir(), "motis_reg_test")
  dir.create(tmp_reg_dir, showWarnings = FALSE)
  on.exit(unlink(tmp_reg_dir, recursive = TRUE))

  test_registry <- list(
    "srv1" = list(
      id = "srv1", pid = 123, port = 8080,
      work_dir = "/tmp/simple/path", running = TRUE
    )
  )

  with_clean_state(registry = test_registry, session_id = "sess1", {
    with_internal_mocks(
      list(.motis_registry_dir = function() tmp_reg_dir),
      {
        # Save
        rmotis:::.motis_registry_save()
        expected_file <- file.path(tmp_reg_dir, "sess1.json")
        expect_true(file.exists(expected_file))

        # Check content
        content <- paste(readLines(expected_file), collapse = "")
        expect_true(grepl("srv1", content))
        expect_true(grepl("8080", content))
        expect_true(grepl("/tmp/simple/path", content))

        # Load after clearing
        state <- get_motis_state()
        state$registry <- list()
        rmotis:::.motis_registry_load()

        expect_length(state$registry, 1)
        if (length(state$registry) > 0) {
          expect_equal(state$registry$srv1$port, 8080)
        }
      }
    )
  })
})
