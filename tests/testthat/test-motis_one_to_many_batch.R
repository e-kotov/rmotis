test_that("motis_one_to_many_batch requires valid data_dir", {
  one <- data.frame(lat = 59.3, lon = 18.0, id = "A")
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2),
                     id = c("X", "Y"))

  # Use a regex that matches both Unix and Windows error messages from normalizePath
  expect_error(
    suppressWarnings(motis_one_to_many_batch(one, many, data_dir = "/nonexistent/path")),
    "nonexistent"
  )
})

test_that("motis_one_to_many_batch respects output_dir and keep_files", {
  skip_on_cran()

  one <- data.frame(lat = 59.3, lon = 18.0, id = "A")
  many <- data.frame(lat = c(59.4, 59.5), lon = c(18.1, 18.2),
                     id = c("X", "Y"))
  
  mock_run <- function(command, args, ...) {
    r_idx <- which(args == "-r") + 1
    response_file <- args[r_idx]
    writeLines('[{"duration":123,"distance":456},{"duration":789,"distance":1011}]', response_file)
    list(status = 0L, stdout = "Success", stderr = "")
  }
  
  mock_resolve <- function(...) "/fake/motis"
  
  custom_dir <- tempfile("motis_batch_test_")
  dir.create(custom_dir)
  on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)
  
  testthat::with_mocked_bindings(
    run = mock_run,
    .package = "processx",
    code = {
      testthat::with_mocked_bindings(
        resolve_motis_cmd = mock_resolve,
        .package = "rmotis",
        code = {
          msgs <- capture.output({
            res <- motis_one_to_many(
              one, many, 
              data_dir = ".", 
              engine = "batch",
              motis_path = "/fake/motis", # dummy for mock
              temp_dir = custom_dir,
              keep_files = TRUE,
              echo = TRUE
            )
          }, type = "message")
          
          expect_true(any(grepl("Query file: .* bytes, .* lines", msgs)))
          files <- list.files(custom_dir)
          expect_true(any(grepl("motis_query_.*\\.txt$", files)))
          expect_equal(nrow(res), 2)
          expect_equal(res$duration_s, c(123, 789))
        }
      )
    }
  )
})

test_that("motis_one_to_many_batch respects split parameter", {
  skip_on_cran()

  one <- data.frame(lat = 50, lon = 8, id = "1")
  many <- data.frame(lat = c(50.1, 50.2, 50.3, 50.4), 
                     lon = c(8.1, 8.2, 8.3, 8.4),
                     id = c("A", "B", "C", "D"))
  
  mock_run <- function(command, args, ...) {
    q_idx <- which(args == "-q") + 1
    r_idx <- which(args == "-r") + 1
    query_file <- args[q_idx]
    response_file <- args[r_idx]
    n_lines <- length(readLines(query_file))
    res_content <- replicate(n_lines, '[{"duration":100},{"duration":200}]', simplify = TRUE)
    writeLines(res_content, response_file)
    list(status = 0L, stdout = "Success", stderr = "")
  }
  
  mock_resolve <- function(...) "/fake/motis"
  
  testthat::with_mocked_bindings(
    run = mock_run,
    .package = "processx",
    code = {
      testthat::with_mocked_bindings(
        resolve_motis_cmd = mock_resolve,
        .package = "rmotis",
        code = {
          msgs <- capture.output({
              res <- motis_one_to_many(
                one, many, 
                data_dir = ".", 
                engine = "batch",
                max_destinations_per_batch = 2L,
                spatial_filter_km = NULL,
                spatial_sort = FALSE,
                echo = TRUE
              )
          }, type = "message")
          
          expect_true(any(grepl("Query file: .* 2 lines", msgs)))
          expect_equal(nrow(res), 4)
        }
      )
    }
  )
})

test_that("spatial_sort reorders origins by latitude", {
  skip_on_cran()
  
  one <- data.frame(lat = c(60, 58, 59), lon = c(18, 18, 18), id = c("C", "A", "B"))
  many <- data.frame(lat = 59.5, lon = 18.0, id = "X")
  
  mock_run <- function(command, args, ...) {
    r_idx <- which(args == "-r") + 1
    q_idx <- which(args == "-q") + 1
    n_lines <- length(readLines(args[q_idx]))
    writeLines(replicate(n_lines, '[{"duration":100}]'), args[r_idx])
    list(status = 0L, stdout = "Success", stderr = "")
  }
  
  mock_resolve <- function(...) "/fake/motis"
  
  testthat::with_mocked_bindings(
    run = mock_run,
    .package = "processx",
    code = {
      testthat::with_mocked_bindings(
        resolve_motis_cmd = mock_resolve,
        .package = "rmotis",
        code = {
          custom_dir <- tempfile("motis_batch_sort_")
          dir.create(custom_dir)
          on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)
          
          res <- motis_one_to_many(
            one, many,
            data_dir = ".",
            engine = "batch",
            temp_dir = custom_dir,
            keep_files = TRUE,
            spatial_sort = TRUE,
            spatial_filter_km = NULL,
            echo = FALSE
          )
          
          query_files <- list.files(custom_dir, pattern = "motis_query.*\\.txt$", full.names = TRUE)
          meta_file <- paste0(query_files[1], ".meta")
          meta_lines <- readLines(meta_file)
          origin_ids <- vapply(strsplit(meta_lines, "\t"), `[`, character(1), 1)
          expect_equal(origin_ids, c("A", "B", "C"))
        }
      )
    }
  )
})

test_that("spatial_filter reduces destinations per origin", {
  skip_on_cran()
  
  one <- data.frame(lat = 59.3, lon = 18.0, id = "Stockholm")
  many <- data.frame(
    lat = c(59.4, 59.5, 35.0, 51.5), 
    lon = c(18.1, 18.2, 139.7, -0.1),
    id = c("Near1", "Near2", "Tokyo", "London")
  )
  
  mock_run <- function(command, args, ...) {
    r_idx <- which(args == "-r") + 1
    q_idx <- which(args == "-q") + 1
    n_lines <- length(readLines(args[q_idx]))
    writeLines(replicate(n_lines, '[{"duration":100}]'), args[r_idx])
    list(status = 0L, stdout = "Success", stderr = "")
  }
  
  mock_resolve <- function(...) "/fake/motis"
  
  testthat::with_mocked_bindings(
    run = mock_run,
    .package = "processx",
    code = {
      testthat::with_mocked_bindings(
        resolve_motis_cmd = mock_resolve,
        .package = "rmotis",
        code = {
          custom_dir <- tempfile("motis_batch_filter_")
          dir.create(custom_dir)
          on.exit(unlink(custom_dir, recursive = TRUE), add = TRUE)
          
          res <- motis_one_to_many(
            one, many,
            data_dir = ".",
            engine = "batch",
            temp_dir = custom_dir,
            keep_files = TRUE,
            mode = "WALK",
            spatial_filter_km = 15,
            echo = FALSE
          )
          
          query_files <- list.files(custom_dir, pattern = "motis_query.*\\.txt$", full.names = TRUE)
          meta_file <- paste0(query_files[1], ".meta")
          meta_lines <- readLines(meta_file)
          meta_parts <- strsplit(meta_lines, "\t")[[1]]
          dest_ids <- meta_parts[-1]
          expect_length(dest_ids, 1)
          expect_equal(dest_ids, "Near1")
        }
      )
    }
  )
})

test_that("spatial_filter=FALSE includes all destinations", {
  skip_on_cran()
  
  one <- data.frame(lat = 59.3, lon = 18.0, id = "Stockholm")
  many <- data.frame(lat = c(59.4, 35.0), lon = c(18.1, 139.7), id = c("Near", "Tokyo"))
  
  mock_run <- function(command, args, ...) {
    r_idx <- which(args == "-r") + 1
    writeLines('[{"duration":100},{"duration":200}]', args[r_idx])
    list(status = 0L, stdout = "Success", stderr = "")
  }
  
  mock_resolve <- function(...) "/fake/motis"
  
  testthat::with_mocked_bindings(
    run = mock_run,
    .package = "processx",
    code = {
      testthat::with_mocked_bindings(
        resolve_motis_cmd = mock_resolve,
        .package = "rmotis",
        code = {
          res <- motis_one_to_many(
            one, many,
            data_dir = ".",
            engine = "batch",
            spatial_filter_km = NULL,
            echo = FALSE
          )
          expect_equal(nrow(res), 2)
          expect_setequal(res$to_id, c("Near", "Tokyo"))
        }
      )
    }
  )
})
