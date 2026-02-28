library(testthat)

test_that(".motis_validate_args blocks invalid local requests", {
  # Mock motis_servers to return a local server with config_path
  local_reg <- data.frame(
    id = "test-server",
    pid = 1234L,
    port = 8080L,
    work_dir = "private/lux-test",
    config_path = "local-testing/test_config.yml",
    alive = TRUE,
    external = FALSE,
    stringsAsFactors = FALSE
  )
  
  withr::local_options(list(rmotis.url = "http://127.0.0.1:8080"))
  
  # Mock read_motis_config (internals can be mocked by overriding in the test env)
  with_mocked_bindings(
    motis_servers = function(...) local_reg,
    read_motis_config = function(...) list(limits = list(
      onetomany_max_many = 128L,
      onetoall_max_travel_minutes = 90L,
      plan_max_results = 50L,
      plan_max_search_window_minutes = 1440L
    )),
    {
      # Case 1: onetomany_max_many exceeded
      expect_error(
        .motis_validate_args(n_many = 200),
        "exceeds the server limit 'onetomany_max_many' \\(128\\)"
      )
      
      # Case 2: onetoall_max_travel_minutes exceeded
      expect_error(
        .motis_validate_args(max_travel_time = 120),
        "exceeds the server limit 'onetoall_max_travel_minutes' \\(90\\)"
      )
      
      # Case 3: Valid request
      expect_silent(.motis_validate_args(n_many = 10, max_travel_time = 60))
    }
  )
})

test_that(".motis_validate_args warns for remote servers", {
  withr::local_options(list(
    rmotis.url = "http://motis.example.org:8080",
    rmotis.warned_urls = NULL
  ))
  
  # Should warn once per session
  expect_warning(
    .motis_validate_args(n_many = 1000),
    "Remote server detected"
  )
  
  # Subsequent calls should be silent
  expect_silent(.motis_validate_args(n_many = 1000))
})
