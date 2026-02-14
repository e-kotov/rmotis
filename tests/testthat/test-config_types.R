library(testthat)
devtools::load_all()

test_that("motis_set_config handles numeric types correctly", {
  # Create a temporary config file
  tmp_config <- tempfile(fileext = ".yml")
  on.exit(unlink(tmp_config))
  
  # Initial empty config
  yaml::write_yaml(list(), tmp_config)
  
  # 1. Test strict mapping fields (within integer range)
  motis_set_config(tmp_config, 
    server = list(
      port = 8080.0, 
      n_threads = 16.0
    ),
    force = TRUE
  )
  
  config_content <- readLines(tmp_config)
  expect_true(any(grepl("port: 8080", config_content)) && !any(grepl("port: 8080\\.0", config_content)))
  expect_true(any(grepl("n_threads: 16", config_content)) && !any(grepl("n_threads: 16\\.0", config_content)))
  
  # 2. Test large number (should not be converted to NA)
  motis_set_config(tmp_config, 
    server = list(
      db_size = 1e12
    ),
    force = TRUE
  )
  config_content <- readLines(tmp_config)
  expect_true(any(grepl("db_size: 1\\.0e\\+12", config_content)))
  
  # 3. Test heuristic for any whole number
  motis_set_config(tmp_config, 
    timetable = list(
      update_interval = 60.0,
      max_matching_distance = 25.0
    ),
    force = TRUE
  )
  
  config_content <- readLines(tmp_config)
  expect_true(any(grepl("update_interval: 60", config_content)) && !any(grepl("update_interval: 60\\.0", config_content)))
  expect_true(any(grepl("max_matching_distance: 25", config_content)) && !any(grepl("max_matching_distance: 25\\.0", config_content)))
  
  # 4. Test preservation of actual doubles
  motis_set_config(tmp_config, 
    timetable = list(
      max_matching_distance = 25.5
    ),
    force = TRUE
  )
  
  config_content <- readLines(tmp_config)
  expect_true(any(grepl("max_matching_distance: 25\\.5", config_content)))
})
