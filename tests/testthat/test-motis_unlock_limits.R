library(testthat)

test_that("motis_unlock_limits sets onetomany_max_many to 30000", {
  tmp_dir <- tempfile("motis_unlock_limits_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  config_path <- file.path(tmp_dir, "config.yml")
  yaml::write_yaml(list(limits = list(onetomany_max_many = 128L)), config_path)

  out <- motis_unlock_limits(config_path, force = TRUE)
  expect_equal(normalizePath(out, mustWork = TRUE), normalizePath(config_path, mustWork = TRUE))

  config <- yaml::read_yaml(config_path)
  expect_equal(config$limits$onetomany_max_many, 30000L)
})
