# --- parse_motis_asset_name() ---

test_that("parse_motis_asset_name handles tar.bz2 for linux/macos", {
  skip_if_not_installed("withr")
  expect_equal(
    parse_motis_asset_name("motis-linux-amd64.tar.bz2"),
    list(os = "linux", arch = "amd64")
  )
  expect_equal(
    parse_motis_asset_name("motis-linux-arm64.tar.bz2"),
    list(os = "linux", arch = "arm64")
  )
  expect_equal(
    parse_motis_asset_name("motis-macos-arm64.tar.bz2"),
    list(os = "macos", arch = "arm64")
  )
  expect_equal(
    parse_motis_asset_name("motis-macos-amd64.tar.bz2"),
    list(os = "macos", arch = "amd64")
  )
})

test_that("parse_motis_asset_name handles zip for all platforms", {
  expect_equal(
    parse_motis_asset_name("motis-linux-amd64.zip"),
    list(os = "linux", arch = "amd64")
  )
  expect_equal(
    parse_motis_asset_name("motis-macos-arm64.zip"),
    list(os = "macos", arch = "arm64")
  )
  expect_equal(
    parse_motis_asset_name("motis-windows-amd64.zip"),
    list(os = "windows", arch = "amd64")
  )
})

test_that("parse_motis_asset_name handles windows without arch", {
  expect_equal(
    parse_motis_asset_name("motis-windows.zip"),
    list(os = "windows", arch = "amd64")
  )
  expect_equal(
    parse_motis_asset_name("motis-windows.tar.bz2"),
    list(os = "windows", arch = "amd64")
  )
})

test_that("parse_motis_asset_name handles all archive extensions", {
  exts <- c(".tar.bz2", ".tar.gz", ".tar.xz", ".tgz", ".zip")
  for (ext in exts) {
    res <- parse_motis_asset_name(paste0("motis-linux-amd64", ext))
    expect_false(is.null(res), info = paste("Failed for extension:", ext))
    expect_equal(res$os, "linux")
    expect_equal(res$arch, "amd64")
  }
})

test_that("parse_motis_asset_name returns NULL for non-matching filenames", {
  expect_null(parse_motis_asset_name("motis-source.tar.gz"))
  expect_null(parse_motis_asset_name("other-linux-amd64.zip"))
  expect_null(parse_motis_asset_name("motis-linux-amd64.exe"))
  expect_null(parse_motis_asset_name("motis-linux-amd64.deb"))
  expect_null(parse_motis_asset_name(""))
  expect_null(parse_motis_asset_name("README.md"))
})

# --- find_release_assets() ---

test_that("find_release_assets parses mock release correctly", {
  mock_release <- list(assets = list(
    list(
      name = "motis-linux-amd64.tar.bz2",
      browser_download_url = "https://example.com/linux-amd64"
    ),
    list(
      name = "motis-macos-arm64.zip",
      browser_download_url = "https://example.com/macos-arm64"
    ),
    list(
      name = "motis-windows.zip",
      browser_download_url = "https://example.com/windows"
    ),
    list(
      name = "source-code.tar.gz",
      browser_download_url = "https://example.com/source"
    )
  ))
  result <- find_release_assets(mock_release)
  expect_equal(result$linux_amd64, "https://example.com/linux-amd64")
  expect_equal(result$macos_arm64, "https://example.com/macos-arm64")
  expect_equal(result$windows_amd64, "https://example.com/windows")
  expect_equal(length(result), 3)
})

test_that("find_release_assets returns empty list for no assets", {
  expect_equal(find_release_assets(list(assets = list())), list())
})

# --- get_platform_info() ---

test_that("get_platform_info returns valid os and arch", {
  result <- get_platform_info()
  expect_true(result$os %in% c("linux", "macos", "windows"))
  expect_true(result$arch %in% c("amd64", "arm64"))
})

# --- motis_install() argument validation ---

test_that("motis_install rejects both path and location", {
  expect_error(
    motis_install(location = "cache", path = "/tmp/test", quiet = TRUE),
    "either 'location' or 'path'"
  )
})

test_that("motis_install rejects non-existent file", {
  expect_error(
    motis_install(file = "/nonexistent/file.zip", quiet = TRUE),
    "existing archive file"
  )
})

test_that("motis_install rejects non-character path", {
  expect_error(
    motis_install(path = 123, quiet = TRUE),
    "'path' must be a character string"
  )
})

# --- motis_install() extraction and verification with mock archive ---

# Helper: create a mock zip containing a single file named "motis"
create_mock_motis_zip <- function(content, executable = TRUE) {
  staging_dir <- tempfile("motis_staging_")
  dir.create(staging_dir)

  fake_motis <- file.path(staging_dir, "motis")
  writeLines(content, fake_motis)
  if (executable) Sys.chmod(fake_motis, "0755")

  mock_zip <- tempfile(fileext = ".zip")
  withr::with_dir(staging_dir, {
    utils::zip(mock_zip, "motis", flags = "-q")
  })

  unlink(staging_dir, recursive = TRUE)
  mock_zip
}

test_that("motis_install extracts zip and verifies binary", {
  skip_on_os("windows")

  mock_zip <- create_mock_motis_zip(c("#!/bin/sh", 'echo "v0.0.0-test"'))
  on.exit(unlink(mock_zip), add = TRUE)

  dest <- tempfile("motis_dest_")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  result <- NULL
  suppressMessages(expect_message(
    result <- motis_install(
      file = mock_zip, path = dest, path_action = "none", force = TRUE
    ),
    "Verified: MOTIS v0.0.0-test"
  ))

  expect_true(file.exists(file.path(dest, "motis")))
  expect_equal(result, normalizePath(dest))
})

test_that("motis_install suppresses verification message when quiet", {
  skip_on_os("windows")

  mock_zip <- create_mock_motis_zip(c("#!/bin/sh", 'echo "v0.0.0-test"'))
  on.exit(unlink(mock_zip), add = TRUE)

  dest <- tempfile("motis_dest_")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  expect_no_message(
    motis_install(
      file = mock_zip, path = dest, path_action = "none",
      force = TRUE, quiet = TRUE
    )
  )
})

test_that("motis_install warns when binary exits with error", {
  skip_on_os("windows")

  mock_zip <- create_mock_motis_zip(c("#!/bin/sh", "exit 1"))
  on.exit(unlink(mock_zip), add = TRUE)

  dest <- tempfile("motis_dest_")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  suppressMessages(expect_warning(
    motis_install(
      file = mock_zip, path = dest, path_action = "none", force = TRUE
    ),
    "could not be executed"
  ))
})

test_that("motis_install warns when binary is not executable", {
  skip_on_os("windows")

  # Create zip with a non-executable file — processx::run() will error,
  # tryCatch returns NULL, triggering the warning
  mock_zip <- create_mock_motis_zip(
    c("not a real binary"), executable = FALSE
  )
  on.exit(unlink(mock_zip), add = TRUE)

  dest <- tempfile("motis_dest_")
  dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE), add = TRUE)

  suppressMessages(expect_warning(
    motis_install(
      file = mock_zip, path = dest, path_action = "none", force = TRUE
    ),
    "could not be executed"
  ))
})
