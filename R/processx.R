#' Generate a MOTIS Configuration File
#'
#' Runs the `motis config` command to generate a `config.yml` file in the
#' specified working directory.
#'
#' @details
#' This function creates a configuration file based on the provided data inputs.
#' If `osm_pbf` and `gtfs_files` are not provided, it will automatically scan
#' the `work_dir` for any `.osm.pbf` and `.zip` files to include. The generated
#' `config.yml` can be manually edited before running `motis_import()`.
#'
#' @param work_dir A string. The path to the directory where the `config.yml`
#'   will be created and where input files are scanned if not specified.
#' @param osm_pbf An optional string. The path to the OpenStreetMap `.osm.pbf` file.
#' @param gtfs_files An optional character vector of paths to GTFS `.zip` files.
#' @param motis_path An optional string. The path to the *directory* containing
#'   the `motis` executable. If `NULL`, the executable is assumed to be on the
#'   system `PATH`.
#' @param echo_cmd A logical. If `TRUE`, prints the full command. Defaults to `FALSE`.
#' @param echo A logical. If `TRUE`, streams process output. Defaults to `TRUE`.
#' @param spinner A logical. If `TRUE`, shows a console spinner. Defaults to `TRUE`.
#' @return The path to the generated `config.yml` file, invisibly.
#' @export
motis_config <- function(
  work_dir,
  osm_pbf = NULL,
  gtfs_files = NULL,
  motis_path = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required.", call. = FALSE)
  }

  cmd <- resolve_motis_cmd(motis_path)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)

  # Auto-discover files if not provided
  input_paths <- c()
  if (is.null(osm_pbf) && is.null(gtfs_files)) {
    message("No input files specified, scanning '", work_dir, "'...")
    pbf_found <- list.files(
      work_dir,
      pattern = "\\.osm\\.pbf$",
      full.names = TRUE
    )
    zip_found <- list.files(work_dir, pattern = "\\.zip$", full.names = TRUE)
    input_paths <- c(pbf_found, zip_found)
  } else {
    input_paths <- c(osm_pbf, gtfs_files)
  }

  if (length(input_paths) == 0) {
    warning(
      "No input files found or provided. Generating an empty config.",
      call. = FALSE
    )
  }

  config_args <- c("config", normalizePath(input_paths, mustWork = TRUE))
  processx::run(
    command = cmd,
    args = config_args,
    wd = work_dir,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )

  config_file <- file.path(work_dir, "config.yml")
  if (!file.exists(config_file)) {
    stop("MOTIS failed to generate 'config.yml' in: ", work_dir, call. = FALSE)
  }

  message("✅ `config.yml` generated successfully in: ", work_dir)
  invisible(config_file)
}

#' Import and Preprocess MOTIS Data
#'
#' Runs the `motis import` command to preprocess OSM and GTFS data based on a
#' `config.yml` file.
#'
#' @details
#' This function executes the main data preprocessing step, which can be
#' time-consuming. It reads the `config.yml` in the `work_dir` and creates a
#' `data` subdirectory containing the processed graph files.
#'
#' @inheritParams motis_config
#' @return The path to the created `data` directory, invisibly.
#' @export
motis_import <- function(
  work_dir,
  motis_path = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required.", call. = FALSE)
  }

  cmd <- resolve_motis_cmd(motis_path)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)

  if (!file.exists(file.path(work_dir, "config.yml"))) {
    stop(
      "'config.yml' not found in '",
      work_dir,
      "'. Please run `motis_config()` first.",
      call. = FALSE
    )
  }

  message("--- Importing data (this may take a while) ---")
  processx::run(
    command = cmd,
    args = "import",
    wd = work_dir,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )

  data_dir <- file.path(work_dir, "data")
  if (!dir.exists(data_dir)) {
    stop(
      "MOTIS import failed to create the 'data' directory in: ",
      work_dir,
      call. = FALSE
    )
  }

  message("✅ Data imported successfully in: ", data_dir)
  invisible(data_dir)
}

#' Prepare MOTIS Data Directory (Config + Import)
#'
#' A wrapper function that runs `motis_config` and then `motis_import` to
#' fully prepare a data directory for the MOTIS server.
#'
#' @inheritParams motis_config
#' @return A list with elements:
#'   \item{work_dir}{The normalized path to the working directory.}
#'   \item{logs}{A list of `processx::run` results for the `config` and `import` steps.}
#' @export
motis_prepare_data <- function(
  work_dir,
  osm_pbf = NULL,
  gtfs_files = NULL,
  motis_path = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }

  message("--- Step 1: Generating config.yml ---")
  motis_config(
    work_dir,
    osm_pbf,
    gtfs_files,
    motis_path,
    echo_cmd,
    echo,
    spinner
  )

  message("\n--- Step 2: Importing data ---")
  motis_import(work_dir, motis_path, echo_cmd, echo, spinner)

  invisible(normalizePath(work_dir))
}

#' Start a MOTIS Server Process
#'
#' Launches the `motis server` command as a background process.
#'
#' @inheritParams motis_config
#' @param port An integer. The TCP port for the server. Defaults to `8080`.
#' @param host A string. The IP address to bind to. Defaults to `"127.0.0.1"`.
#' @return A `processx::process` object for the running server.
#' @export
motis_start_server <- function(
  work_dir,
  port = 8080L,
  host = "127.0.0.1",
  motis_path = NULL,
  echo_cmd = FALSE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required.", call. = FALSE)
  }

  cmd <- resolve_motis_cmd(motis_path)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)
  if (!dir.exists(file.path(work_dir, "data"))) {
    stop(
      "`data` directory not found in '",
      work_dir,
      "'. Please run `motis_import()` first.",
      call. = FALSE
    )
  }

  args <- c("server", "--host", host, "--port", as.character(port))

  if (isTRUE(echo_cmd)) {
    message("Running command in '", work_dir, "':")
    message(cmd, " ", paste(shQuote(args), collapse = " "))
  }

  server_process <- processx::process$new(
    command = cmd,
    args = args,
    wd = work_dir,
    stdout = "|",
    stderr = "|"
  )

  Sys.sleep(2) # Give it a moment to initialize
  if (!server_process$is_alive()) {
    stop(
      "MOTIS server failed to start. Check logs:\n",
      "STDOUT: ",
      paste(server_process$read_output_lines(), collapse = "\n"),
      "\n",
      "STDERR: ",
      paste(server_process$read_error_lines(), collapse = "\n"),
      call. = FALSE
    )
  }

  message(
    "✅ MOTIS server started on http://",
    host,
    ":",
    port,
    " (PID: ",
    server_process$get_pid(),
    ")"
  )
  return(server_process)
}

#' Stop a MOTIS Server Process
#'
#' Terminates a MOTIS server process launched by `motis_start_server()`.
#'
#' @param server A `processx::process` object from `motis_start_server()`.
#' @return Invisibly returns the (now stopped) `processx::process` object.
#' @export
motis_stop_server <- function(server) {
  if (!inherits(server, "processx_process")) {
    stop(
      "'server' must be a processx::process object from `motis_start_server()`.",
      call. = FALSE
    )
  }
  if (server$is_alive()) {
    pid <- server$get_pid()
    server$kill()
    message("✅ MOTIS server (PID: ", pid, ") has been stopped.")
  } else {
    message("ℹ MOTIS server was not running.")
  }
  invisible(server)
}
