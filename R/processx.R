#' Prepare MOTIS Data Directory
#'
#' Runs the `motis config` and `motis import` commands to prepare a data
#' directory for the MOTIS server. This involves generating a configuration
#' file and preprocessing the OSM and GTFS data into a graph format.
#'
#' @details
#' This function requires that the `motis` executable is available on the system
#' `PATH`. You can install it using `motis_install()`. The function will first
#' create a `config.yml` file based on the provided data inputs and then run
#' the import process, which can be time-consuming depending on the size of
#' the datasets. The resulting graph data is stored in a `data` subdirectory
#' within the `work_dir`.
#'
#' @param work_dir A string specifying the directory where MOTIS commands will be
#'   run and where the `data` and `config.yml` files will be stored. Defaults
#'   to the current working directory.
#' @param osm_pbf A string. The path to the input OpenStreetMap `.osm.pbf` file.
#' @param gtfs_files A character vector of paths to the input GTFS `.zip` files.
#' @param echo_cmd A logical. If `TRUE`, prints the full command being run to the
#'   console. Defaults to `FALSE`.
#' @param echo A logical. If `TRUE`, streams the stdout and stderr from the MOTIS
#'   process to the console. Defaults to `TRUE`.
#' @param spinner A logical. If `TRUE`, shows a spinner in the console while
#'   the process is running. Defaults to `TRUE`.
#' @return A list with elements:
#'   \item{work_dir}{The normalized path to the working directory.}
#'   \item{logs}{A list containing the `processx::run` result objects for the
#'   `config` and `import` steps.}
#' @export
#' @examples
#' \dontrun{
#' # Assuming 'aachen.osm.pbf' and 'gtfs.zip' are in the current directory
#' prep_result <- motis_prepare_data(
#'   work_dir = tempdir(),
#'   osm_pbf = "aachen.osm.pbf",
#'   gtfs_files = "gtfs.zip"
#' )
#' # This directory can now be used with motis_start_server()
#' server <- motis_start_server(work_dir = prep_result$work_dir)
#' motis_stop_server(server)
#' }
motis_prepare_data <- function(
  work_dir = getwd(),
  osm_pbf,
  gtfs_files,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  # --- 1. Checks and Validation ---
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required.", call. = FALSE)
  }
  if (Sys.which("motis") == "") {
    stop(
      "'motis' executable not found on PATH. Please run `motis_install()`.",
      call. = FALSE
    )
  }
  if (!dir.exists(work_dir)) {
    dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  }
  work_dir <- normalizePath(work_dir)
  osm_pbf <- normalizePath(osm_pbf, mustWork = TRUE)
  gtfs_files <- normalizePath(gtfs_files, mustWork = TRUE)

  # --- 2. Run 'motis config' ---
  message("--- Step 1: Generating config.yml ---")
  config_args <- c("config", osm_pbf, gtfs_files)
  config_logs <- processx::run(
    "motis",
    args = config_args,
    wd = work_dir,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )

  # --- 3. Run 'motis import' ---
  message("\n--- Step 2: Importing data (this may take a while) ---")
  import_logs <- processx::run(
    "motis",
    args = "import",
    wd = work_dir,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )

  # --- 4. Final verification ---
  data_dir <- file.path(work_dir, "data")
  if (!dir.exists(data_dir)) {
    stop(
      "MOTIS import failed to create the 'data' directory in: ",
      work_dir,
      call. = FALSE
    )
  }

  message("\n✅ Data preparation successful in: ", work_dir)
  invisible(list(
    work_dir = work_dir,
    logs = list(config = config_logs, import = import_logs)
  ))
}

#' Start a MOTIS Server Process
#'
#' Launches the `motis server` command as a background process.
#'
#' @details
#' This function starts the MOTIS HTTP server, which will listen for API
#' requests. It requires a prepared data directory (created by
#' `motis_prepare_data()`). The function returns a `processx::process` object,
#' which is essential for managing the server's lifecycle (e.g., checking if
#' it's running, or stopping it with `motis_stop_server()`).
#'
#' @param work_dir A string. The path to the MOTIS working directory, which must
#'   contain the `data` subdirectory with preprocessed graph files.
#' @param port An integer. The TCP port for the server to listen on. Defaults
#'   to `8080`.
#' @param host A string. The IP address to bind the server to. Defaults to
#'   `"127.0.0.1"` (localhost). Use `"0.0.0.0"` to make it accessible on the
#'   network.
#' @param echo_cmd A logical. If `TRUE`, prints the full command being run to the
#'   console. Defaults to `FALSE`.
#' @return A `processx::process` object for the running server.
#' @export
#' @importFrom processx process
#' @examples
#' \dontrun{
#' # First, prepare the data
#' data_dir <- motis_prepare_data(...)$work_dir
#'
#' # Start the server
#' server <- motis_start_server(work_dir = data_dir, port = 8081)
#'
#' # Check if it's running
#' server$is_alive()
#'
#' # Open the UI in a browser
#' motis_open_ui(server)
#'
#' # Stop the server when done
#' motis_stop_server(server)
#' }
motis_start_server <- function(
  work_dir,
  port = 8080L,
  host = "127.0.0.1",
  echo_cmd = FALSE
) {
  # --- 1. Checks and Validation ---
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required.", call. = FALSE)
  }
  if (Sys.which("motis") == "") {
    stop(
      "'motis' executable not found on PATH. Please run `motis_install()`.",
      call. = FALSE
    )
  }
  work_dir <- normalizePath(work_dir, mustWork = TRUE)
  if (!dir.exists(file.path(work_dir, "data"))) {
    stop(
      "`data` directory not found in '",
      work_dir,
      "'. Please run `motis_prepare_data()` first.",
      call. = FALSE
    )
  }

  # --- 2. Build arguments and start process ---
  args <- c("server", "--host", host, "--port", as.character(port))

  if (isTRUE(echo_cmd)) {
    message("Running command in '", work_dir, "':")
    message("motis ", paste(shQuote(args), collapse = " "))
  }

  server_process <- processx::process$new(
    "motis",
    args = args,
    wd = work_dir,
    stdout = "|",
    stderr = "|"
  )

  # --- 3. Wait for startup and return ---
  Sys.sleep(2) # Give it a moment to initialize
  if (!server_process$is_alive()) {
    stop(
      "MOTIS server failed to start. Check logs:\n",
      "STDOUT: ",
      server_process$read_output_lines(),
      "\n",
      "STDERR: ",
      server_process$read_error_lines(),
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
#' Terminates a MOTIS server process that was launched by `motis_start_server()`.
#'
#' @param server A `processx::process` object returned by `motis_start_server()`.
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

#' Open MOTIS Web UI
#'
#' Opens the MOTIS web interface for a running server in the default browser.
#'
#' @param server An optional `processx::process` object from `motis_start_server()`.
#'   If provided, host and port are inferred from the server's command line arguments.
#' @param host A string. The server's host address. Defaults to `"127.0.0.1"`.
#' @param port An integer. The server's port. Defaults to `8080`.
#' @return Invisibly returns the URL that was opened.
#' @export
#' @importFrom utils browseURL
motis_open_ui <- function(server = NULL, host = "127.0.0.1", port = 8080L) {
  if (inherits(server, "processx_process")) {
    cmd_args <- server$get_cmdline()
    port_idx <- which(cmd_args == "--port")
    host_idx <- which(cmd_args == "--host")
    if (length(port_idx) > 0) {
      port <- as.integer(cmd_args[port_idx + 1])
    }
    if (length(host_idx) > 0) host <- cmd_args[host_idx + 1]
  }

  url <- paste0("http://", host, ":", port)
  message("Opening MOTIS UI at: ", url)
  tryCatch(
    utils::browseURL(url),
    error = function(e) {
      warning("Could not open browser automatically.", call. = FALSE)
    }
  )
  invisible(url)
}


#' Diagnose MOTIS Setup
#'
#' Performs a comprehensive health check of the MOTIS environment.
#'
#' @details
#' This function checks for and reports on several key aspects of a MOTIS setup:
#' \itemize{
#'   \item **Executable**: Checks if the `motis` executable is on the `PATH` and
#'     retrieves its version.
#'   \item **System**: Reports the current OS and architecture.
#'   \item **Server Status**: Pings a given host and port to see if a MOTIS
#'     server is already running.
#'   \item **Graph Files**: If a `work_dir` is provided, it checks for the
#'     presence of essential, preprocessed graph files in the `data` subdirectory.
#' }
#'
#' @param port An integer. The port to check for a running MOTIS server.
#' @param host A string. The host address to check.
#' @param work_dir An optional string. Path to a MOTIS working directory to
#'   check for graph files.
#' @return Invisibly returns `NULL`. The function's purpose is to print
#'   diagnostic information to the console.
#' @export
#' @importFrom httr2 request req_timeout req_perform resp_status
motis_diagnose <- function(port = 8080L, host = "127.0.0.1", work_dir = NULL) {
  message("--- MOTIS Diagnostic Check ---")

  # --- 1. Executable Check ---
  message("\n[1] MOTIS Executable:")
  motis_path <- Sys.which("motis")
  if (nzchar(motis_path)) {
    message("  ✔ Found at: ", motis_path)
    version_res <- tryCatch(
      processx::run("motis", "--version", error_on_status = FALSE),
      error = function(e) NULL
    )
    if (!is.null(version_res) && version_res$status == 0) {
      message("  ✔ Version: ", trimws(version_res$stdout))
    } else {
      message(
        "  ✖ Could not execute `motis --version`. Executable might be corrupted or have missing dependencies."
      )
    }
  } else {
    message("  ✖ Not found on PATH. Please run `motis_install()`.")
  }

  # --- 2. System Info ---
  message("\n[2] System Information:")
  platform <- get_platform_info()
  message("  ✔ Platform: ", platform$os, "-", platform$arch)

  # --- 3. Server Status Check ---
  message("\n[3] Server Status:")
  url <- paste0("http://", host, ":", port)
  server_running <- FALSE
  tryCatch(
    {
      req <- httr2::request(url) |> httr2::req_timeout(2)
      resp <- httr2::req_perform(req)
      if (httr2::resp_status(resp) == 200) server_running <- TRUE
    },
    error = function(e) {
      # This is intentional: Do nothing if the request fails.
      # The server_running variable remains FALSE.
    }
  )

  if (server_running) {
    message("  ✔ A server appears to be running at: ", url)
  } else {
    message("  ℹ No server detected at: ", url)
  }

  # --- 4. Graph File Check ---
  message("\n[4] Graph Files:")
  if (is.null(work_dir)) {
    message("  ℹ No `work_dir` provided, skipping check.")
  } else {
    work_dir_norm <- normalizePath(work_dir, mustWork = FALSE)
    data_dir <- file.path(work_dir_norm, "data")
    config_file <- file.path(work_dir_norm, "config.yml")
    message("  ℹ Checking for data in: ", data_dir)

    if (!dir.exists(data_dir)) {
      message("  ✖ 'data' directory not found.")
    } else {
      expected_data_files <- c(
        "timetable_metrics.json",
        "matches.bin",
        "shape_offsets_data.bin",
        "shape_offsets_idx.bin",
        "shape_route_bboxes.bin",
        "shape_route_segment_bboxes_data.bin",
        "shape_route_segment_bboxes_idx.bin",
        "shape_trip_offsets.bin",
        "shapes_data.bin",
        "shapes_idx.bin",
        "tags.bin",
        "tt.bin"
      )

      found_files <- file.exists(file.path(data_dir, expected_data_files))
      n_found <- sum(found_files)
      n_total <- length(expected_data_files)

      if (n_found == n_total) {
        message(
          "  ✔ Found all ",
          n_total,
          " expected files in 'data' directory."
        )
      } else {
        message(
          "  ✖ Found ",
          n_found,
          " of ",
          n_total,
          " expected files in 'data' directory."
        )
        missing_files <- expected_data_files[!found_files]
        message("    Missing: ", paste(missing_files, collapse = ", "))
      }
    }

    if (file.exists(config_file)) {
      message("  ✔ Found config.yml in working directory.")
    } else {
      message("  ✖ config.yml not found in working directory.")
    }
  }
  message("\n--- End of Diagnostic ---")
  invisible(NULL)
}
