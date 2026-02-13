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
#' @inheritParams motis_config
#' @param port An integer. The port to check for a running MOTIS server.
#' @param host A string. The host address to check.
#' @return Invisibly returns `NULL`. Prints diagnostic info to the console.
#' @export
#' @importFrom httr2 request req_timeout req_perform resp_status
motis_diagnose <- function(
  port = 8080L,
  host = "127.0.0.1",
  work_dir = NULL,
  motis_path = NULL
) {
  message("--- MOTIS Diagnostic Check ---")

  # --- 1. Executable Check ---
  message("\n[1] MOTIS Executable:")
  motis_cmd <- try(resolve_motis_cmd(motis_path), silent = TRUE)
  if (!inherits(motis_cmd, "try-error")) {
    message("  \u2714 Found at: ", motis_cmd)
    version_res <- tryCatch(
      processx::run(motis_cmd, "--version", error_on_status = FALSE),
      error = function(e) NULL
    )
    if (!is.null(version_res) && version_res$status == 0) {
      message("  \u2714 Version: ", trimws(version_res$stdout))
    } else {
      message(
        "  \u2716 Could not execute `motis --version`. Executable might be corrupted."
      )
    }
  } else {
    message("  \u2716 Not found. ", gsub("Error : |\n", "", motis_cmd))
  }

  # --- 2. System Info ---
  message("\n[2] System Information:")
  platform <- get_platform_info()
  message("  \u2714 Platform: ", platform$os, "-", platform$arch)

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
      # Intentionally do nothing on error
    }
  )

  if (server_running) {
    message("  \u2714 A server appears to be running at: ", url)
  } else {
    message("  \u2139 No server detected at: ", url)
  }

  # --- 4. Graph File Check ---
  message("\n[4] Graph Files:")
  if (is.null(work_dir)) {
    message("  \u2139 No `work_dir` provided, skipping check.")
  } else {
    work_dir_norm <- normalizePath(work_dir, mustWork = FALSE)
    data_dir <- file.path(work_dir_norm, "data")
    config_file <- file.path(work_dir_norm, "config.yml")
    message("  \u2139 Checking in: ", work_dir_norm)

    if (file.exists(config_file)) {
      message("  \u2714 Found config.yml.")
    } else {
      message("  \u2716 config.yml not found.")
    }

    if (!dir.exists(data_dir)) {
      message("  \u2716 'data' directory not found.")
    } else {
      message("  \u2714 'data' directory found.")
    }
  }
  message("\n--- End of Diagnostic ---")
  invisible(NULL)
}
