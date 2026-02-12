#' Start a MOTIS Server Process
#'
#' Launches the `motis server` command as a background process.
#'
#' @inheritParams motis_add_assets
#' @param port Integer. The port to bind to. If NULL, tries to read from config.yml, defaults to 8080.
#' @param echo_cmd A logical. If `TRUE`, prints the full command. Defaults to `FALSE`.
#' @return A `motis_server` object (inherits from `processx::process`).
#' @export
motis_start_server <- function(
  work_dir,
  motis_path = NULL,
  port = NULL,
  echo_cmd = FALSE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' is required.", call. = FALSE)
  }

  cmd <- resolve_motis_cmd(motis_path)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)
  data_dir <- file.path(work_dir, "data")
  config_in_data <- file.path(data_dir, "config.yml")

  if (!dir.exists(data_dir) || !file.exists(config_in_data)) {
    stop(
      "`data/config.yml` not found. Please run `motis_import()` first.",
      call. = FALSE
    )
  }

  # 1. Resolve Port and Update Config if needed
  conf <- tryCatch(read_motis_config(config_in_data), error = function(e) list())
  current_host <- conf$server$host %||% "127.0.0.1"
  current_port <- conf$server$port %||% 8080L

  if (!is.null(port)) {
    port <- as.integer(port)
    if (port != current_port) {
      motis_set_server_address(config_in_data, host = current_host, port = port, force = TRUE)
    }
  } else {
    port <- current_port
  }

  # 2. Check Conflicts
  running <- motis_servers(include_all = TRUE)
  if (any(running$port == port & running$alive)) {
    stop(sprintf("Port %d is already in use by another MOTIS server.", port), call. = FALSE)
  }

  if (isTRUE(echo_cmd)) {
    message("Running command in '", work_dir, "':")
    message(cmd, " server --data ", shQuote(data_dir))
  }

  # 3. Launch
  # We update the config file instead of using CLI args because MOTIS does not 
  # support --server.port overrides in all versions.
  
  # Use temp files for logging to prevent pipe deadlocks and keep logs available for debug
  log_stdout <- tempfile(pattern = "motis_out_", fileext = ".log")
  log_stderr <- tempfile(pattern = "motis_err_", fileext = ".log")
  
  server_process <- processx::process$new(
    command = cmd,
    args = c("server", paste0("--data=", data_dir)),
    wd = work_dir,
    stdout = log_stdout,
    stderr = log_stderr
  )

  # Grace period check
  start_time <- Sys.time()
  is_alive <- TRUE
  while(difftime(Sys.time(), start_time, units = "secs") < 2) {
      if (!server_process$is_alive()) {
          is_alive <- FALSE
          break
      }
      Sys.sleep(0.2)
  }

  if (!is_alive) {
    # Read logs from files
    out_log <- if(file.exists(log_stdout)) readLines(log_stdout, warn = FALSE) else "(empty)"
    err_log <- if(file.exists(log_stderr)) readLines(log_stderr, warn = FALSE) else "(empty)"
    
    stop(
      "MOTIS server failed to start. Check logs:
",
      "STDOUT:
",
      paste(tail(out_log, 20), collapse = "
"),
      "
",
      "STDERR:
",
      paste(tail(err_log, 20), collapse = "
"),
      call. = FALSE
    )
  }

  # 4. Register
  id <- .motis_register(server_process, port, work_dir)

  message(
    "✅ MOTIS server started on http://127.0.0.1:",
    port,
    " (PID: ",
    server_process$get_pid(),
    ")"
  )

  # 5. Return object with class for S3 methods
  structure(
    server_process, 
    class = c("motis_server", "process", "R6"),
    motis_id = id,
    motis_port = port,
    motis_dir = work_dir,
    log_stdout = log_stdout,
    log_stderr = log_stderr
  )
}

#' @export
print.motis_server <- function(x, ...) {
  cat(sprintf("<MOTIS Server> ID: %s | Port: %d | PID: %d | Alive: %s
", 
              attr(x, "motis_id"), attr(x, "motis_port"), x$get_pid(), x$is_alive()))
  if (!x$is_alive()) {
      cat("Status: STOPPED / CRASHED\n")
  }
  invisible(x)
}
