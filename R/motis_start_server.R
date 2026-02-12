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
  server_process <- processx::process$new(
    command = cmd,
    args = c("server", paste0("--data=", data_dir)),
    wd = work_dir,
    stdout = "|",
    stderr = "|"
  )

  Sys.sleep(2) # Give it a moment to initialize
  if (!server_process$is_alive()) {
    stop(
      "MOTIS server failed to start. Check logs:
",
      "STDOUT: ",
      paste(server_process$read_output_lines(), collapse = "
"),
      "
",
      "STDERR: ",
      paste(server_process$read_error_lines(), collapse = "
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
    motis_dir = work_dir
  )
}

#' @export
print.motis_server <- function(x, ...) {
  cat(sprintf("<MOTIS Server> ID: %s | Port: %d | PID: %d | Alive: %s
", 
              attr(x, "motis_id"), attr(x, "motis_port"), x$get_pid(), x$is_alive()))
  invisible(x)
}
