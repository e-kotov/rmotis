#' Stop MOTIS Server
#' @param server A motis_server object.
#' @param id The server ID.
#' @param port The server port.
#' @param include_all Logical. If TRUE, allows stopping servers from other R sessions.
#' @export
motis_stop_server <- function(server = NULL, id = NULL, port = NULL, include_all = FALSE) {
  target_id <- id
  target_pid <- NULL
  
  # 1. Resolve ID/PID from arguments
  if (!is.null(server)) {
    if (inherits(server, "motis_server")) {
      target_id <- attr(server, "motis_id")
    } else if (inherits(server, "process")) {
       target_pid <- server$get_pid()
       # Check local registry for this PID
       reg <- .motis_state$registry
       found <- Filter(function(x) !is.null(x$pid) && x$pid == target_pid, reg)
       if (length(found)) target_id <- found[[1]]$id
    }
  } else if (!is.null(port)) {
    # Check local first
    reg <- .motis_state$registry
    found <- Filter(function(x) !is.null(x$port) && x$port == port, reg)
    if (length(found)) {
      target_id <- found[[1]]$id
    } else if (include_all) {
      # Check external
      others <- .motis_registry_scan_others()
      found_ex <- Filter(function(x) !is.null(x$port) && x$port == port, others)
      if (length(found_ex)) target_id <- found_ex[[1]]$id
    }
  }
  
  # 2. Try Local Stop (cleanest)
  if (!is.null(target_id)) {
    if (!is.null(.motis_state$registry[[target_id]])) {
      entry <- .motis_state$registry[[target_id]]
      if (!is.null(entry$proc) && entry$proc$is_alive()) entry$proc$kill()
      .motis_deregister(target_id)
      message("✅ Stopped local server ", target_id)
      return(invisible(TRUE))
    }
  }
  
  # 3. Try External Stop (if ID found externally or PID known)
  if (include_all) {
    # If we have an ID but it wasn't local, look it up in scan results
    if (!is.null(target_id) && is.null(target_pid)) {
      others <- .motis_registry_scan_others()
      found <- Filter(function(x) !is.null(x$id) && x$id == target_id, others)
      if (length(found)) target_pid <- found[[1]]$pid
    }
    
    if (!is.null(target_pid)) {
      if (.motis_pid_is_running(target_pid)) {
        tryCatch({
          ps::ps_kill(ps::ps_handle(as.integer(target_pid)))
          message("✅ Killed external process PID ", target_pid)
          return(invisible(TRUE))
        }, error = function(e) {
          warning("Failed to kill external PID ", target_pid, ": ", e$message)
        })
      } else {
        message("ℹ PID ", target_pid, " is not running.")
      }
    }
  } else if (!is.null(target_id)) {
    message("ℹ Server ", target_id, " not found in local registry. Use include_all=TRUE to check external sessions.")
  }
  
  invisible(FALSE)
}

#' Stop all running MOTIS servers
#' @param include_all Logical. If TRUE, stops servers from other sessions too.
#' @export
motis_stop_all <- function(include_all = FALSE) {
  # Stop locals
  reg <- .motis_state$registry
  ids <- names(reg)
  count <- 0
  for (id in ids) {
    if (motis_stop_server(id = id)) {
      count <- count + 1
    }
  }
  
  # Stop externals
  if (include_all) {
    others <- .motis_registry_scan_others()
    for (entry in others) {
      if (!is.null(entry$pid) && .motis_pid_is_running(entry$pid)) {
        if (motis_stop_server(id = entry$id, include_all = TRUE)) {
          count <- count + 1
        }
      }
    }
  }
  invisible(count)
}
