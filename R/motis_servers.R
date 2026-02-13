#' List Running MOTIS Servers
#' @param include_all Logical. If TRUE, includes servers started by other R sessions (external).
#' @return A data.frame of running servers.
#' @export
motis_servers <- function(include_all = FALSE) {
  reg <- .motis_state$registry
  
  build_df <- function(list_data, external = FALSE) {
    if (length(list_data) == 0) {
      return(data.frame(
        id = character(0), pid = integer(0), port = integer(0), 
        work_dir = character(0), alive = logical(0), external = logical(0),
        stringsAsFactors = FALSE
      ))
    }
    do.call(rbind, lapply(list_data, function(x) {
      # For external processes, we can only check PID
      alive <- if (!is.null(x$proc)) x$proc$is_alive() else .motis_pid_is_running(x$pid)
      data.frame(
        id = x$id, pid = x$pid, port = x$port, 
        work_dir = x$work_dir, alive = alive, external = external,
        stringsAsFactors = FALSE
      )
    }))
  }
  
  df <- build_df(reg, external = FALSE)
  
  if (include_all) {
    others <- .motis_registry_scan_others()
    if (length(others) > 0) {
      df_others <- build_df(others, external = TRUE)
      df <- rbind(df, df_others)
    }
  }
  
  df
}
