# R/motis_server_registry.R

.motis_state <- new.env(parent = emptyenv())
.motis_state$registry <- list()
.motis_state$session_id <- NULL

.motis_registry_dir <- function() {
  # Use cache dir for persistence across sessions
  d <- tools::R_user_dir("rmotis", which = "cache")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

.motis_registry_path <- function() {
  if (is.null(.motis_state$session_id)) {
    .motis_state$session_id <- sprintf(
      "session-%s-%s-%s", 
      Sys.getpid(), 
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      paste0(sample(c(0:9, letters), 6, replace = TRUE), collapse = "")
    )
  }
  file.path(.motis_registry_dir(), paste0(.motis_state$session_id, ".json"))
}

# Internal JSON serializer
.to_json_registry <- function(reg_list) {
  # Remove non-serializable 'proc' object before saving
  clean_list <- lapply(reg_list, function(x) {
    x$proc <- NULL
    x
  })
  
  if (length(clean_list) == 0) {
    return("[]")
  }
  
  # Manual serialization
  json_entries <- vapply(clean_list, function(x) {
    fields <- vapply(names(x), function(nm) {
      val <- x[[nm]]
      if (is.null(val) || (length(val) == 1 && is.na(val))) return(sprintf('"%s": null', nm))
      
      if (is.character(val)) {
        # Basic escaping: quotes and backslashes
        val_esc <- gsub("\\", "\\\\", val, fixed = TRUE)
        val_esc <- gsub('"', '\\"', val_esc, fixed = TRUE)
        return(sprintf('"%s": "%s"', nm, val_esc))
      }
      
      if (is.numeric(val)) return(sprintf('"%s": %s', nm, val))
      if (is.logical(val)) return(sprintf('"%s": %s', nm, ifelse(val, "true", "false")))
      "" 
    }, character(1))
    fields <- fields[nzchar(fields)]
    sprintf("{%s}", paste(fields, collapse = ", "))
  }, character(1))
  
  sprintf("[%s]", paste(json_entries, collapse = ", "))
}

.motis_registry_save <- function() {
  json_str <- .to_json_registry(.motis_state$registry)
  writeLines(json_str, .motis_registry_path())
}

.motis_read_registry_file <- function(path) {
  if (!file.exists(path)) return(list())
  
  # Use RcppSimdJson for fast reading
  data <- try(RcppSimdJson::fparse(path), silent = TRUE)
  
  if (inherits(data, "try-error")) {
    # Fallback to jsonlite if available
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      data <- try(jsonlite::read_json(path, simplifyVector = TRUE), silent = TRUE)
      if (inherits(data, "try-error")) return(list())
    } else {
      return(list())
    }
  }
  
  # RcppSimdJson might return a data.frame if all entries match
  if (is.data.frame(data)) {
    # Convert back to list of lists
    data <- split(data, seq(nrow(data)))
    data <- lapply(data, as.list)
  }
  
  if (!is.list(data)) {
      return(list())
  }
  
  # Convert list of lists to named list (by ID) if possible
  if (length(data) > 0) {
    # Check if items have IDs (handle both list-of-lists and simplified output nuances)
    # Using 'x$id' is safe for list elements.
    ids <- tryCatch(vapply(data, function(x) as.character(x$id), character(1)), error = function(e) NULL)
    
    if (!is.null(ids)) {
      names(data) <- ids
    }
  }
  
  data
}

.motis_registry_load <- function() {
  path <- .motis_registry_path()
  reg <- .motis_read_registry_file(path)
  if (length(reg) > 0) {
    .motis_state$registry <- reg
  }
}

.motis_pid_is_running <- function(pid) {
  if (is.null(pid) || is.na(pid)) return(FALSE)
  tryCatch(ps::ps_is_running(ps::ps_handle(as.integer(pid))), error = function(e) FALSE)
}

.motis_register <- function(proc, port, work_dir) {
  pid <- proc$get_pid()
  id <- sprintf("motis-%d-%d-%s", pid, port, format(Sys.time(), "%Y%m%d%H%M%S"))
  
  entry <- list(
    id = id,
    pid = pid,
    port = as.integer(port),
    work_dir = work_dir,
    started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
    proc = proc
  )
  
  .motis_state$registry[[id]] <- entry
  .motis_registry_save()
  id
}

.motis_deregister <- function(id) {
  if (!is.null(.motis_state$registry[[id]])) {
    .motis_state$registry[[id]] <- NULL
    .motis_registry_save()
  }
}

# Scan other session files to detect external conflicts
.motis_registry_scan_others <- function() {
  mydir <- .motis_registry_dir()
  files <- list.files(mydir, pattern = "^session-.*\\.json$", full.names = TRUE)
  files <- setdiff(files, .motis_registry_path())
  
  all_external <- list()
  
  for (f in files) {
    reg <- .motis_read_registry_file(f)
    if (length(reg) > 0) {
      # Check if owner session is still alive
      owner_pid <- as.integer(sub("^session-([0-9]+)-.*", "\\1", basename(f)))
      owner_alive <- !is.na(owner_pid) && .motis_pid_is_running(owner_pid)
      
      alive_reg <- list()
      has_alive_procs <- FALSE
      
      for (entry in reg) {
        if (!is.null(entry$pid) && .motis_pid_is_running(entry$pid)) {
          entry$external <- TRUE
          alive_reg[[entry$id]] <- entry
          has_alive_procs <- TRUE
        }
      }
      
      if (has_alive_procs) {
        all_external <- c(all_external, alive_reg)
        
        # If owner is dead but processes are alive, we could update the file
        # to remove dead entries, but strictly speaking we just want to read.
        # If owner is dead, we *could* take ownership or just leave it.
        # For now, we leave the file as is to avoid race conditions, 
        # but we only report alive processes.
        
        if (!owner_alive && length(alive_reg) < length(reg)) {
            # Optional: Prune dead entries from the abandoned file
             try({
                 clean_list <- lapply(alive_reg, function(x) { x$external <- NULL; x })
                 json_str <- .to_json_registry(clean_list)
                 writeLines(json_str, f)
             }, silent = TRUE)
        }
        
      } else if (!owner_alive) {
        # Owner is dead AND no running processes -> Garbage collect file
        try(unlink(f), silent = TRUE)
      }
    } else {
      # Empty or unreadable file -> GC if owner dead or file old
      try(unlink(f), silent = TRUE)
    }
  }
  all_external
}

.onLoad <- function(libname, pkgname) {
  .motis_state$session_id <- sprintf(
      "session-%s-%s-%s", 
      Sys.getpid(), 
      format(Sys.time(), "%Y%m%d%H%M%OS3"),
      paste0(sample(c(0:9, letters), 6, replace = TRUE), collapse = "")
    )
  .motis_registry_load()
}

.onUnload <- function(libpath) {
  # Clean up registry file if session is ending
  if (!is.null(.motis_state$session_id)) {
    path <- .motis_registry_path()
    if (file.exists(path)) unlink(path)
  }
}
