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
      "session-%s-%s", Sys.getpid(), format(Sys.time(), "%Y%m%d%H%M%OS3")
    )
  }
  file.path(.motis_registry_dir(), paste0(.motis_state$session_id, ".json"))
}

# Internal JSON serializer (since RcppSimdJson is parser-only)
.to_json_registry <- function(reg_list) {
  # Remove non-serializable 'proc' object before saving
  clean_list <- lapply(reg_list, function(x) {
    x$proc <- NULL
    x
  })
  
  # Manual serialization for simple flat list structure
  json_entries <- vapply(clean_list, function(x) {
    fields <- vapply(names(x), function(nm) {
      val <- x[[nm]]
      if (is.null(val) || (length(val) == 1 && is.na(val))) return(sprintf('"%s": null', nm))
      if (is.character(val)) return(sprintf('"%s": "%s"', nm, val)) # Basic string
      if (is.numeric(val)) return(sprintf('"%s": %s', nm, val))
      if (is.logical(val)) return(sprintf('"%s": %s', nm, ifelse(val, "true", "false")))
      "" 
    }, character(1))
    sprintf("{%s}", paste(fields, collapse = ", "))
  }, character(1))
  
  sprintf("[%s]", paste(json_entries, collapse = ", "))
}

.motis_registry_save <- function() {
  json_str <- .to_json_registry(.motis_state$registry)
  writeLines(json_str, .motis_registry_path())
}

.motis_registry_load <- function() {
  path <- .motis_registry_path()
  if (file.exists(path)) {
    # Use RcppSimdJson for fast reading
    data <- try(RcppSimdJson::fparse(path), silent = TRUE)
    if (!inherits(data, "try-error") && is.list(data)) {
      # RcppSimdJson returns list of lists, index by ID
      for(item in data) {
        if (!is.null(item$id)) .motis_state$registry[[item$id]] <- item
      }
    }
  }
}

.motis_pid_is_running <- function(pid) {
  if (is.null(pid) || is.na(pid)) return(FALSE)
  tryCatch(ps::ps_is_running(ps::ps_handle(as.integer(pid))), error = function(e) FALSE)
}

.motis_register <- function(proc, port, work_dir) {
  pid <- proc$get_pid()
  id <- sprintf("motis-%d-%d", pid, port)
  
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
  if (exists(id, envir = as.environment(.motis_state$registry))) {
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
    # Use RcppSimdJson
    data <- try(RcppSimdJson::fparse(f), silent = TRUE)
    if (!inherits(data, "try-error") && length(data) > 0) {
      # Clean up dead session files
      owner_pid <- as.integer(sub("^session-([0-9]+)-.*", "\\1", basename(f)))
      if (!.motis_pid_is_running(owner_pid)) {
        # defer cleanup to explicit user action or smart GC
        all_external <- c(all_external, data)
      } else {
        all_external <- c(all_external, data)
      }
    }
  }
  all_external
}

.onLoad <- function(libname, pkgname) {
  .motis_registry_load()
}

.onUnload <- function(libpath) {
  # Clean up registry file
  if (!is.null(.motis_state$session_id)) {
    unlink(.motis_registry_path())
  }
}
