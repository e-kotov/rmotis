#' Clear MOTIS Path from .Rprofile and Restore Session PATH
#'
#' Removes MOTIS-specific configuration from the project's `.Rprofile` and
#' restores the `PATH` environment variable for the current R session to its
#' state before `motis_install()` was called.
#'
#' @details
#' This function performs two actions:
#' 1.  It restores the `PATH` for the current R session using a backup stored in
#'     `options("rmotis.original_path")`. If the backup is not found, a message
#'     is printed.
#' 2.  It reads the `.Rprofile` file in the current working directory, removes
#'     any lines containing the special `#-added-by-r-pkg-rmotis` comment, and
#'     writes the cleaned content back to the file.
#'
#' @param project_path The path to the project directory containing the
#'   `.Rprofile` file. Defaults to the current working directory (`getwd()`).
#' @param quiet A logical value. If `TRUE`, suppresses messages. Defaults to `FALSE`.
#' @return Invisibly returns `TRUE` if the `.Rprofile` was modified, `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Assuming motis_install(path_action = "project") was run previously
#'
#' # Clean up the project's .Rprofile and restore the session PATH
#' motis_clear_path()
#' }
motis_clear_path <- function(project_path = getwd(), quiet = FALSE) {
  # 1. Restore session PATH
  original_path <- getOption("rmotis.original_path")
  if (!is.null(original_path)) {
    Sys.setenv(PATH = original_path)
    options(rmotis.original_path = NULL)
    if (!quiet) message("Restored PATH for the current R session.")
  } else {
    if (!quiet) message("No original session PATH backup found in options().")
  }

  # 2. Modify .Rprofile
  r_profile_path <- file.path(project_path, ".Rprofile")

  if (!file.exists(r_profile_path)) {
    if (!quiet) {
      message("No .Rprofile file found in '", project_path, "'.")
    }
    return(invisible(FALSE))
  }

  lines <- readLines(r_profile_path, warn = FALSE)
  comment_tag <- "#-added-by-r-pkg-rmotis"

  lines_to_keep <- lines[!grepl(comment_tag, lines, fixed = TRUE)]

  if (length(lines_to_keep) == length(lines)) {
    if (!quiet) {
      message(
        ".Rprofile does not contain any paths set by rmotis. No changes made."
      )
    }
    return(invisible(FALSE))
  }

  if (!quiet) {
    message("Removing rmotis PATH configuration from: ", r_profile_path)
  }
  writeLines(lines_to_keep, r_profile_path)
  if (!quiet) {
    message(
      "Successfully removed configuration. Please restart R for changes to take effect."
    )
  }

  return(invisible(TRUE))
}


#' Handle PATH Environment Variable Setting
#'
#' Internal wrapper to direct path setting to session or project scope.
#'
#' @param action A string, one of "session", "project", "both", or "none".
#' @param dir The directory to add to the PATH.
#' @param project_path The project directory for `action = "project"`.
#' @param quiet A logical value to suppress messages.
#' @return NULL (invisibly).
#' @noRd
handle_path_setting <- function(action, dir, project_path, quiet) {
  if (action %in% c("session", "both")) {
    set_path_session(dir, quiet)
  }
  if (action %in% c("project", "both")) {
    set_path_project(dir, project_path, quiet)
  }
  invisible(NULL)
}

#' Set PATH for the Current R Session
#'
#' Prepends a directory to the PATH environment variable for the current session
#' and backs up the original PATH.
#'
#' @param dir The directory to prepend to the PATH.
#' @param quiet A logical value to suppress messages.
#' @return NULL (invisibly).
#' @noRd
set_path_session <- function(dir, quiet = FALSE) {
  path_sep <- .Platform$path.sep
  current_path <- Sys.getenv("PATH")

  # Backup original PATH if not already backed up
  if (is.null(getOption("rmotis.original_path"))) {
    options(rmotis.original_path = current_path)
  }

  path_elements <- strsplit(current_path, path_sep)[[1]]
  normalized_dir <- normalizePath(dir, mustWork = FALSE)
  normalized_paths <- normalizePath(path_elements, mustWork = FALSE)

  if (!normalized_dir %in% normalized_paths) {
    new_path <- paste(normalized_dir, current_path, sep = path_sep)
    Sys.setenv(PATH = new_path)
    if (!quiet) {
      message(
        "\u2714 PATH set for current session: '",
        normalized_dir,
        "'\n",
        "  (Undo by restarting R or running `motis_clear_path()`)"
      )
    }
  }
}

#' Set PATH in a Project's .Rprofile
#'
#' Adds or appends a line to the .Rprofile file in a specified project
#' directory to set the PATH at startup.
#'
#' @param dir The directory to add to the PATH.
#' @param project_path The root directory of the project.
#' @param quiet A logical value to suppress messages and prompts.
#' @return NULL (invisibly).
#' @noRd
#' @importFrom utils askYesNo
set_path_project <- function(dir, project_path, quiet = FALSE) {
  r_profile_path <- file.path(project_path, ".Rprofile")

  if (!quiet) {
    message("! Modifying project startup file: ", r_profile_path)
  }

  if (interactive() && !quiet) {
    ans <- utils::askYesNo("Do you want to proceed?", default = TRUE)
    if (!isTRUE(ans)) {
      message("Aborted. .Rprofile was not modified.")
      return(invisible(NULL))
    }
  }

  comment_tag <- "#-added-by-r-pkg-rmotis"
  line_to_add <- sprintf(
    'Sys.setenv(PATH = paste("%s", Sys.getenv("PATH"), sep = "%s")) %s',
    normalizePath(dir),
    .Platform$path.sep,
    comment_tag
  )

  if (!file.exists(r_profile_path)) {
    if (!quiet) {
      message("  Creating new .Rprofile file.")
    }
    writeLines(line_to_add, r_profile_path)
  } else {
    lines <- readLines(r_profile_path, warn = FALSE)
    if (any(grepl(comment_tag, lines, fixed = TRUE))) {
      if (!quiet) {
        message(
          "  .Rprofile already contains rmotis PATH configuration. No changes made."
        )
      }
      return(invisible(NULL))
    }
    if (!quiet) {
      message("  Appending configuration to existing .Rprofile.")
    }
    if (length(lines) > 0 && nzchar(lines[length(lines)])) {
      lines <- c(lines, "")
    }
    lines <- c(lines, line_to_add)
    writeLines(lines, r_profile_path)
  }
  if (!quiet) {
    message(
      "\u2714 .Rprofile modified. PATH will be set on project startup.\n",
      "  (Undo by running `motis_clear_path()`)"
    )
  }
  invisible(NULL)
}
