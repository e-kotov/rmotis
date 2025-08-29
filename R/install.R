#' Install MOTIS Backend Binaries
#'
#' Downloads and installs pre-compiled binaries for the MOTIS backend from the
#' official GitHub releases.
#'
#' @details
#' This function automates the download and setup of MOTIS. It performs the
#' following steps:
#' 1.  Queries the GitHub API to find the specified MOTIS release.
#' 2.  Identifies the correct binary archive for the user's OS and architecture.
#' 3.  Downloads and extracts the archive.
#' 4.  Copies the MOTIS executable and its supporting files to the specified
#'     installation directory.
#' 5.  Optionally modifies the `PATH` environment variable for the current
#'     session and/or the current project's `.Rprofile`.
#'
#' Please provide either the `location` OR the `path` argument, but not both.
#'
#' @param version A string specifying the MOTIS version tag to install.
#'   Defaults to `"latest"`, which automatically finds the most recent stable
#'   version by calling `motis_check_latest_version()`.
#' @param location A string specifying the type of installation location.
#'   One of:
#'   \itemize{
#'     \item `"cache"` (default): Installs to a persistent, user-level cache
#'       directory recommended by `tools::R_user_dir("rmotis", "cache")`.
#'     \item `"project"`: Installs to a `./bin/motis/{platform-arch}` subdirectory
#'       within the current project.
#'   }
#' @param path An optional string specifying a custom directory path for the
#'   installation. If provided, `location` must not be set.
#' @param force A logical value. If `TRUE`, reinstall MOTIS even if it's already
#'   found in the target directory. Defaults to `FALSE`.
#' @param path_action A string specifying how to handle the system `PATH`. One of:
#'   \itemize{
#'     \item `"session"`: Adds the MOTIS directory to `PATH` for the current R
#'       session only.
#'     \item `"project"`: Modifies the `.Rprofile` in the current project to
#'       set the `PATH` for future sessions in that project.
#'     \item `"both"` (default): Performs both the `"session"` and `"project"` actions.
#'     \item `"none"`: Does not modify the `PATH`.
#'   }
#' @param quiet A logical value. If `TRUE`, suppresses messages and confirmation
#'   prompts. Defaults to `FALSE`.
#' @return The path to the installation directory, invisibly.
#' @export
#' @examples
#' \dontrun{
#' # Install to project and set PATH for both session and .Rprofile
#' motis_install(location = "project", path_action = "both")
#'
#' # Install to cache and set PATH for session only
#' motis_install(location = "cache", path_action = "session")
#'
#' # Clean up the project's .Rprofile and session PATH
#' motis_clear_path()
#' }
motis_install <- function(
  version = "latest",
  location = "cache",
  path = NULL,
  force = FALSE,
  path_action = c("both", "session", "project", "none"),
  quiet = FALSE
) {
  # --- 1. Argument validation and dependency checks ---
  call <- match.call()
  if ("path" %in% names(call) && "location" %in% names(call)) {
    stop(
      "Please specify either 'location' or 'path', but not both.",
      call. = FALSE
    )
  }
  if (!is.null(path) && !is.character(path)) {
    stop("'path' must be a character string.", call. = FALSE)
  }
  if (is.null(path)) {
    location <- match.arg(location, c("cache", "project"))
  }

  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required.", call. = FALSE)
  }
  path_action <- match.arg(path_action)
  platform <- get_platform_info()

  # --- 2. Determine destination directory ---
  project_path <- getwd()
  if (!is.null(path)) {
    dest_dir <- path
  } else if (location == "project") {
    platform_key <- paste(platform$os, platform$arch, sep = "-")
    dest_dir <- file.path(project_path, "bin", "motis", platform_key)
  } else {
    # location == "cache"
    dest_dir <- tools::R_user_dir("rmotis", which = "cache")
  }

  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  dest_dir <- normalizePath(dest_dir)

  # --- 3. Check for existing installation ---
  motis_exe <- file.path(
    dest_dir,
    if (.Platform$OS.type == "windows") "motis.exe" else "motis"
  )
  if (file.exists(motis_exe) && !force) {
    if (!quiet) {
      message("MOTIS executable already found in: ", dest_dir)
      message("Use force = TRUE to reinstall.")
    }
    handle_path_setting(path_action, dest_dir, project_path, quiet)
    return(invisible(dest_dir))
  }

  # --- 4. Determine version and find asset URL ---
  if (version == "latest") {
    if (!quiet) {
      message("Finding latest stable version with available binaries...")
    }
    version <- motis_check_latest_version()
    if (!quiet) message("Latest stable version is '", version, "'")
  }
  release_info <- get_release_by_tag(version)
  assets <- find_release_assets(release_info)
  platform_id <- paste(platform$os, platform$arch, sep = "_")
  asset_url <- assets[[platform_id]]

  if (is.null(asset_url)) {
    stop(
      "Could not find a compatible binary for your platform (",
      platform_id,
      ") in release '",
      version,
      "'.",
      call. = FALSE
    )
  }
  if (!quiet) {
    message("Found matching binary: ", basename(asset_url))
  }

  # --- 5. Download and extract ---
  if (!quiet) {
    message("Downloading from ", asset_url)
  }
  tmp_file <- tempfile(fileext = basename(asset_url))
  on.exit(unlink(tmp_file), add = TRUE)

  tryCatch(
    {
      req <- httr2::request(asset_url)
      httr2::req_perform(req, path = tmp_file)
    },
    error = function(e) {
      stop("Failed to download file: ", e$message, call. = FALSE)
    }
  )

  tmp_extract_dir <- tempfile()
  dir.create(tmp_extract_dir)
  on.exit(unlink(tmp_extract_dir, recursive = TRUE), add = TRUE)
  if (!quiet) {
    message("Extracting files...")
  }

  if (grepl("\\.zip$", tmp_file)) {
    utils::unzip(tmp_file, exdir = tmp_extract_dir)
  } else {
    utils::untar(
      tmp_file,
      exdir = tmp_extract_dir,
      extras = "--strip-components=1"
    )
  }

  # --- 6. Copy files to destination ---
  files_to_copy <- list.files(
    tmp_extract_dir,
    full.names = TRUE,
    all.files = TRUE,
    no.. = TRUE
  )
  if (!quiet) {
    message("Installing MOTIS to ", dest_dir)
  }
  file.copy(
    from = files_to_copy,
    to = dest_dir,
    overwrite = TRUE,
    recursive = TRUE
  )

  # --- 7. Set permissions and update PATH ---
  if (.Platform$OS.type != "windows") {
    if (!quiet) {
      message("Setting executable permissions...")
    }
    Sys.chmod(motis_exe, mode = "0755")
  }

  handle_path_setting(path_action, dest_dir, project_path, quiet)

  if (!quiet) {
    message("\n✅ MOTIS installation successful!")
  }
  return(invisible(dest_dir))
}


#' Uninstall MOTIS Backend Binaries
#'
#' Removes the MOTIS backend binaries from a specified installation directory.
#'
#' @param location A string specifying the type of installation location to
#'   uninstall from. One of: `"cache"` (default) or `"project"`.
#' @param path An optional string specifying a custom directory path to uninstall
#'   from. If provided, `location` must not be set.
#' @param quiet A logical value. If `TRUE`, suppresses confirmation prompts.
#'   Defaults to `FALSE`.
#' @return Invisibly returns `TRUE` if the directory was successfully removed,
#'   `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Uninstall MOTIS from the default cache location
#' motis_uninstall()
#'
#' # Uninstall from a project-specific directory
#' motis_uninstall(location = "project")
#' }
motis_uninstall <- function(location = "cache", path = NULL, quiet = FALSE) {
  # --- 1. Argument validation ---
  call <- match.call()
  if ("path" %in% names(call) && "location" %in% names(call)) {
    stop(
      "Please specify either 'location' or 'path', but not both.",
      call. = FALSE
    )
  }
  if (!is.null(path) && !is.character(path)) {
    stop("'path' must be a character string.", call. = FALSE)
  }
  if (is.null(path)) {
    location <- match.arg(location, c("cache", "project"))
  }

  platform <- get_platform_info()

  # --- 2. Determine directory to remove ---
  if (!is.null(path)) {
    dir_to_remove <- path
  } else if (location == "project") {
    platform_key <- paste(platform$os, platform$arch, sep = "-")
    dir_to_remove <- file.path(getwd(), "bin", "motis", platform_key)
  } else {
    # location == "cache"
    dir_to_remove <- tools::R_user_dir("rmotis", which = "cache")
  }
  dir_to_remove <- normalizePath(dir_to_remove, mustWork = FALSE)

  # --- 3. Check existence and ask for confirmation ---
  if (!dir.exists(dir_to_remove)) {
    message("MOTIS installation not found at: ", dir_to_remove)
    return(invisible(FALSE))
  }

  message("This will permanently remove the MOTIS installation from:")
  message(dir_to_remove)

  proceed <- FALSE
  if (interactive() && !quiet) {
    ans <- utils::askYesNo("Do you want to proceed?", default = FALSE)
    if (isTRUE(ans)) proceed <- TRUE
  } else {
    proceed <- TRUE # Assume yes in non-interactive sessions
  }

  # --- 4. Perform deletion ---
  if (proceed) {
    message("Removing directory...")
    unlink(dir_to_remove, recursive = TRUE, force = TRUE)
    if (!dir.exists(dir_to_remove)) {
      message("✅ Successfully uninstalled MOTIS binaries.")
      return(invisible(TRUE))
    } else {
      warning("Failed to remove directory: ", dir_to_remove, call. = FALSE)
      return(invisible(FALSE))
    }
  } else {
    message("Uninstall aborted by user.")
    return(invisible(FALSE))
  }
}
