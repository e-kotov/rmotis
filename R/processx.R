#' Read a MOTIS Config File Safely
#'
#' Reads a config.yml file while preventing coercion errors for very large
#' numbers (e.g., `db_size`) by pre-processing the text to quote the value.
#'
#' @param config_path Path to the config.yml file.
#' @return A list representing the parsed YAML content.
#' @noRd
#' @importFrom yaml read_yaml
read_motis_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path, call. = FALSE)
  }
  # Read as text to avoid direct parsing errors with large numbers
  config_text <- readLines(config_path, warn = FALSE)

  # Safely quote the db_size value if it's a very large number to prevent
  # coercion to NA by the yaml parser.
  # The regex looks for 'db_size:' followed by a number of 10+ digits.
  config_text <- gsub("^( *db_size:\\s*)(\\d{10,})$", "\\1'\\2'", config_text)

  yaml::read_yaml(text = paste(config_text, collapse = "\n"))
}

#' Add MOTIS Assets to a Working Directory
#'
#' Copies or creates symbolic links for required MOTIS asset directories (e.g.,
#' `tiles-profiles`, `ui`) from a MOTIS installation into a specified working
#' directory. This is a necessary step before running `motis_import`.
#'
#' @param work_dir A string. The path to the target directory where the assets
#'   will be placed.
#' @param assets_action A string specifying how to handle the assets. One of:
#'   \itemize{
#'     \item `"copy"` (default): Copies assets to the `work_dir`. Safe and works everywhere.
#'     \item `"symlink"`: Creates a symbolic link to the assets. Saves disk space,
#'       best for non-Windows systems. Will fall back to copying if it fails.
#'     \item `"none"`: Does nothing. For users who manage assets manually.
#'   }
#' @param motis_path An optional string. The path to the *directory* containing
#'   the `motis` executable. If `NULL`, the executable is assumed to be on the
#'   system `PATH`.
#' @return The path to the working directory, invisibly.
#' @export
motis_add_assets <- function(
  work_dir,
  assets_action = c("copy", "symlink", "none"),
  motis_path = NULL
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  assets_action <- match.arg(assets_action)
  cmd <- resolve_motis_cmd(motis_path)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)
  motis_install_dir <- dirname(cmd)

  if (assets_action != "none") {
    message("--- Handling MOTIS assets ---")
    assets_to_manage <- c("tiles-profiles", "ui")
    for (asset in assets_to_manage) {
      src_path <- file.path(motis_install_dir, asset)
      dest_path <- file.path(work_dir, asset)

      if (!dir.exists(src_path)) {
        message(
          "  ℹ Asset directory not found in MOTIS installation, skipping: ",
          asset
        )
        next
      }

      if (dir.exists(dest_path) || file.exists(dest_path)) {
        unlink(dest_path, recursive = TRUE, force = TRUE)
      }

      if (assets_action == "copy") {
        message("  -> Copying '", asset, "' to working directory.")
        file.copy(src_path, work_dir, recursive = TRUE)
      } else {
        # assets_action == "symlink"
        message("  -> Symlinking '", asset, "' to working directory.")
        tryCatch(
          {
            file.symlink(src_path, dest_path)
          },
          warning = function(w) {
            message(
              "  ! Symlink failed (this is common on Windows without admin rights). Falling back to copying."
            )
            file.copy(src_path, work_dir, recursive = TRUE)
          },
          error = function(e) {
            message(
              "  ! Symlink failed with an error. Falling back to copying."
            )
            file.copy(src_path, work_dir, recursive = TRUE)
          }
        )
      }
    }
  }
  invisible(work_dir)
}

#' Set Server Address in a MOTIS Configuration File
#'
#' Safely reads a `config.yml` file, adds or updates the `server` settings
#' for `host` and `port`, and writes the file back.
#'
#' @details
#' Because this function modifies a file in place, it requires confirmation if
#' run in an interactive session. To override this and allow modification without
#' a prompt (e.g., in scripts), use `force = TRUE`.
#'
#' @param config_path A string. The path to the `config.yml` file to modify.
#' @param host A string. The IP address for the server to bind to.
#' @param port An integer between 1 and 65535. The TCP port for the server.
#' @param force A logical. If `FALSE` (default), the function will ask for
#'   confirmation before modifying the file in an interactive session. If `TRUE`,
#'   the file is always modified.
#' @return The path to the modified `config.yml` file, invisibly.
#' @export
#' @importFrom yaml write_yaml
#' @importFrom utils askYesNo
motis_set_server_address <- function(
  config_path,
  host = "127.0.0.1",
  port = 8080L,
  force = FALSE
) {
  # --- 1. Argument Validation ---
  if (
    !is.numeric(port) ||
      length(port) != 1 ||
      port %% 1 != 0 ||
      port <= 0 ||
      port > 65535
  ) {
    stop(
      "'port' must be a single, whole number between 1 and 65535.",
      call. = FALSE
    )
  }
  port <- as.integer(port) # Ensure it's a true integer for YAML writing

  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("'yaml' package is required.", call. = FALSE)
  }
  config_path <- normalizePath(config_path, mustWork = TRUE)

  # --- 2. User Confirmation ---
  if (interactive() && !force) {
    ans <- utils::askYesNo(
      paste0(
        "This will modify the existing file '",
        basename(config_path),
        "'. Proceed?"
      ),
      default = FALSE
    )
    if (!isTRUE(ans)) {
      message("Modification aborted by user.")
      return(invisible(NULL))
    }
  }

  # --- 3. Read, Modify, Write ---
  config_list <- read_motis_config(config_path)

  if (is.null(config_list$server)) {
    config_list$server <- list()
  }

  config_list$server$host <- host
  config_list$server$port <- port

  yaml::write_yaml(config_list, config_path)
  message("  -> Updated '", basename(config_path), "' with server host/port.")
  invisible(config_path)
}


#' Generate and Customize a MOTIS Configuration File
#'
#' Generates a `config.yml` file and sets the server host and port.
#'
#' @details
#' This function first runs `motis_add_assets`, then generates a base `config.yml`,
#' and finally updates the configuration file to include the specified HTTP
#' server settings (`host` and `port`).
#'
#' If a `config.yml` file already exists in the `work_dir`, this function will
#' stop with an error to prevent accidental overwriting. To proceed and
#' overwrite the existing file, specify `force = TRUE`.
#'
#' @inheritParams motis_add_assets
#' @param osm_pbf An optional string. The path to a single OpenStreetMap `.osm.pbf` file.
#' @param gtfs_files An optional character vector of paths to GTFS `.zip` files.
#' @param host A string. The IP address for the server to bind to.
#' @param port An integer. The TCP port for the server.
#' @param force A logical. If `TRUE`, an existing `config.yml` file in the
#'   `work_dir` will be overwritten. Defaults to `FALSE`.
#' @param echo_cmd A logical. If `TRUE`, prints the full command. Defaults to `FALSE`.
#' @param echo A logical. If `TRUE`, streams process output. Defaults to `TRUE`.
#' @param spinner A logical. If `TRUE`, shows a console spinner. Defaults to `TRUE`.
#' @return The path to the generated `config.yml` file, invisibly.
#' @export
motis_config <- function(
  work_dir,
  osm_pbf = NULL,
  gtfs_files = NULL,
  assets_action = c("copy", "symlink", "none"),
  motis_path = NULL,
  host = "127.0.0.1",
  port = 8080L,
  force = FALSE,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' is required.", call. = FALSE)
  }

  assets_action <- match.arg(assets_action)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)

  # Check for existing config before doing anything
  config_file <- file.path(work_dir, "config.yml")
  if (file.exists(config_file) && !force) {
    stop(
      "A 'config.yml' already exists in the working directory.\n",
      "Use `force = TRUE` to overwrite it.",
      call. = FALSE
    )
  }

  motis_add_assets(work_dir, assets_action, motis_path)

  message("--- Generating config.yml ---")
  cmd <- resolve_motis_cmd(motis_path)

  if (is.null(osm_pbf)) {
    pbf_found <- list.files(
      work_dir,
      pattern = "\\.osm\\.pbf$",
      full.names = TRUE
    )
    if (length(pbf_found) > 1) {
      stop(
        "Found multiple .osm.pbf files. Please specify one via 'osm_pbf'.",
        call. = FALSE
      )
    } else if (length(pbf_found) == 1) {
      osm_pbf <- pbf_found
    }
  } else {
    if (length(osm_pbf) > 1) {
      stop("Only one .osm.pbf file can be provided.", call. = FALSE)
    }
  }
  if (is.null(gtfs_files)) {
    gtfs_files <- list.files(work_dir, pattern = "\\.zip$", full.names = TRUE)
  }
  input_paths <- c(osm_pbf, gtfs_files)
  if (length(input_paths) == 0) {
    warning("No input files found or provided.", call. = FALSE)
  }

  processx::run(
    command = cmd,
    args = c("config", normalizePath(input_paths, mustWork = TRUE)),
    wd = work_dir,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )

  if (!file.exists(config_file)) {
    stop("MOTIS failed to generate 'config.yml'.", call. = FALSE)
  }

  # Set host and port, forcing the modification since the user already
  # authorized the overwrite at this function's level.
  motis_set_server_address(config_file, host, port, force = TRUE)

  message("✅ `config.yml` generated and configured successfully.")
  invisible(config_file)
}

#' Import and Preprocess MOTIS Data
#'
#' @description Runs `motis import` after a `config.yml` has been created.
#' @inheritParams motis_add_assets
#' @param echo_cmd A logical. If `TRUE`, prints the full command. Defaults to `FALSE`.
#' @param echo A logical. If `TRUE`, streams process output. Defaults to `TRUE`.
#' @param spinner A logical. If `TRUE`, shows a console spinner. Defaults to `TRUE`.
#' @return The path to the created `data` directory, invisibly.
#' @export
motis_import <- function(
  work_dir,
  motis_path = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  if (!requireNamespace("processx", quietly = TRUE)) {
    stop("'processx' package is required.", call. = FALSE)
  }
  cmd <- resolve_motis_cmd(motis_path)
  work_dir <- normalizePath(work_dir, mustWork = TRUE)
  if (!file.exists(file.path(work_dir, "config.yml"))) {
    stop("'config.yml' not found.", call. = FALSE)
  }
  message("--- Importing data (this may take a while) ---")
  processx::run(
    command = cmd,
    args = "import",
    wd = work_dir,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )
  data_dir <- file.path(work_dir, "data")
  if (!dir.exists(data_dir)) {
    stop("MOTIS import failed to create the 'data' directory.", call. = FALSE)
  }
  message("✅ Data imported successfully in: ", data_dir)
  invisible(data_dir)
}

#' Prepare MOTIS Data Directory (Config + Import)
#'
#' @description A wrapper that runs `motis_config` and `motis_import`.
#' @inheritParams motis_config
#' @return The normalized path to the working directory, invisibly.
#' @export
motis_prepare_data <- function(
  work_dir,
  osm_pbf = NULL,
  gtfs_files = NULL,
  assets_action = c("copy", "symlink", "none"),
  motis_path = NULL,
  host = "127.0.0.1",
  port = 8080L,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
) {
  if (missing(work_dir)) {
    stop("'work_dir' must be specified.", call. = FALSE)
  }
  assets_action <- match.arg(assets_action)

  motis_config(
    work_dir = work_dir,
    osm_pbf = osm_pbf,
    gtfs_files = gtfs_files,
    assets_action = assets_action,
    motis_path = motis_path,
    host = host,
    port = port,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )
  motis_import(
    work_dir = work_dir,
    motis_path = motis_path,
    echo_cmd = echo_cmd,
    echo = echo,
    spinner = spinner
  )
  invisible(normalizePath(work_dir))
}
