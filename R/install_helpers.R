#' Get All Releases from the MOTIS GitHub Repository
#'
#' Fetches a list of all releases from the 'motis-project/motis' repository
#' using the GitHub API.
#'
#' @return A list representing the JSON response of all releases.
#' @noRd
#' @importFrom httr2 request req_error resp_body_json req_perform
get_all_releases <- function() {
  repo <- "motis-project/motis"
  url <- paste0("https://api.github.com/repos/", repo, "/releases")

  req <- httr2::request(url) |>
    httr2::req_error(body = function(resp) httr2::resp_body_json(resp)$message)

  resp <- tryCatch(
    {
      httr2::req_perform(req)
    },
    error = function(e) {
      stop("GitHub API request failed: ", e$message, call. = FALSE)
    }
  )

  httr2::resp_body_json(resp)
}

#' Get a Specific Release by Tag Name
#'
#' Fetches information for a single release from the 'motis-project/motis'
#' repository based on its git tag.
#'
#' @param version A string with the version tag (e.g., "v0.1.0").
#' @return A list representing the JSON response for the specified release.
#' @noRd
#' @importFrom httr2 request req_error resp_body_json req_perform
get_release_by_tag <- function(version) {
  repo <- "motis-project/motis"
  url <- paste0(
    "https://api.github.com/repos/",
    repo,
    "/releases/tags/",
    version
  )

  req <- httr2::request(url) |>
    httr2::req_error(body = function(resp) httr2::resp_body_json(resp)$message)

  resp <- tryCatch(
    {
      httr2::req_perform(req)
    },
    error = function(e) {
      stop(
        "GitHub API request failed for tag '",
        version,
        "': ",
        e$message,
        call. = FALSE
      )
    }
  )

  httr2::resp_body_json(resp)
}

#' Parse a MOTIS Asset Filename
#'
#' Extracts platform information (OS and architecture) from a MOTIS release
#' asset filename.
#'
#' @param filename The asset filename string (e.g., "motis-linux-amd64.tar.bz2").
#' @return A list containing `os` and `arch` if matched, otherwise `NULL`.
#' @noRd
parse_motis_asset_name <- function(filename) {
  # Pattern for linux/macos: motis-{os}-{arch}.tar.bz2
  pattern_nix <- "motis-(linux|macos)-(amd64|arm64)\\.tar\\.bz2"
  match_nix <- regexec(pattern_nix, filename)

  if (match_nix[[1]][1] != -1) {
    parts <- regmatches(filename, match_nix)[[1]]
    return(list(os = parts[2], arch = parts[3]))
  }

  # Pattern for windows: motis-windows.zip (assume x64/amd64)
  pattern_win <- "motis-windows\\.zip"
  match_win <- regexec(pattern_win, filename)

  if (match_win[[1]][1] != -1) {
    return(list(os = "windows", arch = "amd64"))
  }

  return(NULL)
}

#' Find and Analyze Assets in a Release
#'
#' Scans the assets of a given release and identifies the available MOTIS
#' binaries for different platforms.
#'
#' @param release A list object representing a single GitHub release.
#' @return A list where names are platform identifiers (e.g., "linux_amd64") and
#'   values are the download URLs. Returns an empty list if no matching assets
#'   are found.
#' @noRd
find_release_assets <- function(release) {
  asset_info <- list()
  if (length(release$assets) == 0) {
    return(asset_info)
  }

  for (asset in release$assets) {
    platform_info <- parse_motis_asset_name(asset$name)
    if (!is.null(platform_info)) {
      platform_key <- paste(platform_info$os, platform_info$arch, sep = "_")
      asset_info[[platform_key]] <- asset$browser_download_url
    }
  }
  return(asset_info)
}

#' Detect Current Platform Information
#'
#' Determines the operating system and architecture of the current machine in a
#' format that matches the MOTIS binary naming scheme.
#'
#' @return A list with two elements: `os` (e.g., "linux", "macos", "windows") and
#'   `arch` (e.g., "amd64", "arm64").
#' @noRd
get_platform_info <- function() {
  os <- switch(
    Sys.info()[["sysname"]],
    Linux = "linux",
    Darwin = "macos",
    Windows = "windows"
  )
  arch <- switch(
    Sys.info()[["machine"]],
    x86_64 = "amd64",
    amd64 = "amd64",
    aarch64 = "arm64",
    arm64 = "arm64"
  )
  if (is.null(os) || is.null(arch)) {
    stop(
      "Your platform is not supported by pre-compiled MOTIS binaries. OS: ",
      Sys.info()[["sysname"]],
      ", Arch: ",
      Sys.info()[["machine"]],
      call. = FALSE
    )
  }
  list(os = os, arch = arch)
}
