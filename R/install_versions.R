#' Check for Available MOTIS Versions
#'
#' Queries the GitHub API to find all MOTIS releases and reports which ones
#' have pre-compiled binaries for major platforms.
#'
#' @details
#' This function scans all releases in the `motis-project/motis` GitHub
#' repository. For each release, it checks for asset files matching common
#' platform binaries (e.g., `motis-linux-amd64.tar.bz2`). The result is a
#' table showing the availability of these binaries for each version tag.
#'
#' @param prereleases A logical value. If `TRUE`, include pre-release versions
#'   in the returned table. Defaults to `FALSE`.
#' @return A `tibble` (or `data.frame`) with the following columns:
#'   \item{tag_name}{The version tag string.}
#'   \item{published_at}{The publication timestamp of the release.}
#'   \item{prerelease}{A logical indicating if it is a pre-release.}
#'   \item{linux_amd64}{Logical, `TRUE` if a binary is available for this platform.}
#'   \item{macos_arm64}{Logical, `TRUE` if a binary is available for this platform.}
#'   \item{windows_amd64}{Logical, `TRUE` if a binary is available for this platform.}
#' @export
#' @examples
#' \dontrun{
#' # Get a table of all stable versions with available binaries
#' available_versions <- motis_check_available_versions()
#' print(available_versions)
#' }
motis_check_available_versions <- function(prereleases = FALSE) {
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required. Please install it.", call. = FALSE)
  }

  releases <- get_all_releases()
  if (!prereleases) {
    releases <- Filter(function(rel) !isTRUE(rel$prerelease), releases)
  }

  all_platforms <- c("linux_amd64", "macos_arm64", "windows_amd64")

  version_info <- lapply(releases, function(rel) {
    assets <- find_release_assets(rel)
    availability <- setNames(
      lapply(all_platforms, function(p) p %in% names(assets)),
      all_platforms
    )
    tibble::tibble(
      tag_name = rel$tag_name,
      published_at = rel$published_at,
      prerelease = rel$prerelease,
      !!!availability
    )
  })

  do.call(rbind, version_info)
}

#' Find the Latest Stable MOTIS Version
#'
#' Queries the GitHub API to find the most recent stable (non-pre-release)
#' version tag for the MOTIS backend that has at least one binary available.
#'
#' @return A string containing the latest version tag (e.g., `"v0.1.0"`).
#' @export
#' @examples
#' \dontrun{
#' latest_version <- motis_check_latest_version()
#' print(latest_version)
#' # Use it to install the latest version
#' # motis_install(version = latest_version)
#' }
motis_check_latest_version <- function() {
  releases <- get_all_releases()
  for (rel in releases) {
    assets <- find_release_assets(rel)
    if (!isTRUE(rel$prerelease) && length(assets) > 0) {
      return(rel$tag_name)
    }
  }
  stop(
    "Could not find any stable releases with available binaries.",
    call. = FALSE
  )
}
