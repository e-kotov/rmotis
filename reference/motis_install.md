# Install MOTIS Backend Binaries

Downloads and installs pre-compiled binaries for the MOTIS backend from
the official GitHub releases, or installs from a local archive file.

## Usage

``` r
motis_install(
  version = "latest",
  file = NULL,
  location = "cache",
  path = NULL,
  force = FALSE,
  path_action = c("both", "session", "project", "none"),
  quiet = FALSE
)
```

## Arguments

- version:

  A string specifying the MOTIS version tag to install. Defaults to
  `"latest"`, which automatically finds the most recent stable version
  by calling
  [`motis_check_latest_version()`](http://www.ekotov.pro/rmotis/reference/motis_check_latest_version.md).
  Ignored if `file` is provided.

- file:

  An optional path to a local MOTIS archive file (`.zip`, `.tar.bz2`,
  etc.) to install from, instead of downloading from GitHub. This is
  useful for installing binaries downloaded manually from GitHub
  releases or GitHub Actions artifacts.

- location:

  A string specifying the type of installation location. One of:

  - `"cache"` (default): Installs to a persistent, user-level cache
    directory recommended by `tools::R_user_dir("rmotis", "cache")`.

  - `"project"`: Installs to a `./bin/motis/{platform-arch}`
    subdirectory within the current project.

- path:

  An optional string specifying a custom directory path for the
  installation. If provided, `location` must not be set.

- force:

  A logical value. If `TRUE`, reinstall MOTIS even if it's already found
  in the target directory. Defaults to `FALSE`.

- path_action:

  A string specifying how to handle the system `PATH`. One of:

  - `"session"`: Adds the MOTIS directory to `PATH` for the current R
    session only.

  - `"project"`: Modifies the `.Rprofile` in the current project to set
    the `PATH` for future sessions in that project.

  - `"both"` (default): Performs both the `"session"` and `"project"`
    actions.

  - `"none"`: Does not modify the `PATH`.

- quiet:

  A logical value. If `TRUE`, suppresses messages and confirmation
  prompts. Defaults to `FALSE`.

## Value

The path to the installation directory, invisibly.

## Details

This function automates the download and setup of MOTIS. It performs the
following steps:

1.  Queries the GitHub API to find the specified MOTIS release (skipped
    if `file` is provided).

2.  Identifies the correct binary archive for the user's OS and
    architecture.

3.  Downloads and extracts the archive.

4.  Copies the MOTIS executable and its supporting files to the
    specified installation directory.

5.  Optionally modifies the `PATH` environment variable for the current
    session and/or the current project's `.Rprofile`.

Please provide either the `location` OR the `path` argument, but not
both.

## Examples

``` r
if (FALSE) { # \dontrun{
# Install to project and set PATH for both session and .Rprofile
motis_install(location = "project", path_action = "both")

# Install to cache and set PATH for session only
motis_install(location = "cache", path_action = "session")

# Install from a local archive file
motis_install(file = "path/to/motis-macos-arm64.zip")

# Clean up the project's .Rprofile and session PATH
motis_clear_path()
} # }
```
