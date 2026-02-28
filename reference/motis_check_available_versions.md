# Check for Available MOTIS Versions

Queries the GitHub API to find all MOTIS releases and reports which ones
have pre-compiled binaries for major platforms.

## Usage

``` r
motis_check_available_versions(prereleases = FALSE)
```

## Arguments

- prereleases:

  A logical value. If `TRUE`, include pre-release versions in the
  returned table. Defaults to `FALSE`.

## Value

A `tibble` (or `data.frame`) with the following columns:

- tag_name:

  The version tag string.

- published_at:

  The publication timestamp of the release.

- prerelease:

  A logical indicating if it is a pre-release.

- linux_amd64:

  Logical, `TRUE` if a binary is available for this platform.

- macos_arm64:

  Logical, `TRUE` if a binary is available for this platform.

- windows_amd64:

  Logical, `TRUE` if a binary is available for this platform.

## Details

This function scans all releases in the `motis-project/motis` GitHub
repository. For each release, it checks for asset files matching common
platform binaries (e.g., `motis-linux-amd64.tar.bz2`). The result is a
table showing the availability of these binaries for each version tag.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get a table of all stable versions with available binaries
available_versions <- motis_check_available_versions()
print(available_versions)
} # }
```
