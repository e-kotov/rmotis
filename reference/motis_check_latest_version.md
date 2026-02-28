# Find the Latest Stable MOTIS Version

Queries the GitHub API to find the most recent stable (non-pre-release)
version tag for the MOTIS backend that has at least one binary
available.

## Usage

``` r
motis_check_latest_version()
```

## Value

A string containing the latest version tag (e.g., `"v0.1.0"`).

## Examples

``` r
if (FALSE) { # \dontrun{
latest_version <- motis_check_latest_version()
print(latest_version)
# Use it to install the latest version
# motis_install(version = latest_version)
} # }
```
