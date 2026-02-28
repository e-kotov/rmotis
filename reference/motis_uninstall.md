# Uninstall MOTIS Backend Binaries

Removes the MOTIS backend binaries from a specified installation
directory.

## Usage

``` r
motis_uninstall(location = "cache", path = NULL, quiet = FALSE)
```

## Arguments

- location:

  A string specifying the type of installation location to uninstall
  from. One of: `"cache"` (default) or `"project"`.

- path:

  An optional string specifying a custom directory path to uninstall
  from. If provided, `location` must not be set.

- quiet:

  A logical value. If `TRUE`, suppresses confirmation prompts. Defaults
  to `FALSE`.

## Value

Invisibly returns `TRUE` if the directory was successfully removed,
`FALSE` otherwise.

## Examples

``` r
if (FALSE) { # \dontrun{
# Uninstall MOTIS from the default cache location
motis_uninstall()

# Uninstall from a project-specific directory
motis_uninstall(location = "project")
} # }
```
