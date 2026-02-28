# Clear MOTIS Path from .Rprofile and Restore Session PATH

Removes MOTIS-specific configuration from the project's `.Rprofile` and
restores the `PATH` environment variable for the current R session to
its state before
[`motis_install()`](https://www.ekotov.pro/rmotis/reference/motis_install.md)
was called.

## Usage

``` r
motis_clear_path(project_path = getwd(), quiet = FALSE)
```

## Arguments

- project_path:

  The path to the project directory containing the `.Rprofile` file.
  Defaults to the current working directory
  ([`getwd()`](https://rdrr.io/r/base/getwd.html)).

- quiet:

  A logical value. If `TRUE`, suppresses messages. Defaults to `FALSE`.

## Value

Invisibly returns `TRUE` if the `.Rprofile` was modified, `FALSE`
otherwise.

## Details

This function performs two actions:

1.  It restores the `PATH` for the current R session using a backup
    stored in `options("rmotis.original_path")`. If the backup is not
    found, a message is printed.

2.  It reads the `.Rprofile` file in the current working directory,
    removes any lines containing the special `#-added-by-r-pkg-rmotis`
    comment, and writes the cleaned content back to the file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Assuming motis_install(path_action = "project") was run previously

# Clean up the project's .Rprofile and restore the session PATH
motis_clear_path()
} # }
```
