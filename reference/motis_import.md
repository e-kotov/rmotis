# Import and Preprocess MOTIS Data

Runs `motis import` after a `config.yml` has been created.

## Usage

``` r
motis_import(
  work_dir,
  motis_path = NULL,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
)
```

## Arguments

- work_dir:

  A string. The path to the target directory where the assets will be
  placed.

- motis_path:

  An optional string. The path to the *directory* containing the `motis`
  executable. If `NULL`, the executable is assumed to be on the system
  `PATH`.

- echo_cmd:

  A logical. If `TRUE`, prints the full command. Defaults to `FALSE`.

- echo:

  A logical. If `TRUE`, streams process output. Defaults to `TRUE`.

- spinner:

  A logical. If `TRUE`, shows a console spinner. Defaults to `TRUE`.

## Value

The path to the created `data` directory, invisibly.
