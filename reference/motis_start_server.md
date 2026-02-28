# Start a MOTIS Server Process

Launches the `motis server` command as a background process.

## Usage

``` r
motis_start_server(work_dir, motis_path = NULL, port = NULL, echo_cmd = FALSE)
```

## Arguments

- work_dir:

  A string. The path to the target directory where the assets will be
  placed.

- motis_path:

  An optional string. The path to the *directory* containing the `motis`
  executable. If `NULL`, the executable is assumed to be on the system
  `PATH`.

- port:

  Integer. The port to bind to. If NULL, tries to read from config.yml,
  defaults to 8080.

- echo_cmd:

  A logical. If `TRUE`, prints the full command. Defaults to `FALSE`.

## Value

A `motis_server` object (inherits from
[`processx::process`](http://processx.r-lib.org/reference/process.md)).
