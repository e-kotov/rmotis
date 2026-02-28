# Prepare MOTIS Data Directory (Config + Import)

A wrapper that runs `motis_config` and `motis_import`.

## Usage

``` r
motis_prepare_data(
  work_dir,
  osm_pbf = NULL,
  gtfs_files = NULL,
  assets_action = c("copy", "symlink", "none"),
  motis_path = NULL,
  host = "127.0.0.1",
  port = 8080L,
  force = FALSE,
  unlock_limits = TRUE,
  echo_cmd = FALSE,
  echo = TRUE,
  spinner = TRUE
)
```

## Arguments

- work_dir:

  A string. The path to the target directory where the assets will be
  placed.

- osm_pbf:

  An optional string. The path to a single OpenStreetMap `.osm.pbf`
  file.

- gtfs_files:

  An optional character vector of paths to GTFS `.zip` files.

- assets_action:

  A string specifying how to handle the assets. One of:

  - `"copy"` (default): Copies assets to the `work_dir`. Safe and works
    everywhere.

  - `"symlink"`: Creates a symbolic link to the assets. Saves disk
    space, best for non-Windows systems. Will fall back to copying if it
    fails.

  - `"none"`: Does nothing. For users who manage assets manually.

- motis_path:

  An optional string. The path to the *directory* containing the `motis`
  executable. If `NULL`, the executable is assumed to be on the system
  `PATH`.

- host:

  A string. The IP address for the server to bind to.

- port:

  An integer. The TCP port for the server.

- force:

  A logical. If `TRUE`, an existing `config.yml` file in the `work_dir`
  will be overwritten. Defaults to `FALSE`.

- unlock_limits:

  A logical. If `TRUE` (default), sets very high server limits to
  prevent query capping.

- echo_cmd:

  A logical. If `TRUE`, prints the full command. Defaults to `FALSE`.

- echo:

  A logical. If `TRUE`, streams process output. Defaults to `TRUE`.

- spinner:

  A logical. If `TRUE`, shows a console spinner. Defaults to `TRUE`.

## Value

The normalized path to the working directory, invisibly.
