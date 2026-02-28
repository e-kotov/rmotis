# Launch a GUI to View and Debug MOTIS Routing

Launches a lightweight Shiny application to interactively visualize
point-to-point routing using a running MOTIS server. Supports left-click
for start, right-click for end.

## Usage

``` r
motis_gui(
  input_motis = NULL,
  port = NULL,
  style = "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json",
  center = c(13.4, 52.52),
  zoom = 10,
  debug = FALSE
)
```

## Arguments

- input_motis:

  Optional. Can be:

  - A `motis_server` object (process) returned by
    [`motis_start_server()`](https://www.ekotov.pro/rmotis/reference/motis_start_server.md).

  - A path string to a MOTIS working directory (contains `config.yml`
    and `data/`).

  - `NULL` (default): Auto-detects a running MOTIS server using
    [`motis_servers()`](https://www.ekotov.pro/rmotis/reference/motis_servers.md).

- port:

  Integer. The port the server is running on. If `NULL` (default),
  attempts to auto-detect from the server object or registry.

- style:

  Character. Map style for `mapgl`. Defaults to Carto Voyager.

- center:

  Numeric vector `c(lng, lat)`. Initial map center.

- zoom:

  Numeric. Initial zoom level.

- debug:

  Logical. If `TRUE`, prints detailed debug messages to the console.
  Defaults to `FALSE`.

## Value

No return value; launches a Shiny Gadget.
