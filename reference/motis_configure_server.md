# Configure a MOTIS Server

Updates the `config.yml` file used for running the MOTIS server
(typically located in the `data/` subdirectory). This function can
automatically identify the data directory even if you provide the path
to the root project folder.

## Usage

``` r
motis_configure_server(path, ..., force = FALSE)
```

## Arguments

- path:

  A string. Path to the `config.yml` file, the `data/` directory, or the
  root MOTIS project directory.

- ...:

  Named arguments representing the configuration structure. Nested keys
  can be provided as named lists.

- force:

  A logical. If `FALSE` (default), asks for confirmation in interactive
  sessions before modifying the file.

## Value

The path to the modified `config.yml`, invisibly.

## Configuration Options

**Server Settings (`server`):**

- `host`: The IP address for the server to bind to (default: "0.0.0.0").

- `port`: The TCP port for the server (default: 8080).

- `web_folder`: Folder with static files to serve (e.g., "ui").

- `n_threads`: Number of hardware threads to use (default: auto).

- `data_attribution_link`: Link to data sources or license.

**Routing Limits (`limits`):**

- `stoptimes_max_results`: Max results for stoptimes (default: 256).

- `plan_max_results`: Max results for plan queries (default: 256).

- `plan_max_search_window_minutes`: Max search window in minutes (max:
  21600).

- `onetomany_max_many`: Maximum accepted number of `many` locations for
  one-to-many requests.

- `onetoall_max_results`: Max results for one-to-all queries.

- `onetoall_max_travel_minutes`: Max travel duration for one-to-all.

- `routing_max_timeout_seconds`: Max duration for a routing query
  (default: 90).

- `street_routing_max_prepost_transit_seconds`: Limit for
  maxPre/PostTransitTime.

- `street_routing_max_direct_seconds`: Limit for maxDirectTime.

**Modules:**

- `street_routing`: Enable street routing (Boolean or list with
  `elevation_data_dir`).

- `geocoding`: Enable geocoding (Boolean).

- `reverse_geocoding`: Enable reverse geocoding (Boolean).

- `osr_footpath`: Enable OSR footpath routing (Boolean).

## Examples

``` r
if (FALSE) { # \dontrun{
# Update server port in the data directory
motis_configure_server(
  "~/motis-project",
  server = list(port = 8081)
)

# Increase routing limits
motis_configure_server(
  "~/motis-project/data",
  limits = list(plan_max_results = 500)
)
} # }
```
