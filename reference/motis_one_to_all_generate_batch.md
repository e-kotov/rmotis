# Generate MOTIS Batch Query File for One-to-All

Efficiently constructs a text file of MOTIS one-to-all queries for batch
processing.

## Usage

``` r
motis_one_to_all_generate_batch(
  one,
  output_file,
  time = Sys.time(),
  max_travel_time = 90,
  arrive_by = FALSE,
  ...,
  append = FALSE,
  api_endpoint = "/api/v1/one-to-all"
)
```

## Arguments

- one:

  The origin location (when `arrive_by = FALSE`) or destination (when
  `arrive_by = TRUE`).

- output_file:

  The path to the output text file.

- time:

  The departure or arrival time. Defaults to
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

- max_travel_time:

  Integer. The maximum travel time in minutes. Defaults to 90.

- arrive_by:

  Logical. If `FALSE` (the default), calculates routes from `one` to all
  reachable stops. If `TRUE`, calculates routes from all reachable stops
  to `one`.

- ...:

  Additional MOTIS API parameters.

- append:

  Logical. If `TRUE`, appends to `output_file`.

- api_endpoint:

  The API path. Defaults to `"/api/v1/one-to-all"`.

## Value

Invisibly returns the number of queries written.
