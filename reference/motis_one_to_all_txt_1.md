# Builds a MOTIS one-to-all request manually and saves it to a text file.

This function efficiently constructs the API request for a one-to-all
query by building the URL string manually. It writes the resulting URL
(path and query string) to a specified text file. This is useful for
scenarios where requests are generated in one step and executed later,
for example with a bulk processing tool.

## Usage

``` r
motis_one_to_all_txt_1(
  one,
  output_file,
  time = Sys.time(),
  arrive_by = FALSE,
  max_travel_time = 90,
  quiet = FALSE,
  ...
)
```

## Arguments

- one:

  The single origin (when `arrive_by = FALSE`) or destination (when
  `arrive_by = TRUE`).

- output_file:

  The path to the output text file where the request URL will be
  written.

- time:

  The departure or arrival time. Defaults to
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

- arrive_by:

  Logical. If `FALSE` (the default), calculates routes from `one`. If
  `TRUE`, calculates routes to `one`.

- max_travel_time:

  Integer. The maximum travel time in minutes. Defaults to 120.

- quiet:

  Logical. If `TRUE`, suppress status messages.

- ...:

  Additional parameters to be passed to the MOTIS One-to-All API, (e.g.,
  `maxTransfers`, `transitModes`).

## Value

Invisibly returns a character vector containing the generated request
string. The primary side effect is writing this string to `output_file`.
