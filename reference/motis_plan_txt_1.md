# Builds MOTIS plan requests manually and dumps them to a text file.

This function efficiently constructs API requests for planning paired or
matrix journeys by building the URL strings manually, avoiding the
overhead of `httr2`. It writes the resulting URLs (path and query
string) to a specified text file, with each request on a new line.

## Usage

``` r
motis_plan_txt_1(
  from,
  to,
  output_file,
  time = Sys.time(),
  arrive_by = FALSE,
  from_id_col = "id",
  to_id_col = "id",
  ...,
  output = c("itineraries", "travel_time_matrix_long"),
  quiet = FALSE
)
```

## Arguments

- from:

  The origin location(s).

- to:

  The destination location(s).

- output_file:

  The path to the output text file.

- time:

  The departure/arrival time. Defaults to
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

- arrive_by:

  If `TRUE`, `time` is arrival time. Defaults to `FALSE`.

- from_id_col:

  Name of the ID column in `from`. Defaults to `"id"`.

- to_id_col:

  Name of the ID column in `to`. Defaults to `"id"`.

- ...:

  Additional parameters to be passed to the MOTIS Plan API, (e.g.,
  `maxTransfers`, `transitModes`).

- output:

  The mode of operation: `"itineraries"` for paired journeys or
  `"travel_time_matrix_long"` for a full matrix.

- quiet:

  Logical. If `TRUE`, suppress status messages.

## Value

Invisibly returns a character vector of the generated request strings.
The primary side effect is writing these strings to `output_file`.

## Examples

``` r
if (FALSE) { # \dontrun{
# --- Example Usage ---

# Define some origin and destination points
origins <- data.frame(
  id = c("berlin_hbf", "hamburg_hbf"),
  lat = c(52.525, 53.552),
  lon = c(13.369, 10.006)
)

destinations <- data.frame(
  name = c("Munich", "Frankfurt"),
  lat = c(48.140, 50.107),
  lon = c(11.560, 8.662)
)

# --- 1. Paired Journeys (Berlin -> Munich, Hamburg -> Frankfurt) ---
motis_plan_txt_1(
  from = origins,
  to = destinations,
  output_file = "paired_requests.txt",
  output = "itineraries",
  transitModes = c("RAIL", "COACH"), # Example of a vector parameter
  maxTransfers = 1
)

# --- 2. Travel Time Matrix (all origins to all destinations) ---
motis_plan_txt_1(
  from = origins,
  to = destinations,
  output_file = "matrix_requests.txt",
  output = "travel_time_matrix_long",
  maxTravelTime = 300 # 5 hours
)

} # }
```
