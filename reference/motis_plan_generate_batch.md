# Generate MOTIS Batch Query File

Efficiently constructs a text file of MOTIS routing queries for batch
processing. This function is designed for performance, using vectorised
operations to generate potentially millions of query URLs in seconds.

## Usage

``` r
motis_plan_generate_batch(
  from,
  to,
  output_file,
  time = Sys.time(),
  arrive_by = FALSE,
  from_id_col = "id",
  to_id_col = "id",
  all_pairs = FALSE,
  ...,
  append = FALSE,
  api_endpoint = "/api/v1/plan",
  quiet = FALSE
)
```

## Arguments

- from:

  The origin location(s). Can be a character vector of station IDs, a
  data frame/tibble with ID or coordinate columns, an `sf` object, or a
  numeric matrix (`lon`, `lat`).

- to:

  The destination location(s). Same types as `from`.

- output_file:

  The path to the output text file.

- time:

  The departure or arrival time. Can be a single `POSIXct` object
  (applied to all queries) or a vector of the same length as the number
  of queries. Defaults to
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html).

- arrive_by:

  Logical. If `TRUE`, `time` is treated as the arrival time. Defaults to
  `FALSE`.

- from_id_col:

  Column name for station IDs in `from`. Defaults to `"id"`.

- to_id_col:

  Column name for station IDs in `to`. Defaults to `"id"`.

- all_pairs:

  Logical. If `TRUE`, generates queries for all combinations of `from`
  and `to` (Cartesian product). If `FALSE` (default), `from` and `to`
  must have the same length and are paired 1-to-1.

- ...:

  Additional MOTIS API parameters (e.g., `directModes`,
  `maxTravelTime`). Vector arguments (e.g. `c("WALK", "CAR")`) are
  automatically collapsed to comma-separated strings. Parameters are
  validated against
  [`motis.client::mc_plan()`](https://rdrr.io/pkg/motis.client/man/mc_plan.html)
  before generation begins.

- append:

  Logical. If `TRUE`, appends to `output_file` instead of overwriting.
  Defaults to `FALSE`.

- api_endpoint:

  The API path. Defaults to `"/api/v1/plan"`.

- quiet:

  Logical. If `TRUE`, suppress status messages.

## Value

Invisibly returns the number of queries written to the file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate paired queries
origins <- data.frame(lat = c(59.3, 59.4), lon = c(18.0, 18.1))
dests <- data.frame(lat = c(59.5, 59.6), lon = c(18.2, 18.3))
motis_plan_generate_batch(origins, dests, "queries.txt")

# Generate all-pairs matrix
motis_plan_generate_batch(origins, dests, "matrix.txt", all_pairs = TRUE)
} # }
```
