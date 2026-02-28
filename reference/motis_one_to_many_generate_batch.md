# Generate MOTIS Batch Query File for One-to-Many

Efficiently constructs a text file of MOTIS one-to-many street routing
queries for batch processing. Also writes a metadata sidecar file
(`{output_file}.meta`) that records the origin and destination IDs for
each query line, enabling reliable reconstruction of results via
[`motis_one_to_many_read_batch()`](http://www.ekotov.pro/rmotis/reference/motis_one_to_many_read_batch.md).

## Usage

``` r
motis_one_to_many_generate_batch(
  one,
  many,
  output_file,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  withDistance = FALSE,
  one_id_col = "id",
  many_id_col = "id",
  ...,
  append = FALSE,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max",
  quiet = FALSE
)
```

## Arguments

- one:

  The single origin (when `arrive_by = FALSE`) or destination (when
  `arrive_by = TRUE`).

- many:

  The multiple destinations (when `arrive_by = FALSE`) or origins (when
  `arrive_by = TRUE`).

- output_file:

  The path to the output text file.

- mode:

  The routing profile to use (WALK, BIKE, CAR).

- arrive_by:

  Logical. If `FALSE` (the default), calculates routes from `one` to
  `many`. If `TRUE`, calculates routes from `many` to `one`.

- max:

  maximum travel time in seconds

- maxMatchingDistance:

  maximum matching distance in meters

- withDistance:

  Logical. Include distance in the query? Defaults to `FALSE`.

- one_id_col:

  The name of the column in `one` to use as the origin identifier in the
  metadata file. Defaults to `"id"`. Falls back to sequential row
  numbers if the column is not found.

- many_id_col:

  The name of the column in `many` to use as the destination identifiers
  in the metadata file. Defaults to `"id"`.

- ...:

  Additional MOTIS API parameters.

- append:

  Logical. If `TRUE`, appends to `output_file` and its `.meta` sidecar.

- api_endpoint:

  The API path. Defaults to `"/api/v1/one-to-many"`.

- duration_key:

  The name of the duration parameter in the query. Defaults to `"max"`.

- quiet:

  Logical. If `TRUE`, suppress status messages.

## Value

Invisibly returns the number of queries written (always 1).
