# Run Full One-to-Many Batch Routing Cycle via CLI

**\[deprecated\]**

This function is now a wrapper around
[`motis_one_to_many()`](https://www.ekotov.pro/rmotis/reference/motis_one_to_many.md)
with `engine = "batch"`.

## Usage

``` r
motis_one_to_many_batch(
  one,
  many,
  data_dir,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  one_id_col = "id",
  many_id_col = "id",
  withDistance = FALSE,
  ...,
  motis_path = NULL,
  chunk_size = 10000L,
  output_callback = NULL,
  echo = TRUE,
  output_dir = tempdir(),
  keep_files = FALSE,
  spatial_filter_km = NULL,
  spatial_sort = TRUE,
  split = 1L,
  eol = NULL
)
```

## Arguments

- one:

  Origin(s). Can be a data frame/tibble with coordinate columns, an `sf`
  object, or a vector/matrix of coordinates.

- many:

  Destination(s). Same format as `one`.

- data_dir:

  Path to MOTIS data directory. Required if `engine='batch'`.

- mode:

  The routing profile to use. Defaults to `"WALK"`.

- arrive_by:

  Logical. If `FALSE` (the default), calculates routes from `one` to
  `many`. If `TRUE`, calculates routes from `many` to `one`.

- max:

  maximum travel time in seconds

- maxMatchingDistance:

  maximum matching distance in meters

- one_id_col:

  The name of the column in `one` to use for identifying the point in
  the output. Defaults to `"id"`.

- many_id_col:

  The name of the column in `many` to use for identifying the point in
  the output. Defaults to `"id"`.

- withDistance:

  Logical. Include distance in the output? Defaults to `FALSE`.

- ...:

  Additional MOTIS API parameters.

- motis_path:

  Path to the directory containing the MOTIS binary, or `NULL` to use
  the system PATH.

- chunk_size:

  Number of lines to read and process at a time. Defaults to `10000L`.

- output_callback:

  Optional function that receives each processed chunk (a data.frame) as
  its argument.

- echo:

  Logical. If `TRUE` (default), echo MOTIS batch output (timing
  statistics) to the console.

- output_dir:

  Directory where to save the temporary batch files. Mapped to
  `temp_dir` in the new interface.

- keep_files:

  Logical. Keep temporary files? Defaults to `FALSE`.

- spatial_filter_km:

  Numeric. Optional straight-line distance threshold.

- spatial_sort:

  Logical. Sort origins spatially.

- split:

  Integer. Mapped to `max_destinations_per_batch` if \> 1.

- eol:

  Optional line ending for batch query files (e.g., `"\n"` for LF or
  `"\r\n"` for CRLF). If provided, forces this line ending even on
  Windows. Typically, `"\n"` is required for MOTIS batch processing.

## Value

A data.frame or `output_path` invisibly.
