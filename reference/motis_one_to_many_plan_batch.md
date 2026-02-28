# Multi-origin Batch Planning for One-to-Many Routing

Generate a batch query file for multiple origins with spatial
optimizations. This function wraps
[`motis_one_to_many_generate_batch()`](https://www.ekotov.pro/rmotis/reference/motis_one_to_many_generate_batch.md)
and iterates over origins, applying spatial sorting and filtering for
improved performance.

## Usage

``` r
motis_one_to_many_plan_batch(
  one,
  many,
  output_file,
  mode = c("WALK", "BIKE", "CAR"),
  arrive_by = FALSE,
  max = 7200,
  maxMatchingDistance = 1000,
  one_id_col = "id",
  many_id_col = "id",
  withDistance = FALSE,
  ...,
  spatial_filter_km = NULL,
  spatial_sort = TRUE,
  progress = TRUE,
  api_endpoint = "/api/v1/one-to-many",
  duration_key = "max"
)
```

## Arguments

- one:

  Origins. Same as
  [`motis_one_to_many_batch()`](https://www.ekotov.pro/rmotis/reference/motis_one_to_many_batch.md).

- many:

  Destinations. Same as
  [`motis_one_to_many_batch()`](https://www.ekotov.pro/rmotis/reference/motis_one_to_many_batch.md).

- output_file:

  Path to the output query file.

- mode:

  Travel mode: `"WALK"`, `"BIKE"`, or `"CAR"`.

- arrive_by:

  Logical. If `TRUE`, treat `one` as destinations and `many` as origins
  (many-to-one).

- max:

  Maximum travel time in seconds. Defaults to `7200` (2 hours).

- maxMatchingDistance:

  Maximum map-matching distance in meters. Defaults to `1000`.

- one_id_col:

  Column name for origin IDs. Defaults to `"id"`.

- many_id_col:

  Column name for destination IDs. Defaults to `"id"`.

- withDistance:

  Logical. Include distance in the output? Defaults to `FALSE`.

- ...:

  Additional API parameters passed to the MOTIS one-to-many endpoint.

- spatial_filter_km:

  Numeric. Optional straight-line distance threshold (in kilometers). If
  provided, destinations further than this distance from an origin will
  be excluded from the batch query for that origin. Reduces query file
  size and server processing load.

- spatial_sort:

  Logical. If `TRUE` (default), sort origins by latitude before
  generating queries. Improves MOTIS graph cache locality when
  processing the batch file.

- progress:

  Logical. If `TRUE` (default), display progress messages.

- api_endpoint:

  API endpoint path. Defaults to `"/api/v1/one-to-many"`.

- duration_key:

  The name of the duration parameter in the query. Defaults to `"max"`.

## Value

Invisibly returns a list with:

- `file`: Path to the output file

- `n_lines`: Number of query lines generated

- `file_size`: File size in bytes

## Relationship to Other Functions

This function is a convenience wrapper that:

1.  Sorts origins by latitude (`spatial_sort = TRUE`)

2.  For each origin:

    - Filters destinations to a bounding box (`spatial_filter_km`)

    - Calls
      [`motis_one_to_many_generate_batch()`](https://www.ekotov.pro/rmotis/reference/motis_one_to_many_generate_batch.md)
      with `append = TRUE`

3.  Returns file metadata

## Spatial Optimizations

- **Spatial sort**: Origins are sorted by latitude before iteration.
  This improves MOTIS graph cache hits when processing the batch file
  sequentially.

- **Spatial filter**: For each origin, destinations are filtered to a
  bounding box based on `spatial_filter_km` and a 5% buffer. This
  reduces unnecessary map-matching attempts and response I/O.

## See also

[`motis_one_to_many_batch()`](https://www.ekotov.pro/rmotis/reference/motis_one_to_many_batch.md),
[`motis_one_to_many_generate_batch()`](https://www.ekotov.pro/rmotis/reference/motis_one_to_many_generate_batch.md)
