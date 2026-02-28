# Configure MOTIS Import Settings

Updates the `config.yml` file in the root directory, which is used
during the
[`motis_import()`](https://www.ekotov.pro/rmotis/reference/motis_import.md)
process. This allows you to adjust how the data is preprocessed (e.g.,
matching distances, footpath lengths).

## Usage

``` r
motis_configure_import(path, ..., force = FALSE)
```

## Arguments

- path:

  A string. Path to the root MOTIS directory or the `config.yml` file.

- ...:

  Named arguments for configuration.

- force:

  A logical. If `FALSE` (default), asks for confirmation.

## Value

The path to the modified `config.yml`, invisibly.

## Import Configuration Options

**Timetable Settings (`timetable`):**

- `first_day`: First day of timetable to load ("YYYY-MM-DD" or "TODAY").

- `num_days`: Number of days to load (default: 365).

- `railviz`: Enable real-time vehicle visualization (default: true).

- `with_shapes`: Extract and serve shapes (default: true).

- `adjust_footpaths`: Adjust footpaths if they are too fast (default:
  true).

- `link_stop_distance`: Max distance in meters to link stops (default:
  100).

- `max_footpath_length`: Max footpath length in minutes (default: 15).

- `max_matching_distance`: Max distance from geolocation to OSM ways
  (default: 25.0).
