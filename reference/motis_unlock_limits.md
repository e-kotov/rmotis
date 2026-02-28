# Unlock MOTIS Server Limits

Sets extremely high limits in the MOTIS configuration file to ensure
that most queries (even very long or complex ones) are not capped by the
server. This function automatically targets the server configuration
(usually in the `data/` subdirectory).

## Usage

``` r
motis_unlock_limits(path, force = FALSE)
```

## Arguments

- path:

  Path to the `config.yml` file, the `data/` directory, or the root
  MOTIS project directory.

- force:

  A logical. If `TRUE`, skips user confirmation.

## Value

The path to the modified `config.yml`, invisibly.

## Details

The preset includes `limits.onetomany_max_many = 30000`.
