# Diagnose MOTIS Setup

Performs a comprehensive health check of the MOTIS environment.

## Usage

``` r
motis_diagnose(
  port = 8080L,
  host = "127.0.0.1",
  work_dir = NULL,
  motis_path = NULL
)
```

## Arguments

- port:

  An integer. The port to check for a running MOTIS server.

- host:

  A string. The host address to check.

- work_dir:

  A string. The path to the target directory where the assets will be
  placed.

- motis_path:

  An optional string. The path to the *directory* containing the `motis`
  executable. If `NULL`, the executable is assumed to be on the system
  `PATH`.

## Value

Invisibly returns `NULL`. Prints diagnostic info to the console.
