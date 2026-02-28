# Set Server Address in a MOTIS Configuration File

Updates the `server` settings for `host` and `port` in a MOTIS
configuration file. Automatically targets the server configuration
(usually in the `data/` subdirectory).

## Usage

``` r
motis_set_server_address(path, host = "127.0.0.1", port = 8080L, force = FALSE)
```

## Arguments

- path:

  A string. Path to the `config.yml` file, the `data/` directory, or the
  root MOTIS project directory.

- host:

  A string. The IP address for the server to bind to.

- port:

  An integer between 1 and 65535. The TCP port for the server.

- force:

  A logical. If `FALSE` (default), the function will ask for
  confirmation before modifying the file in an interactive session. If
  `TRUE`, the file is always modified.

## Value

The path to the modified `config.yml` file, invisibly.

## Details

Because this function modifies a file in place, it requires confirmation
if run in an interactive session. To override this and allow
modification without a prompt (e.g., in scripts), use `force = TRUE`.
