# Open MOTIS Web UI

Opens the MOTIS web interface for a running server in the default
browser.

## Usage

``` r
motis_open_ui(server = NULL, host = "127.0.0.1", port = 8080L)
```

## Arguments

- server:

  An optional
  [`processx::process`](http://processx.r-lib.org/reference/process.md)
  object from
  [`motis_start_server()`](http://www.ekotov.pro/rmotis/reference/motis_start_server.md).
  If provided, host and port are inferred from the server's command line
  arguments.

- host:

  A string. The server's host address. Defaults to `"127.0.0.1"`.

- port:

  An integer. The server's port. Defaults to `8080`.

## Value

Invisibly returns the URL that was opened.
