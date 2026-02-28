# rmotis

The goal of `rmotis` is to automate installation of the
[MOTIS](https://github.com/motis-project/motis) server for routing and
provide user friendly API for the most common use cases in a manner
simialar to [`{r5r}`](https://github.com/ipeaGIT/r5r/) and
[`osrm`](https://github.com/riatelab/osrm) +
[`osrm.backend`](https://github.com/e-kotov/osrm.backend).

This is highly experimental, use at your own risk. `rmotis` depends on
[`motis.client`](https://github.com/e-kotov/motis.client) R package
which is entirely generated form official API spec using
[`openapi3`](https://github.com/e-kotov/openapi3/) R package. All pieces
of this are experimental and may break at any time.

## Installation

You can install the development version of rmotis from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("e-kotov/rmotis")
```
