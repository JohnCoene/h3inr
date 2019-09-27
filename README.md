
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/JohnCoene/h3inr.svg?branch=master)](https://travis-ci.org/JohnCoene/h3inr)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

# h3inr

Brings [Uber h3](https://github.com/uber/h3-js) to R via
[V8](https://github.com/jeroen/V8).

## Installation

You can install the package from Github:

``` r
# install.packages("remotes")
remotes::install_github("JohnCoene/h3inr")
```

## Example

``` r
library(h3inr)

hexagons <- quakes %>% 
  geo_to_h3(lat, long, resolution = 4L) %>% # get hex id
  h3_to_geo(hex = hex) # get hex center coordinates

hexagons %>% 
  head() %>% 
  knitr::kable()
```

|     lat |   long | depth | mag | stations | hex             | hex\_center\_lat | hex\_center\_lon |
| ------: | -----: | ----: | --: | -------: | :-------------- | ---------------: | ---------------: |
| \-20.42 | 181.62 |   562 | 4.8 |       41 | 849b701ffffffff |       \-20.44006 |       \-178.3266 |
| \-20.62 | 181.03 |   650 | 4.2 |       15 | 849b757ffffffff |       \-20.63448 |       \-179.1528 |
| \-26.00 | 184.10 |    42 | 5.4 |       43 | 84bad65ffffffff |       \-25.80558 |       \-175.8370 |
| \-17.97 | 181.66 |   626 | 4.1 |       19 | 849b557ffffffff |       \-17.88495 |       \-178.1865 |
| \-20.42 | 181.96 |   649 | 4.0 |       11 | 849b705ffffffff |       \-20.34078 |       \-177.9118 |
| \-19.68 | 184.31 |   195 | 4.0 |       12 | 849b08bffffffff |       \-19.82371 |       \-175.8216 |

See [globe4r](https://globe4r.john-coene.com/articles/hexagons.html) for
an example of what h3inr enables.
