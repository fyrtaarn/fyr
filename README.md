# fyr <img src='man/figures/fyr_logo.png' align="right" width="120" height="139" />

## Intro

Register data from [Norsk pasientregister](https://www.fhi.no/he/npr/)
(NPR) and [Kommunalt pasient- og
brukerregister](https://www.fhi.no/he/kpr/) (KPR) contain information on
accidents and injuries in Norway. Nevertheless, merging these two data
sources is not a straightforward process. This package helps to mitigate
this challenge.

## Installation

To install `fyr` package can be done as follows:

``` r
if(!require(pak)) install.packages("pak")
pak::pkg_install("fyrtaarn/fyr")
```

## Usage

To count injury cases, use function `count_case()`.

``` r
dt <- count_case(
  d,
  period = 0,
  id = "lopenr",
  date.col = "innDato",
  acute = FALSE,
  days = 0,
  diag.col = "hoveddiagnoser"
)
```

See under
[References](https://fyrtaarn.github.io/fyr/reference/index.html) for
other functions.
