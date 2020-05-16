
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyflow

<!-- badges: start -->

[![R build
status](https://github.com/cimentadaj/tidyflow/workflows/R-CMD-check/badge.svg)](https://github.com/cimentadaj/tidyflow/actions)
[![Codecov test
coverage](https://codecov.io/gh/cimentadaj/tidyflow/branch/master/graph/badge.svg)](https://codecov.io/gh/cimentadaj/tidyflow?branch=master)
<!-- badges: end -->

## What is a tidyflow?

<!-- A tidyflow is an object that can bundle together your data, splitting, resampling, preprocessing, modeling, and post-processing requests. For example, if you have a `recipe` and `parsnip` model, these can be combined into a workflow. The advantages are: -->

<!--  * You don't have to keep track of separate objects in your workspace. -->

<!--  * The recipe prepping and model fitting can be executed using a single call to `fit()`. -->

<!--  * If you have custom tuning parameter settings, these can be defined using a simpler interface when combined with [tune](https://github.com/tidymodels/tune). -->

<!--  * In the future, workflows will be able to add post-processing operations, such as modifying the probability cutoff for two-class models. -->

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/tidyflow")
```

## Example

``` r
library(tidymodels)
#> ── Attaching packages ────────────────────────────────────── tidymodels 0.1.0 ──
#> ✔ broom     0.5.4           ✔ recipes   0.1.12     
#> ✔ dials     0.0.6           ✔ rsample   0.0.6      
#> ✔ dplyr     0.8.99.9003     ✔ tibble    3.0.1.9000 
#> ✔ ggplot2   3.3.0.9000      ✔ tune      0.1.0      
#> ✔ infer     0.5.1           ✔ workflows 0.1.1      
#> ✔ parsnip   0.1.0           ✔ yardstick 0.0.6      
#> ✔ purrr     0.3.4
#> ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
#> ✖ purrr::discard()  masks scales::discard()
#> ✖ dplyr::filter()   masks stats::filter()
#> ✖ dplyr::lag()      masks stats::lag()
#> ✖ ggplot2::margin() masks dials::margin()
#> ✖ recipes::step()   masks stats::step()
library(tidyflow)
#> 
#> Attaching package: 'tidyflow'
#> The following objects are masked from 'package:workflows':
#> 
#>     .fit_model, .fit_pre

# Build tidyflow
tflow <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_formula(mpg ~ .) %>%
  plug_model(linear_reg())

# Fit model
fit_m <- fit(tflow)
#> Warning: Engine set to `lm`.

# Predict on testing
fit_m %>%
  predict_testing()
#> # A tibble: 8 x 12
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb .pred
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21.4     6 258     110  3.08  3.22  19.4     1     0     3     1  19.3
#> 2  24.4     4 147.     62  3.69  3.19  20       1     0     4     2  20.3
#> 3  32.4     4  78.7    66  4.08  2.2   19.5     1     1     4     1  25.1
#> 4  33.9     4  71.1    65  4.22  1.84  19.9     1     1     4     1  26.9
#> 5  15.5     8 318     150  2.76  3.52  16.9     0     0     3     2  16.2
#> 6  13.3     8 350     245  3.73  3.84  15.4     0     0     3     4  15.6
#> 7  26       4 120.     91  4.43  2.14  16.7     0     1     5     2  23.5
#> 8  30.4     4  95.1   113  3.77  1.51  16.9     1     1     5     2  23.2
```

## Code of Conduct

Please note that the tidyflow project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
