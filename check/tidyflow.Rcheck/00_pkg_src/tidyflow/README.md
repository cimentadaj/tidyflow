
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyflow

<!-- badges: start -->

[![R build
status](https://github.com/cimentadaj/tidyflow/workflows/R-CMD-check/badge.svg)](https://github.com/cimentadaj/tidyflow/actions)
[![Codecov test
coverage](https://codecov.io/gh/cimentadaj/tidyflow/branch/master/graph/badge.svg)](https://codecov.io/gh/cimentadaj/tidyflow?branch=master)
<!-- badges: end -->

## What is a tidyflow?

A tidyflow is a fork of [workflows](https://workflows.tidymodels.org/)
that can bundle together your data, splitting, resampling,
preprocessing, modeling, and grid search. Having all these steps
separated into different objects can prove to be difficult. One can
predict on the testing data by mistake, forget whether the recipe has
been baked or not, or simply do not remember the name of all the tuning
parameters to specify in the grid. `tidyflow` is a package aimed at
bundling all of these steps into a a coherent flow. Among the advantages
are:

  - You don’t have to keep track of separate objects in your workspace.

  - The split, resample, recipe prepping, model fitting and grid search
    can be executed using a single call to `fit()`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("cimentadaj/tidyflow")
```

## Example

Simple example:

``` r
library(tidymodels)
library(tidyflow)
```

``` r
# Build tidyflow
tflow <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_formula(mpg ~ .) %>%
  plug_model(set_engine(linear_reg(), "lm"))

# Fit model
fit_m <- fit(tflow)

# Predict on testing
fit_m %>%
  predict_training()
#> # A tibble: 24 x 12
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb .pred
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4  22.0
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4  21.8
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1  27.7
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1  22.2
#>  5  24.4     4  147.    62  3.69  3.19  20       1     0     4     2  22.9
#>  6  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2  24.0
#>  7  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4  20.2
#>  8  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3  14.5
#>  9  15.2     8  276.   180  3.07  3.78  18       0     0     3     3  15.5
#> 10  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4  12.2
#> # … with 14 more rows
```

Complex example:

``` r
# Build tidyflow
tflow <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_formula(mpg ~ .) %>%
  plug_resample(vfold_cv) %>%
  plug_grid(grid_regular) %>% 
  plug_model(set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet"))

# Fit model
fit_m <- fit(tflow)

# Extract tuning grid
fit_m %>%
  pull_tflow_fit_tuning()
#> #  10-fold cross-validation 
#> # A tibble: 10 x 4
#>    splits         id     .metrics          .notes          
#>    <list>         <chr>  <list>            <list>          
#>  1 <split [21/3]> Fold01 <tibble [18 × 5]> <tibble [0 × 1]>
#>  2 <split [21/3]> Fold02 <tibble [18 × 5]> <tibble [0 × 1]>
#>  3 <split [21/3]> Fold03 <tibble [18 × 5]> <tibble [0 × 1]>
#>  4 <split [21/3]> Fold04 <tibble [18 × 5]> <tibble [0 × 1]>
#>  5 <split [22/2]> Fold05 <tibble [18 × 5]> <tibble [0 × 1]>
#>  6 <split [22/2]> Fold06 <tibble [18 × 5]> <tibble [0 × 1]>
#>  7 <split [22/2]> Fold07 <tibble [18 × 5]> <tibble [0 × 1]>
#>  8 <split [22/2]> Fold08 <tibble [18 × 5]> <tibble [0 × 1]>
#>  9 <split [22/2]> Fold09 <tibble [18 × 5]> <tibble [0 × 1]>
#> 10 <split [22/2]> Fold10 <tibble [18 × 5]> <tibble [0 × 1]>

# Fit best model on the entire training data
final_m <-
  fit_m %>%
  complete_tflow(metric = "rmse")

# Predict on train
final_m %>%
  predict_training()
#> # A tibble: 24 x 12
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb .pred
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4  21.9
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4  21.9
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1  25.9
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1  20.7
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2  16.9
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1  20.3
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4  13.7
#>  8  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4  19.8
#>  9  17.3     8  276.   180  3.07  3.73  17.6     0     0     3     3  16.1
#> 10  15.2     8  276.   180  3.07  3.78  18       0     0     3     3  16.3
#> # … with 14 more rows

# Predict on testing
final_m %>%
  predict_testing()
#> # A tibble: 8 x 12
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb .pred
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  24.4     4 147.     62  3.69  3.19  20       1     0     4     2  23.3
#> 2  22.8     4 141.     95  3.92  3.15  22.9     1     0     4     2  24.9
#> 3  17.8     6 168.    123  3.92  3.44  18.9     1     0     4     4  20.2
#> 4  16.4     8 276.    180  3.07  4.07  17.4     0     0     3     3  15.5
#> 5  10.4     8 472     205  2.93  5.25  18.0     0     0     3     4  12.6
#> 6  27.3     4  79      66  4.08  1.94  18.9     1     1     4     1  27.6
#> 7  30.4     4  95.1   113  3.77  1.51  16.9     1     1     5     2  25.2
#> 8  15       8 301     335  3.54  3.57  14.6     0     1     5     8  12.1
```

## Code of Conduct

Please note that the tidyflow project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
