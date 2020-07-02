## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- message = FALSE---------------------------------------------------------
library(tidymodels)
library(tidyflow)

tflow <-
  mtcars %>%
  tidyflow(seed = 52131) %>%
  plug_formula(mpg ~ .) %>% 
  plug_split(initial_split) %>%
  plug_model(linear_reg() %>% set_engine("lm"))

tflow

## -----------------------------------------------------------------------------
res <- fit(tflow)
res

## -----------------------------------------------------------------------------
pull_tflow_fit(res)$fit

## -----------------------------------------------------------------------------
res2 <-
  tflow %>%
  drop_formula() %>%
  plug_recipe(~ recipe(mpg ~ ., data = .x) %>% step_scale(all_predictors())) %>%
  fit()

pull_tflow_fit(res2)$fit

## -----------------------------------------------------------------------------
res3 <-
  res2 %>%
  plug_resample(vfold_cv, v = 10) %>%
  fit()

pull_tflow_fit_tuning(res3)

## -----------------------------------------------------------------------------
res3 %>%
  pull_tflow_fit_tuning() %>%
  collect_metrics()

## -----------------------------------------------------------------------------

res4 <-
  res3 %>%
  plug_grid(grid_regular) %>%
  replace_model(linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet")) %>%
  fit()

res4

## -----------------------------------------------------------------------------

res4 %>%
  pull_tflow_fit_tuning() %>%
  select_best(metric = "rmse")


## -----------------------------------------------------------------------------
final_model <-
  res4 %>%
  complete_tflow(metric = "rmse")

final_model

## -----------------------------------------------------------------------------
final_model %>%
  predict_training()

## -----------------------------------------------------------------------------
final_model %>%
  predict_testing()

## -----------------------------------------------------------------------------
tflow <-
  mtcars %>%
  tidyflow(seed = 52131) %>%
  plug_recipe(~ recipe(mpg ~ ., data = .x) %>% step_scale(all_predictors())) %>%
  plug_split(initial_split) %>%
  plug_resample(vfold_cv) %>% 
  plug_grid(grid_regular) %>%
  plug_model(linear_reg(penalty = tune(), mixture = tune()) %>% set_engine("glmnet"))

tflow %>%
  fit() %>%
  pull_tflow_fit_tuning() %>%
  collect_metrics()

