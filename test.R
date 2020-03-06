library(rsample)
library(recipes)
library(parsnip)
devtools::load_all()

res <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>% 
  plug_formula(mpg ~ .) %>% 
  plug_model(set_engine(linear_reg(), "lm"))

res

set.seed(23131)
res %>%
  fit()

mod <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_recipe(~ recipe(mpg ~ cyl, data = .) %>% step_log(cyl, base = 10)) %>%
  plug_resample(vfold_cv) %>%
  plug_model(set_engine(linear_reg(), "lm"))

res_vf <-
  mod %>%
  fit()

res_vf

pull_tflow_tuning(res_vf) %>% show_best(metric = "rsq")

## tflow <-
##   res_vf %>%
##   drop_resample()

res_vf %>%
  drop_resample() %>%
  fit()


## Once you finish that, all of this was done in order to adapt
## finalize_workflow to finalize_tidyflow to update the final model

## You will have two functions: one which updates the tidyflow with
## the final parameters and another which fits the last model.
## As a shortcut, you'll have one which does both things be default.
