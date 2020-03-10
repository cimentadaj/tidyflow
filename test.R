library(rsample)
library(recipes)
library(parsnip)
library(dials)
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

# Calculate all tuning params automatically
mod1 <- set_engine(linear_reg(penalty = tune(), mixture = tune("test")), "glmnet")
mod <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_recipe(~ recipe(mpg ~ cyl, data = .) %>% step_ns(cyl, deg_free = tune())) %>%
  plug_resample(vfold_cv) %>%
  plug_model(mod1) %>%
  plug_grid(grid_regular, penalty(c(-5, 0)), mixture(c(0, 0.5)), levels = 5)

mod %>%
  parameters()

# Change tuning grid
mod %>%
  replace_grid(grid_random, levels = 5)

# Specify tuning params through replace_grid
mod %>%
  replace_grid(grid_random, penalty(c(-5, 0)), mixture(c(0, 0.5)))

# Combination of fixed tuning params and varying
mod2 <- set_engine(linear_reg(penalty = tune(), mixture = 0), "glmnet")
mod %>%
  replace_grid(grid_random, penalty(c(-5, 0)))

# Combination of fixed tuning params and varying
mod2 <- set_engine(linear_reg(penalty = tune(), mixture = 0), "glmnet")
mod %>%
  replace_grid(grid_random, penalty(c(-5, 0)))
