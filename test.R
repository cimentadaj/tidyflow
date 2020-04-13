library(tidymodels)
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


library(mlbench)
data(Ionosphere)
Ionosphere <- Ionosphere %>% select(-V2)

svm_mod <-
  svm_rbf(cost = tune("my_cost"), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

iono_rec <-
  ~ recipe(Class ~ ., data = .x)  %>%
  # In case V1 is has a single value sampled
  step_zv(all_predictors()) %>% 
  # convert it to a dummy variable
  step_dummy(V1) %>%
  # Scale it the same as the others
  step_range(matches("V1_"))

tflow <-
  Ionosphere %>% 
  tidyflow(seed = 4943) %>%
  plug_recipe(iono_rec) %>%
  plug_resample(bootstraps, times = 30) %>%
  plug_model(svm_mod) %>%
  plug_grid(grid_latin_hypercube,
            my_cost = cost(c(-10, 10)),
            my_cost = cost(c(-1, 1)),
            size = 10)

t1 <- tflow %>% fit()

## TODO
# This is to become a test in the package to make sure both tidyflow
# and tidymodels returns the same result always.
# The problem why they don't match is because the tuning grid
# returns different values. I spotted the error in dials
# and created this reprex here: https://github.com/tidymodels/dials/issues/109
library(mlbench)
data(Ionosphere)
Ionosphere <- Ionosphere %>% select(-V2)

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

iono_rec <-
  ~ recipe(Class ~ ., data = .x)  %>%
  # In case V1 is has a single value sampled
  step_zv(all_predictors()) %>% 
  # convert it to a dummy variable
  step_dummy(V1) %>%
  # Scale it the same as the others
  step_range(matches("V1_"))

tflow <-
  Ionosphere %>% 
  tidyflow(seed = 4943) %>%
  plug_recipe(iono_rec) %>%
  plug_resample(bootstraps, times = 30) %>%
  plug_model(svm_mod) %>%
  plug_grid(grid_latin_hypercube, size = 10)

tst <-
  tflow %>%
  fit()

library(mlbench)
library(tidymodels)
data(Ionosphere)

Ionosphere <- Ionosphere %>% select(-V2)

svm_mod <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

iono_rec <-
  recipe(Class ~ ., data = Ionosphere)  %>%
  # In case V1 is has a single value sampled
  step_zv(all_predictors()) %>% 
  # convert it to a dummy variable
  step_dummy(V1) %>%
  # Scale it the same as the others
  step_range(matches("V1_"))

set.seed(4943)
iono_rs <- bootstraps(Ionosphere, times = 30)
set.seed(4943)
grid <- grid_latin_hypercube(cost(), rbf_sigma(), size = 10)


set.seed(4943)
rec_form <-
  tune_grid(
    object = svm_mod,
    preprocessor = iono_rec,
    resamples = iono_rs,
    control = ctrl,
    grid = grid
  )

x <- tblattr_2_df(pull_tflow_fit_tuning(tst))
y <- tblattr_2_df(rec_form)

# This sould be he same for both x and y
x %>% slice(1) %>% tidyr::unnest(.metrics)
y %>% slice(1) %>% tidyr::unnest(.metrics)
