library(tidymodels)
devtools::load_all()

mod1 <-
  set_engine(
    rand_forest(mode = "classification", mtry = tune()),
    "randomForest"
  )

tflow <-
  mtcars %>%
  mutate(am = as.factor(am)) %>% 
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_resample(vfold_cv) %>% 
  plug_formula(am ~ .) %>%
  plug_grid(grid_regular) %>% 
  plug_model(mod1)

res <-
  tflow %>%
  fit()

res %>%
  complete_tflow(select_best(pull_tflow_fit_tuning(res), "accuracy")) %>% 
  predict_testing() %>%
  accuracy(am, .pred_class)

## Once you finish that, all of this was done in order to adapt
## finalize_workflow to finalize_tidyflow to update the final model

## You will have two functions: one which updates the tidyflow with
## the final parameters and another which fits the last model.
## As a shortcut, you'll have one which does both things be default.

# Calculate all tuning params automatically
mod1 <- set_engine(linear_reg(penalty = tune(), mixture = tune("test")), "glmnet")
mod2 <- set_engine(linear_reg(), "lm")
mod3 <-
  svm_rbf(cost = tune("my_cost"), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

mod1 <- set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet")
mod2 <- set_engine(linear_reg(), "lm")
mod <-
  mtcars %>%
  tidyflow(seed = 52315) %>%
  plug_split(initial_split, prop = 0.7) %>%
  plug_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(disp, deg_free = tune())) %>%
  ## plug_formula(mpg ~ .) %>% 
  plug_resample(vfold_cv) %>%
  plug_model(mod1) %>% 
  plug_grid(grid_regular)

tst1 <-
  mod %>%
  fit()

final_res <-
  tst1 %>%
  pull_tflow_fit_tuning() %>%
  select_best(metric = "rsq") %>%
  complete_tflow(tst1, .)


bind_cols(
  pull_tflow_testing(final_res),
  predict(final_res, new_data = pull_tflow_testing(final_res))
) %>%
  yardstick::rmse(mpg, .pred)

tst2 <-
  mod %>%
  replace_model(mod2) %>%
  fit()
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

library(tidymodels)

svm_mod <-
  svm_rbf(cost = tune("my_cost"), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

mt_rec <-
  recipe(mpg ~ ., data = mtcars)  %>%
  # In case V1 is has a single value sampled
  step_zv(all_predictors()) %>% 
  # convert it to a dummy variable
  step_mutate(cyl = factor(cyl)) %>% 
  step_dummy(cyl) %>% 
  step_ns(hp, deg_free = tune())

prep(mt_rec)

library(tidymodels)
devtools::load_all()
library(mlbench)
data(Ionosphere)
Ionosphere <- Ionosphere %>% select(-V2) %>% mutate(cont = 1:nrow(.))

svm_mod <-
  svm_rbf(cost = tune("my_cost"), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

iono_rec <-
  ~ recipe(Class ~ ., data = .)  %>%
  # In case V1 is has a single value sampled
  step_zv(all_predictors()) %>% 
  # convert it to a dummy variable
  step_dummy(V1) %>%
  # Scale it the same as the others
  step_range(matches("V1_")) %>% 
  step_ns(cont, deg_free = tune())

tflow <-
  Ionosphere %>% 
  tidyflow(seed = 4943) %>%
  plug_recipe(iono_rec) %>%
  plug_resample(bootstraps, times = 30) %>%
  plug_model(svm_mod) %>%
  plug_grid(grid_latin_hypercube,
            my_cost = cost(c(-10, 10)),
            size = 10)

t1 <- tflow %>% fit()
best_params <- t1 %>% pull_tflow_fit_tuning() %>% select_best("accuracy")
control = control_tidyflow()

t2 <- complete_tflow(t1, t1 %>% pull_tflow_fit_tuning() %>% select_best("accuracy"))


## TODO
# This is to become a test in the package to make sure both tidyflow
# and tidymodels returns the same result always.
# The problem why they don't match is because the tuning grid
# returns different values. I spotted the error in dials
# and created this reprex here: https://github.com/tidymodels/dials/issues/109
library(mlbench)
library(mlbench)
library(tidymodels)
devtools::load_all()

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
  step_range(matches("V1_")) %>% 
  step_ns(V4, deg_free = round(runif(1, 0, 10)))

tflow <-
  Ionosphere %>% 
  tidyflow(seed = 4943) %>%
  plug_split(initial_split) %>% 
  plug_recipe(iono_rec) %>%
  plug_resample(bootstraps, times = 3) %>%
  plug_grid(grid_latin_hypercube, size = 1) %>% 
  plug_model(svm_mod)

tst1 <-
  tflow %>%
  fit()

# check random equality
# recipe
# initial_split
# resample
# grid
# model

data(Ionosphere)

Ionosphere <- Ionosphere %>% select(-V2)

set.seed(4943)
Ionosphere <- training(initial_split(Ionosphere))

set.seed(4943)
iono_rs <- bootstraps(Ionosphere, times = 3)

svm_mod2 <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

set.seed(4943)
iono_rec <-
  recipe(Class ~ ., data = Ionosphere)  %>%
  # In case V1 is has a single value sampled
  step_zv(all_predictors()) %>% 
  # convert it to a dummy variable
  step_dummy(V1) %>%
  # Scale it the same as the others
  step_range(matches("V1_")) %>%
  step_ns(V4, deg_free = round(runif(1, 0, 10)))

set.seed(4943)
grid <- grid_latin_hypercube(cost(range = c(-10, 5)),
                             rbf_sigma(range = c(-10, 0)),
                             size = 1)

set.seed(4943)
rec_form <-
  tune_grid(
    object = svm_mod2,
    preprocessor = iono_rec,
    resamples = iono_rs,
    control = control_grid(),
    grid = grid
  )

x <- tblattr_2_df(pull_tflow_fit_tuning(tst1))
y <- tblattr_2_df(rec_form)

# This sould be he same for both x and y
x %>% slice(1) %>% tidyr::unnest(.metrics)
y %>% slice(1) %>% tidyr::unnest(.metrics)

## all.equal(pull_tflow_grid(tst1), grid)
## all.equal(svm_mod, svm_mod2)
## outside_splits <- lapply(iono_rs$splits, function(x) as_tibble(select(training(x), Class, everything())))
## tflow_splits <- lapply(pull_tflow_resample(tst1)$splits, function(x) as_tibble(select(training(x), Class, everything())))

## for (i in 1:length(tflow_splits)) {
##   print(all.equal(outside_splits[[i]], tflow_splits[[i]]))
## }
