pkgname <- "tidyflow"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "tidyflow-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('tidyflow')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("complete_tflow")
### * complete_tflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: complete_tflow
### Title: Fit the best model from a tuning grid
### Aliases: complete_tflow

### ** Examples

## Not run: 
##D library(parsnip)
##D library(tune)
##D library(dials)
##D library(rsample)
##D 
##D # Fit a regularized regression through a grid search.
##D reg_mod <- set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet")
##D tuned_res <-
##D  mtcars %>%
##D   tidyflow() %>% 
##D   plug_resample(vfold_cv, v = 2) %>% 
##D   plug_formula(mpg ~ .) %>% 
##D   plug_model(reg_mod) %>%
##D   plug_grid(grid_regular, levels = 1) %>%
##D   fit()
##D 
##D # Finalize the best model and refit on the whole dataset
##D final_model <- complete_tflow(tuned_res, metric = "rmse")
##D 
##D # complete_tflow uses tune::select_best as the default method. However,
##D # tune::select_by_one_std_err and
##D # tune::select_by_pct_loss can be used. These need to specify the metric and
##D # the tuning value from which to sort the selection. For example:
##D final_model_stderr <- complete_tflow(tuned_res,
##D                                      metric = "rmse",
##D                                      method = "select_by_one_std_err",
##D                                      penalty)
##D 
##D # select_by_one_std_err finalizs the best model with the simplest tuning
##D # values within one standard deviation from most optimal
##D # combination. For more information on these methods, see
##D # ?select_best
##D 
##D # You can also specify the best parameters, in case you want
##D # to override the automatic extraction of the best fit. If you
##D # specify `best_params` it will override all other arguments
##D 
##D best_params <- select_best(pull_tflow_fit_tuning(tuned_res), metric = "rmse")
##D final_model_custom <- complete_tflow(tuned_res, best_params = best_params)
##D 
##D # To see the final tuning values, extract the model spec
##D pull_tflow_spec(final_model)
##D 
##D # To extract the final fitted model:
##D pull_tflow_fit(final_model)
##D 
##D # Since there was no `plug_split`, the final model is fitted
##D # entirely on the data (no training/testing). If you try to predict
##D # on either one, it will not work:
##D final_model %>%
##D   predict_training()
##D 
##D # Add a split step, fit again and then finalize the model
##D # to predict on the training set
##D tuned_split <-
##D   tuned_res %>%
##D   replace_grid(grid_regular) %>% 
##D   plug_split(initial_split) %>%
##D   fit()
##D 
##D tuned_split %>%
##D  complete_tflow(metric = "rmse") %>%
##D  predict_training()
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("complete_tflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("control_tidyflow")
### * control_tidyflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: control_tidyflow
### Title: Control object for a tidyflow
### Aliases: control_tidyflow

### ** Examples

## Not run: 
##D library(parsnip)
##D library(rsample)
##D library(tune)
##D library(tidyflow)
##D 
##D # Build tidyflow
##D tflow <-
##D   mtcars %>%
##D   tidyflow() %>%
##D   plug_split(initial_split) %>%
##D   plug_formula(mpg ~ .) %>%
##D   plug_resample(vfold_cv, v = 2) %>% 
##D   plug_model(set_engine(linear_reg(), "lm"))
##D 
##D # For each resample object, we want the predictions
##D ct <- control_tidyflow(control_resample = control_resamples(save_pred = TRUE,
##D                                                             verbose = TRUE))
##D 
##D # Specify the control object
##D fit_m <- fit(tflow, control = ct)
##D fit_m
##D 
##D # Extract the predictions
##D fit_m %>%
##D   pull_tflow_fit_tuning() %>%
##D   .[[".predictions"]]
##D 
##D # `control_resamples` is only used when there is a resample but not
##D # grid. When there is a resample and a grid, `control_grid` should be
##D # used.
##D ct <- control_tidyflow(control_grid = control_grid(verbose = TRUE,
##D                                                    save_pred = TRUE))
##D 
##D # Since there is no grid specification, this is ignored.
##D # No messages should be printed nor a new .predictions
##D # columns in the result
##D fit_m <- fit(tflow, control = ct)
##D fit_m
##D 
##D # control_parsnip controls the options of the model
##D # For example, verbosity controls the messags of the model
##D ct <- control_tidyflow(control_parsnip = control_parsnip(verbosity = 2))
##D 
##D # Run a regularized regression with only one independent variable.
##D # This is not possible, it will raise an error and we will see it
##D # because of verbosity
##D res <-
##D   tflow %>%
##D   replace_model(set_engine(linear_reg(penalty = 0, mixture = 1), "glmnet")) %>%
##D   replace_formula(mpg ~ cyl) %>% 
##D   fit(control = ct)
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("control_tidyflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("fit-tidyflow")
### * fit-tidyflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: fit-tidyflow
### Title: Fit a tidyflow object
### Aliases: fit-tidyflow fit.tidyflow

### ** Examples

## Not run: 
##D library(parsnip)
##D library(recipes)
##D library(tune)
##D library(dials)
##D library(rsample)
##D 
##D # Fit a simple linear model
##D model <- set_engine(linear_reg(), "lm")
##D 
##D formula_tidyflow <-
##D  mtcars %>%
##D  tidyflow() %>%
##D  plug_formula(mpg ~ cyl + log(disp)) %>%
##D  plug_model(model)
##D 
##D # The result is a model since we didn't specify any resample/grid
##D res <- fit(formula_tidyflow)
##D 
##D # You can extract the model fit if neede
##D res %>%
##D  pull_tflow_fit()
##D 
##D # Alternatively, we can add a split specification and
##D # predict on the training data automatically:
##D formula_tidyflow <-
##D  formula_tidyflow %>%
##D  plug_split(initial_split)
##D 
##D res2 <- fit(formula_tidyflow)
##D 
##D res2 %>%
##D  predict_training()
##D 
##D # This has the advantage that `predict_training` or `predict_testing` will
##D # apply the recipe/formula automatically for you:
##D 
##D recipe_tidyflow <-
##D  formula_tidyflow %>%
##D  drop_formula() %>% 
##D  plug_recipe(~ recipe(mpg ~ ., .x) %>% step_log(disp))
##D 
##D res3 <- fit(recipe_tidyflow)
##D res3 %>%
##D  predict_testing()
##D 
##D # We can accumulate steps and add a cross-validation and tuning grid.
##D # Fit a regularized regression through a grid search.
##D # Do this by updating the already defined model:
##D new_mod <- set_engine(linear_reg(penalty = tune(), mixture = tune()),
##D                       "glmnet")
##D tuned_res <-
##D   recipe_tidyflow %>%
##D   plug_resample(vfold_cv, v = 2) %>% 
##D   replace_model(new_mod) %>%
##D   plug_grid(grid_regular, levels = 2) %>%
##D   fit()
##D 
##D # Since we specified a resample/grid, the result is now a `tidyflow`
##D # with a resample object
##D tuned_res
##D 
##D # If needed, we can extract that resample:
##D tuned_res %>%
##D  pull_tflow_fit_tuning() %>%
##D  autoplot()
##D 
##D # When the model tuning is finished, `complete_tflow` can
##D # finalize the model with the best model. It can pick
##D # the best model for you.
##D 
##D tuned_res %>%
##D  complete_tflow(metric = "rmse") %>%
##D  predict_training()
##D 
##D # `complete_tflow` is powerful as it already applied the recipe
##D # and retrained the model on the entire training data with
##D # the best tuning parameter from the tuning grid.
##D 
##D # The power of this model building is that you can replace any step
##D # and rerun the fit:
##D bootstrap_res <-
##D  tuned_res %>%
##D  replace_resample(bootstraps, times = 2) %>%
##D  fit()
##D 
##D bootstrap_res %>%
##D  complete_tflow(metric = "rsq") %>%
##D  predict_training()
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("fit-tidyflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("parameters-tidyflow")
### * parameters-tidyflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: parameters-tidyflow
### Title: Extract the parameters of a tidyflow
### Aliases: parameters-tidyflow parameters

### ** Examples

library(rsample)
library(tune)
library(dials)
library(recipes)
library(parsnip)

tflow <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_formula(mpg ~ .) %>% 
  plug_resample(vfold_cv) %>%
  plug_grid(grid_regular) %>%
  plug_model(set_engine(linear_reg(), "lm"))

# No tuning parameters
tidyflow::parameters(tflow)

# But if we add tuning parameters, we can check which ones:
tflow %>%
  drop_formula() %>% 
  plug_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
  tidyflow::parameters()

# parameters extracts both the tuning parameters from the recipe and
# model:
tflow <-
  tflow %>%
  drop_formula() %>% 
  plug_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
  replace_model(set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet"))

tidyflow::parameters(tflow)

# This can serve well to refresh your memory on which tuning
# parameters are present and then override the custom values
# in `plug_grid`:
## Not run: 
##D   res <-
##D     tflow %>%
##D     replace_grid(grid_regular, penalty = penalty(c(-1, 0))) %>%
##D     fit()
##D   res
## End(Not run)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("parameters-tidyflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plug_data")
### * plug_data

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plug_data
### Title: Add a data to a tidyflow
### Aliases: plug_data drop_data replace_data

### ** Examples


wf <- tidyflow()
wf <- plug_data(wf, mtcars)
wf

drop_data(wf)

replace_data(wf, iris)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plug_data", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plug_formula")
### * plug_formula

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plug_formula
### Title: Add formula terms to a tidyflow
### Aliases: plug_formula drop_formula replace_formula

### ** Examples


# Just for the pipe: %>%
library(tibble)

tflow <-
  mtcars %>%
  tidyflow(seed = 652341) %>% 
  plug_formula(mpg ~ .)

tflow

drop_formula(tflow)

replace_formula(tflow, mpg ~ disp)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plug_formula", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plug_grid")
### * plug_grid

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plug_grid
### Title: Add a grid specification to a tidyflow
### Aliases: plug_grid drop_grid replace_grid

### ** Examples


## Not run: 
##D library(parsnip)
##D library(rsample)
##D library(tune)
##D library(dials)
##D library(recipes)
##D 
##D # Grid search:
##D # No need to define the values of the tuning parameters
##D # as they have defaults. For example, see the output of dials::penalty()
##D 
##D # `plug_grid` defines the grid. You can pass all of the arguments of
##D # `grid_regular`:
##D mod <-
##D   mtcars %>%
##D   tidyflow() %>%
##D   plug_split(initial_split) %>%
##D   plug_formula(mpg ~ .) %>% 
##D   plug_resample(vfold_cv) %>%
##D   plug_model(set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet")) %>%
##D   plug_grid(grid_regular, levels = 5)
##D 
##D res <- fit(mod)
##D 
##D # See the grid that was generated after the fit:
##D res %>%
##D   pull_tflow_grid()
##D 
##D # The argument `levels = 5` tells it to generate 5 x 5 combination
##D # of all possible vaues. That's why you have 25 rows.
##D 
##D # You can extract the result from `plug_grid` with `pull_tflow_fit_tuning`:
##D pull_tflow_fit_tuning(res)
##D 
##D # Visualize it:
##D pull_tflow_fit_tuning(res) %>%
##D  autoplot()
##D 
##D # And explore it:
##D 
##D pull_tflow_fit_tuning(res) %>%
##D  collect_metrics()
##D 
##D # If you want to specify tuning values, you can do so with
##D # `plug_grid` or `replace_grid` but they must have the same
##D # name as the tuning parameter
##D res2 <-
##D   mod %>%
##D   replace_grid(grid_regular, penalty = penalty(c(-1, 0)), levels = 2) %>%
##D   fit()
##D 
##D res2 %>%
##D   pull_tflow_fit_tuning() %>%
##D   show_best("rsq")
##D 
##D # If tune assigns a name, then `plug_grid` or `replace_grid` must
##D # use that name to replace it
##D model <-
##D   set_engine(
##D     linear_reg(penalty = tune("my_penalty"), mixture = tune("my_mixture")),
##D     "glmnet"
##D   )
##D 
##D # You must use `my_penalty`
##D res3 <-
##D   mod %>%
##D   replace_model(model) %>%   
##D   replace_grid(grid_regular, my_penalty = penalty(c(-1, 0)), levels = 2) %>%
##D   fit()
##D 
##D res3 %>%
##D   pull_tflow_fit_tuning() %>%
##D   show_best("rsq")
##D 
##D # If you want to create a grid of all possible combination of the tuning
##D # parameters, you must use only `expand.grid` and name every single
##D # model parameter:
##D res4 <-
##D  mod %>%
##D  replace_grid(expand.grid,
##D               penalty = seq(0.01, 0.02, 0.005),
##D               mixture = c(0, 0.5, 1)) %>%
##D  fit()
##D 
##D # The resulting grid is all of the possible combinations
##D # from the values defined above:
##D res4 %>%
##D  pull_tflow_grid()
##D 
##D # See how they values are not random, but rather
##D # all combination of the supplied values
##D res4 %>%
##D  pull_tflow_fit_tuning() %>%
##D  collect_metrics()
##D 
##D # You can also tune values from a recipe directly
##D res5 <-
##D   res3 %>%
##D   drop_formula() %>% 
##D   plug_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
##D   fit()
##D 
##D res5 %>%
##D   pull_tflow_fit_tuning() %>%
##D   show_best("rsq")
## End(Not run)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plug_grid", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plug_model")
### * plug_model

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plug_model
### Title: Add a model to a tidyflow
### Aliases: plug_model drop_model replace_model

### ** Examples

library(parsnip)

# Define two competing model:
lm_model <- set_engine(linear_reg(), "lm")
regularized_model <- set_engine(lm_model, "glmnet")

# Define a minimal tidyflow: data + formula + model
wf <-
  mtcars %>%
  tidyflow() %>%
  plug_formula(mpg ~ .) %>% 
  plug_model(lm_model)

wf

# We can drop the model at any time and the remaining steps
# are intact
drop_model(wf)

# We can fit the model with `fit`:
fitted <- fit(wf)

# Extract the model if needed:
fitted %>%
  pull_tflow_fit()

# If we remove the model from the fitted `tidyflow`,
# the fit is dropped:
drop_model(fitted)

# We could replace the model from the initial tidyflow with
# the regularized model with `replace_model`

## TODO: when https://github.com/cimentadaj/tidyflow/issues/4 is fixed
## replace wf with fitted here.

reg_fitted <-
  wf %>%
  replace_model(regularized_model) %>%
  fit()

reg_fitted %>%
  pull_tflow_fit()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plug_model", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plug_recipe")
### * plug_recipe

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plug_recipe
### Title: Add a recipe to a tidyflow
### Aliases: plug_recipe drop_recipe replace_recipe

### ** Examples

library(recipes)
library(parsnip)

# Passing a function to `plug_recipe`
recipe_fun <- function(.x) {
  recipe(mpg ~ ., data = .x) %>%
   step_center(all_predictors()) %>%
   step_scale(all_predictors())
}

# Let's make sure that it works with the data first
recipe_fun(mtcars)

# Specify the function to be applied to the data in `plug_recipe`
tflow <-
 mtcars %>%
 tidyflow() %>%
 plug_recipe(recipe_fun) %>%
 plug_model(set_engine(linear_reg(), "lm"))

# Fit the model
fit(tflow)

# Specify a formula of a recipe. Remove the old one and specify one on the
# fly:
tflow %>%
 replace_recipe(~ recipe(mpg ~ cyl, data = .) %>% step_log(cyl, base = 10)) %>%
 fit()

# Note how the function argument can be either `.` or `.x`
tflow %>%
 replace_recipe(~ {
  .x %>% 
   recipe(mpg ~ cyl + am) %>%
    step_log(cyl, base = 10) %>%
    step_mutate(am = factor(am)) %>%
    step_dummy(am)
 }) %>%
 fit()




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plug_recipe", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plug_resample")
### * plug_resample

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plug_resample
### Title: Add a resample specification to a tidyflow
### Aliases: plug_resample drop_resample replace_resample

### ** Examples

library(tibble)
library(rsample)

wf <-
 mtcars %>%
 tidyflow() %>%
 plug_resample(vfold_cv, v = 5, strata = "cyl")

wf

# Strata as unquoted name
wf <- replace_resample(wf, initial_split, v = 5, strata = cyl)

wf

drop_resample(wf)

# New split function
replace_resample(wf, bootstraps)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plug_resample", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("plug_split")
### * plug_split

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plug_split
### Title: Add a split specification to a tidyflow
### Aliases: plug_split drop_split replace_split

### ** Examples

library(tibble)
library(rsample)

wf <-
 mtcars %>%
 tidyflow() %>%
 plug_split(initial_split, prop = 0.8, strata = "cyl")

wf

# Strata as unquoted name
wf <- replace_split(wf, initial_split, prop = 0.8, strata = cyl)

wf

drop_split(wf)

# New split function
replace_split(wf, initial_time_split)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plug_split", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("predict-tidyflow")
### * predict-tidyflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: predict-tidyflow
### Title: Predict from a tidyflow
### Aliases: predict-tidyflow predict.tidyflow predict_training
###   predict_testing

### ** Examples

## Not run: 
##D library(parsnip)
##D library(recipes)
##D library(rsample)
##D library(dials)
##D library(tune)
##D 
##D model <- set_engine(linear_reg(), "lm")
##D 
##D tflow <-
##D  mtcars %>%
##D  tidyflow() %>%
##D  plug_split(initial_split) %>%
##D  plug_model(model) %>%
##D  plug_recipe(~ recipe(mpg ~ cyl + disp, .) %>% step_log(disp))
##D 
##D tflow <- fit(tflow)
##D 
##D # This will automatically `bake()` the recipe on `new_data`,
##D # applying the log step to `disp`, and then fit the regression.
##D predict(tflow, new_data = pull_tflow_testing(tflow))
##D 
##D # When a split has been specified through `plug_split`,
##D # predict_training/predict_testing automatically extract
##D # everything and applies the recip/formula:
##D predict_testing(tflow)
##D predict_training(tflow)
##D 
##D # When a grid search has been performed, the user needs to
##D # finalize the model through complete_tflow and then
##D # predict/predict_training/predict_testing will work.
##D res <-
##D  tflow %>%
##D  # Adds a grid search for the polynomials of qsec
##D  replace_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
##D  plug_resample(vfold_cv, v = 2) %>% 
##D  plug_grid(grid_regular, levels = 1) %>%
##D  fit()
##D 
##D # We can complete the tidyflow by fitting the best model
##D # based on the RMSE metric and then predict:
##D res %>%
##D  complete_tflow(metric = "rmse") %>%
##D  predict_training()
##D 
##D # In short, to be able to predict, you need to have either a single model
##D # or a finalized tuning grid with `complete_tflow`.
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("predict-tidyflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidyflow-extractors")
### * tidyflow-extractors

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidyflow-extractors
### Title: Extract elements of a tidyflow
### Aliases: tidyflow-extractors pull_tflow_rawdata pull_tflow_split
###   pull_tflow_training pull_tflow_testing pull_tflow_resample
###   pull_tflow_grid pull_tflow_preprocessor pull_tflow_prepped_recipe
###   pull_tflow_mold pull_tflow_spec pull_tflow_fit pull_tflow_fit_tuning

### ** Examples

library(parsnip)
library(recipes)
library(rsample)

model <- set_engine(linear_reg(), "lm")

recipe <- ~ recipe(.x, mpg ~ cyl + disp) %>% step_log(disp)

tflow <-
 mtcars %>%
 tidyflow() %>%
 plug_split(initial_split) %>%
 plug_model(model)

recipe_tflow <- plug_recipe(tflow, recipe)
formula_tflow <- plug_formula(tflow, mpg ~ cyl + log(disp))

fit_recipe_tflow <- fit(recipe_tflow)
fit_formula_tflow <- fit(formula_tflow)

pull_tflow_rawdata(fit_recipe_tflow)
pull_tflow_rawdata(fit_formula_tflow)

# The preprocessor is either the recipe function or a formula
pull_tflow_preprocessor(recipe_tflow)
pull_tflow_preprocessor(formula_tflow)

# The `spec` is the parsnip spec before it has been fit.
# The `fit` is the fit parsnip model.
pull_tflow_spec(fit_formula_tflow)
pull_tflow_fit(fit_formula_tflow)

# The mold is returned from `hardhat::mold()`, and contains the
# predictors, outcomes, and information about the preprocessing
# for use on new data at `predict()` time.
pull_tflow_mold(fit_recipe_tflow)

# The raw training and testing
pull_tflow_training(fit_recipe_tflow)
pull_tflow_testing(fit_recipe_tflow)

# Or with the preprocessor (recipe/formula) applied
pull_tflow_training(fit_recipe_tflow, prep = TRUE)
pull_tflow_testing(fit_recipe_tflow, prep = TRUE)

# A useful shortcut is to extract the prepped recipe from the tidyflow
pull_tflow_prepped_recipe(fit_recipe_tflow)

# That is identical to
identical(
  pull_tflow_mold(fit_recipe_tflow)$blueprint$recipe,
  pull_tflow_prepped_recipe(fit_recipe_tflow)
)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidyflow-extractors", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("tidyflow")
### * tidyflow

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: tidyflow
### Title: Create a tidyflow
### Aliases: tidyflow

### ** Examples

library(recipes)
library(rsample)
library(dials)
library(parsnip)
library(tune)

wflow <-
 mtcars %>%
 tidyflow(seed = 23113) %>%
 plug_recipe(~ recipe(mpg ~ cyl, .x) %>% step_log(cyl))

# tidyflow gives a prinout of the current specification
# in the order of execution:
wflow

# The minimum tidyflow is: data + recipe/formula + model
wflow <-
 wflow %>%
 plug_model(set_engine(linear_reg(), "lm"))

# The output shows that we have the data, the recipe and the model
wflow

# We can fit that model and we get a brief print out of the model:
fit(wflow)

# We can add further steps and the print out will tell you the
# workflow specification:
wflow <-
 wflow %>%
 plug_split(initial_split) %>%
 plug_resample(vfold_cv, v = 2) %>%
 plug_grid(grid_regular)

# The print out shows that we have a split/resample/grid
# now set correcly.
wflow




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("tidyflow", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
