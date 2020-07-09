mod <- tidyflow(mtcars, seed = 52315)
mod <- plug_resample(mod, rsample::vfold_cv, v = 2)

test_that("complete_tflow fits model on correct data with/without split", {
  mod1 <- parsnip::set_engine(parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()), "glmnet")
  mod <- plug_grid(mod, dials::grid_regular, levels = 2)
  mod <- plug_model(mod, mod1)
  mod <- plug_recipe(mod, ~ recipes::step_ns(recipes::recipe(mpg ~ ., data = .), disp, deg_free = tune::tune()))
  res <- fit(mod)

  # Fitting on all data
  final_res <- complete_tflow(res, metric = "rsq")
  # This should be equivalent to 32
  expect_true(nrow(mtcars) == pull_tflow_fit(final_res)$fit$nobs)

  # Fit on training data
  res <- fit(plug_split(mod, rsample::initial_split))
  final_res <- complete_tflow(res, metric = "rsq")

  # This should be 24 rows, the default percentage in initial_split
  expect_true(24 == pull_tflow_fit(final_res)$fit$nobs)

  # Should always switch the true for trained
  expect_true(final_res$trained)
})

test_that("complete_tflow raises error when completing model with resample", {
  mod1 <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  mod <- plug_formula(mod, mpg ~ .)
  mod <- plug_model(mod, mod1)
  mod <- fit(mod)

  expect_error(
    complete_tflow(mod),
    "`complete_tflow` cannot finalize a model with a resampling result. To finalize a model you need a tuning result. Did you want `plug_grid`?",
    fixed = TRUE
  )
})

# Fit a regularized regression through a grid search.
reg_mod <- parsnip::set_engine(parsnip::linear_reg(penalty = tune::tune(),
                                                   mixture = tune::tune()),
                               "glmnet")

tflow <- plug_resample(tidyflow(mtcars, seed = 23151), rsample::vfold_cv, v = 2)
tflow <- plug_model(plug_formula(tflow, mpg ~ .), reg_mod)
tflow <- fit(plug_grid(tflow, dials::grid_regular, levels = 10))

test_that("complete_tflow works exactly with select_best", {
  best_tune <- select_best(pull_tflow_fit_tuning(tflow), metric = "rmse")
  # Remove the config column JUST so you can compare the
  # tuning parameter values. This column is used in all other
  # settings.
  best_tune$.config <- NULL
  best_model <- complete_tflow(tflow, metric = "rmse")
  model_params <- lapply(pull_tflow_spec(best_model)$args, rlang::eval_tidy)
  expect_identical(model_params, as.list(best_tune))
})

test_that("complete_tflow works exactly with select_by_one_std_err", {
  # This is particularly useful for testing whether `complete_tflow`
  # support passing the `...` without NSE to select_by_one_std_err
  best_tune <- select_by_one_std_err(pull_tflow_fit_tuning(tflow),
                                     mixture,
                                     metric = "rmse")

  best_model <- complete_tflow(tflow,
                               mixture,
                               metric = "rmse",
                               method = "select_by_one_std_err")
  
  model_params <- lapply(pull_tflow_spec(best_model)$args, rlang::eval_tidy)
  expect_identical(model_params, as.list(best_tune[1:2]))
})

test_that("complete_tflow works exactly with select_by_pct_loss", {
  # This is particularly useful for testing whether `complete_tflow`
  # support passing the `...` without NSE to select_by_one_std_err
  best_tune <- select_by_pct_loss(pull_tflow_fit_tuning(tflow),
                                  penalty,
                                  metric = "rsq")

  best_model <- complete_tflow(tflow,
                               penalty,
                               metric = "rmse",
                               method = "select_by_pct_loss")
  
  model_params <- lapply(pull_tflow_spec(best_model)$args, rlang::eval_tidy)
  expect_identical(model_params, as.list(best_tune[1:2]))
})

test_that("best_params overrides method in complete_tflow", {
  # Extract best tune but add a random number to make sure that
  # complete_tflow uses this `best_params` over the best one
  best_tune <- select_best(pull_tflow_fit_tuning(tflow), metric = "rmse")
  
  # Remove .config just to be able to treat best_tune as a numeric df
  # and add 0.05 below. This is because .config is a character vector.
  best_tune$.config <- NULL
  
  best_tune <- best_tune + 0.05
  random_model <- complete_tflow(tflow, metric = "rmse", best_params = best_tune)
  model_params <- lapply(pull_tflow_spec(random_model)$args, rlang::eval_tidy)
  expect_identical(model_params, as.list(best_tune))
})
