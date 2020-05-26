mod <- tidyflow(mtcars, seed = 52315)
mod <- plug_resample(mod, rsample::vfold_cv, v = 2)

test_that("complete_tflow fits model on correct data with/without split", {
  mod1 <- parsnip::set_engine(parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()), "glmnet")
  mod <- plug_grid(mod, dials::grid_regular, levels = 2)
  mod <- plug_model(mod, mod1)
  mod <- plug_recipe(mod, ~ recipes::step_ns(recipes::recipe(mpg ~ ., data = .), disp, deg_free = tune::tune()))

  res <- fit(mod)

  # Fitting on all data
  final_res <-
    complete_tflow(res,
                   tune::select_best(pull_tflow_fit_tuning(res), metric = "rsq"))
  # This should be equivalent to 32
  expect_true(nrow(mtcars) == pull_tflow_fit(final_res)$fit$nobs)

  # Fit on training data
  res <- fit(plug_split(mod, rsample::initial_split))
  final_res <-
    complete_tflow(res,
                   tune::select_best(pull_tflow_fit_tuning(res), metric = "rsq"))

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


