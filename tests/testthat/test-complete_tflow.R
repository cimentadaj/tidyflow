test_that("complete_tflow fits model on correct data with/without split", {
  mod1 <- parsnip::set_engine(parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()), "glmnet")
  mod <- tidyflow(mtcars, seed = 52315)
  mod <- plug_recipe(mod, ~ recipes::step_ns(recipes::recipe(mpg ~ ., data = .), disp, deg_free = tune::tune()))
  mod <- plug_resample(mod, rsample::vfold_cv)
  mod <- plug_model(mod, mod1)
  mod <- plug_grid(mod, dials::grid_regular, levels = 2)
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
})
