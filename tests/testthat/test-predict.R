test_that("can predict from a tidyflow", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result <- predict(fit_tidyflow, mtcars)

  expect_is(result, "tbl_df")
  expect_equal(nrow(result), 32)
})

test_that("can predict from tidyflow + split", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  semi_mold <- fit_tidyflow$pre$mold
  mold <- combine_outcome_preds(semi_mold)

  result <- predict(fit_tidyflow, mold)

  expect_is(result, "tbl_df")
  expect_equal(nrow(result), 24)
})

test_that("tidyflow must have been `fit()` before prediction can be done", {
  expect_error(predict(tidyflow(), mtcars), "Tidyflow has not yet been trained")
})

test_that("formula preprocessing is done to the `new_data`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_formula(tidyflow, mpg ~ log(cyl))
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result1 <- predict(fit_tidyflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  tidyflow <- tidyflow(mtcars_with_log)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result2 <- predict(fit_tidyflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("formula preprocessing is done with split to the `new_data`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars, seed = 23141)
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- plug_formula(tidyflow, mpg ~ log(cyl))
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result1 <- predict(fit_tidyflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  tidyflow <- tidyflow(mtcars_with_log, seed = 23141)
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result2 <- predict(fit_tidyflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("recipe preprocessing is done to the `new_data`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_recipe(tidyflow, rec)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)
  result1 <- predict(fit_tidyflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  tidyflow <- tidyflow(mtcars_with_log)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result2 <- predict(fit_tidyflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("`new_data` must have all of the original predictors", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_recipe(tidyflow, rec)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  cars_no_cyl <- mtcars
  cars_no_cyl$cyl <- NULL

  expect_error(predict(fit_tidyflow, cars_no_cyl), "missing: 'cyl'")
})


test_that("predict without split and new_data raises error", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_recipe(tidyflow, rec)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)
  expect_error(predict(fit_tidyflow),
               'argument "new_data" is missing, with no default') #nolintr
})


test_that("predict without split but with new_data works", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_recipe(tidyflow, rec)
  tidyflow <- plug_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)
  res <- predict(fit_tidyflow, new_data = mtcars)
  expect_equal(nrow(res), 32)
})


test_that("blueprint will get passed on to hardhat::forge()", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  # Pass formula explicitly to keep `lm()` from auto-generating an intercept
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, mod, formula = mpg ~ . + 0)

  blueprint_no_intercept <- hardhat::default_formula_blueprint(intercept = FALSE)
  tidyflow_no_intercept <- plug_formula(tidyflow, mpg ~ hp + disp, blueprint = blueprint_no_intercept)
  fit_no_intercept <- fit(tidyflow_no_intercept)
  prediction_no_intercept <- predict(fit_no_intercept, mtcars)

  blueprint_with_intercept <- hardhat::default_formula_blueprint(intercept = TRUE)
  tidyflow_with_intercept <- plug_formula(tidyflow, mpg ~ hp + disp, blueprint = blueprint_with_intercept)
  fit_with_intercept <- fit(tidyflow_with_intercept)
  prediction_with_intercept <- predict(fit_with_intercept, mtcars)

  expect_false(fit_no_intercept$pre$mold$blueprint$intercept)
  expect_true(fit_with_intercept$pre$mold$blueprint$intercept)

  expect_false(identical(prediction_with_intercept, prediction_no_intercept))
})

test_that("predict raises error when model not fit/tuned", {
  rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, data = .), cyl, base = 10) #nolintr
  tflow <- tidyflow(mtcars)
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))
  fit_tflow <- fit(tflow)

  expect_error(
    predict(fit_tflow, new_data = mtcars),
    "You seem to have a model with tuning parameters or a resample but not a finalized model. Did you call complete_tflow()?" #nolintr
  )

  expect_error(
    predict(drop_resample(fit_tflow), new_data = mtcars),
    "Tidyflow has not yet been trained. Did you call fit()?"
  )

  ## TODO:
  ## When you implement complete_tflow() add a test that when tuning + complete_tflow result is expected
  ## Also, if you rename complete_tflow you need to replace that in the error message from predict.tidyflow

  expect_error(
    predict(fit(drop_resample(fit_tflow))),
    'argument "new_data" is missing, with no default'
  )

  res <- predict(fit(drop_resample(fit_tflow)),
                 new_data = pull_tflow_testing(fit_tflow, TRUE))

  expect_equal(nrow(res), 8)

  res <- predict(fit(drop_resample(fit_tflow)),
                 new_data = pull_tflow_training(fit_tflow, TRUE))

  expect_equal(nrow(res), 24)
})

test_that("predict_training/testing works as expected", {
  rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, data = .), cyl, base = 10) #nolintr
  tflow <- tidyflow(mtcars)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))
  fit_tflow <- fit(tflow)

  expect_error(
    predict_training(fit_tflow),
    "`predict_training` can only work when a split preprocessor has been specifid. Did you want `plug_split`?",
    fixed = TRUE
  )

  expect_error(
    predict_testing(fit_tflow),
    "`predict_testing` can only work when a split preprocessor has been specifid. Did you want `plug_split`?",
    fixed = TRUE
  )


  tflow <- plug_split(tflow, rsample::initial_split)
  res_tr <- predict_training(fit(tflow))
  expect_true(".pred" %in% names(res_tr))
  expect_s3_class(res_tr, "tbl_df")

  res_tst <- predict_training(fit(tflow))
  expect_true(".pred" %in% names(res_tst))
  expect_s3_class(res_tst, "tbl_df")

  tflow <- plug_resample(tflow, rsample::vfold_cv)
  res_tflow <- fit(tflow)

  expect_error(
    predict_training(res_tflow),
    "You seem to have a model with tuning parameters or a resample but not a finalized model. Did you call complete_tflow()?",
    fixed = TRUE
  )
})
