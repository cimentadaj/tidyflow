test_that("can predict from a tidyflow", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result <- predict(fit_tidyflow, mtcars)

  expect_is(result, "tbl_df")
  expect_equal(nrow(result), 32)
})

test_that("can predict from tidyflow + split", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

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
  tidyflow <- add_formula(tidyflow, mpg ~ log(cyl))
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result1 <- predict(fit_tidyflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  tidyflow <- tidyflow(mtcars_with_log)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result2 <- predict(fit_tidyflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("formula preprocessing is done with split to the `new_data`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  set.seed(23141)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- add_formula(tidyflow, mpg ~ log(cyl))
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result1 <- predict(fit_tidyflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  set.seed(23141)
  tidyflow <- tidyflow(mtcars_with_log)
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result2 <- predict(fit_tidyflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("recipe preprocessing is done to the `new_data`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_recipe(tidyflow, rec)
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)
  result1 <- predict(fit_tidyflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  tidyflow <- tidyflow(mtcars_with_log)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)

  result2 <- predict(fit_tidyflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("`new_data` must have all of the original predictors", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_recipe(tidyflow, rec)
  tidyflow <- add_model(tidyflow, mod)

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
  tidyflow <- add_recipe(tidyflow, rec)
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)
  expect_error(predict(fit_tidyflow),
               'argument "new_data" is missing, with no default') #nolintr
})


test_that("predict without split but with new_data works", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_recipe(tidyflow, rec)
  tidyflow <- add_model(tidyflow, mod)

  fit_tidyflow <- fit(tidyflow)
  res <- predict(fit_tidyflow, new_data = mtcars)
  expect_equal(nrow(res), 32)
})


test_that("blueprint will get passed on to hardhat::forge()", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  # Pass formula explicitly to keep `lm()` from auto-generating an intercept
  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_model(tidyflow, mod, formula = mpg ~ . + 0)

  blueprint_no_intercept <- hardhat::default_formula_blueprint(intercept = FALSE)
  tidyflow_no_intercept <- add_formula(tidyflow, mpg ~ hp + disp, blueprint = blueprint_no_intercept)
  fit_no_intercept <- fit(tidyflow_no_intercept)
  prediction_no_intercept <- predict(fit_no_intercept, mtcars)

  blueprint_with_intercept <- hardhat::default_formula_blueprint(intercept = TRUE)
  tidyflow_with_intercept <- add_formula(tidyflow, mpg ~ hp + disp, blueprint = blueprint_with_intercept)
  fit_with_intercept <- fit(tidyflow_with_intercept)
  prediction_with_intercept <- predict(fit_with_intercept, mtcars)

  expect_false(fit_no_intercept$pre$mold$blueprint$intercept)
  expect_true(fit_with_intercept$pre$mold$blueprint$intercept)

  expect_false(identical(prediction_with_intercept, prediction_no_intercept))
})
