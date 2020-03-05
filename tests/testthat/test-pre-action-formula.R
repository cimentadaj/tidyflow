test_that("can add a formula to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  expect_is(tidyflow$pre$actions$formula, "action_formula")
})

test_that("formula is validated", {
  expect_error(plug_formula(tidyflow(), 1), "`formula` must be a formula")
})

test_that("cannot add a formula if a recipe already exists", {
  rec <- ~ recipes::recipe(mpg ~ cyl, .x)

  tidyflow <- tidyflow()
  tidyflow <- plug_recipe(tidyflow, rec)

  expect_error(
    plug_formula(tidyflow, mpg ~ cyl),
    "cannot be added when a recipe already exists"
  )

})

test_that("formula preprocessing is executed upon `fit()`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_formula(tidyflow, mpg ~ log(cyl))
  tidyflow <- plug_model(tidyflow, mod)

  result <- fit(tidyflow)

  expect_equal(
    result$pre$mold$outcomes$mpg,
    mtcars$mpg
  )

  expect_equal(
    result$pre$mold$predictors$`log(cyl)`,
    log(mtcars$cyl)
  )

})

test_that("cannot add two formulas", {
  tidyflow <- tidyflow()
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  expect_error(plug_formula(tidyflow, mpg ~ cyl),
               "`formula` action has already been added")
})

test_that("remove a formula", {
  tidyflow_no_formula <- tidyflow()
  tidyflow_with_formula <- plug_formula(tidyflow_no_formula, mpg ~ cyl)
  tidyflow_removed_formula <- drop_formula(tidyflow_with_formula)

  expect_equal(tidyflow_no_formula$pre, tidyflow_removed_formula$pre)
})

test_that("remove a formula after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow_no_formula <- tidyflow(mtcars)
  tidyflow_no_formula <- plug_model(tidyflow_no_formula, lm_model)

  tidyflow_with_formula  <- plug_formula(tidyflow_no_formula, mpg ~ cyl)
  tidyflow_with_formula <- fit(tidyflow_with_formula)

  tidyflow_removed_formula <- drop_formula(tidyflow_with_formula)

  expect_equal(tidyflow_no_formula$data, tidyflow_removed_formula$pre$mold)
  expect_equal(tidyflow_no_formula$pre, tidyflow_removed_formula$pre)
  expect_null(tidyflow_removed_formula$fit$fit)
})

test_that("update a formula", {
  tidyflow <- tidyflow()
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- replace_formula(tidyflow, mpg ~ disp)

  expect_equal(tidyflow$pre$actions$formula$formula, mpg ~ disp)
})

test_that("update a formula after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  tidyflow <- fit(tidyflow)

  # Should clear fitted model
  tidyflow <- replace_formula(tidyflow, mpg ~ disp)

  expect_equal(tidyflow$pre$actions$formula$formula, mpg ~ disp)

  expect_equal(tidyflow$fit$actions$model$spec, lm_model)
  expect_equal(tidyflow$data, tidyflow$pre$mold)
})

test_that("can pass a blueprint through to hardhat::mold()", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  blueprint <- hardhat::default_formula_blueprint(intercept = TRUE)

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl, blueprint = blueprint)

  tidyflow <- fit(tidyflow)

  expect_true("(Intercept)" %in% colnames(tidyflow$pre$mold$predictors))
  expect_equal(tidyflow$pre$actions$formula$blueprint, blueprint)
  expect_true(tidyflow$pre$mold$blueprint$intercept)
})

test_that("can only use a 'formula_blueprint' blueprint", {
  blueprint <- hardhat::default_recipe_blueprint()

  tidyflow <- tidyflow()

  expect_error(
    plug_formula(tidyflow, mpg ~ cyl, blueprint = blueprint),
    "must be a hardhat 'formula_blueprint'"
  )
})

test_that("Formula works with plug_split", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- plug_formula(tidyflow, mpg ~ log(cyl))
  tidyflow <- plug_model(tidyflow, mod)

  result <- fit(tidyflow)

  # This makes sure that the final model is fit
  # on the splitted sample
  expect_equal(
    result$pre$mold$outcomes$mpg,
    pull_tflow_fit(result)$fit$model[[1]]
  )

  expect_equal(
    result$pre$mold$predictors$`log(cyl)`,
    pull_tflow_fit(result)$fit$model[[2]]
  )
  
})
