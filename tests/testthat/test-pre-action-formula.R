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
    workflows::pull_workflow_mold(result$fit$fit$wflow)$outcomes$mpg,
    mtcars$mpg
  )

  expect_equal(
    workflows::pull_workflow_mold(result$fit$fit$wflow)$predictors$`log(cyl)`,
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

  expect_equal(pull_tflow_rawdata(tidyflow_no_formula),
               tidyflow_no_formula$pre$mold)

  expect_equal(tidyflow_no_formula$pre, tidyflow_removed_formula$pre)
  expect_error(pull_tflow_fit(tidyflow_removed_formula))
})

test_that("drop_formula removes the action and the result", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  tidyflow <- plug_formula(tidyflow(mtcars), mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, lm_model)
  mod1 <- fit(tidyflow)
  tidyflow <- drop_formula(tidyflow)

  # Both are null on dropped tidyflow
  expect_error(pull_tflow_preprocessor(tidyflow))
  expect_null(tidyflow$pre$actions$formula)

  # Both are available on fitted tidyflow
  test_mold <- function(x) {
    mold <- x$pre$mold
    expect_is(mold, "data.frame")
    expect_equal(nrow(mold), 32)
  }

  test_mold(mod1)
  expect_is(pull_tflow_preprocessor(mod1), "formula")
})

test_that("update a formula", {
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- replace_formula(tidyflow, mpg ~ disp)
  tidyflow <- plug_model(tidyflow, set_engine(linear_reg(), "lm"))
  tidyflow <- fit(tidyflow)

  expect_equal(pull_tflow_preprocessor(tidyflow), mpg ~ disp)
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

  expect_equal(pull_tflow_preprocessor(fit(tidyflow)), mpg ~ disp)

  expect_equal(pull_tflow_spec(tidyflow), lm_model)
  expect_equal(pull_tflow_rawdata(tidyflow),
               tidyflow$pre$mold)
})

test_that("can pass a blueprint through to hardhat::mold()", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  blueprint <- hardhat::default_formula_blueprint(intercept = TRUE)

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl, blueprint = blueprint)

  tidyflow <- fit(tidyflow)

  mold <- workflows::pull_workflow_mold(tidyflow$fit$fit$wflow)

  expect_true("(Intercept)" %in% colnames(mold$predictors))
  expect_equal(tidyflow$pre$results$blueprint, blueprint)
  expect_true(mold$blueprint$intercept)
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

  # This makes sure that the final model is fit on the splitted sample

  mold <- workflows::pull_workflow_mold(result$fit$fit$wflow)

  expect_equal(
    mold$outcomes$mpg,
    pull_tflow_fit(result)$fit$model[[1]]
  )

  expect_equal(
    mold$predictors$`log(cyl)`,
    pull_tflow_fit(result)$fit$model[[2]]
  )
})

test_that("Blueprints can overrid formula molds", {
  # Leave factor AS IS
  bp1 <- hardhat::default_formula_blueprint(intercept = TRUE, indicators = 'none')
  # Convert factor to N-1 columns. Default in lm
  bp2 <- hardhat::default_formula_blueprint(intercept = TRUE, indicators = 'traditional')
  # Convert factor into N columns
  bp3 <- hardhat::default_formula_blueprint(intercept = TRUE, indicators = 'one_hot')
  mod <- parsnip::set_engine(parsnip::linear_reg(), "lm")

  tflow <- tidyflow(iris)
  tflow <- plug_formula(tflow, Sepal.Length ~ Species, blueprint = bp1)
  tflow <- plug_model(tflow, mod)
  mold <- workflows::pull_workflow_mold(fit(tflow)$fit$fit$wflow)$predictors

  # Leaves factors as is
  expect_length(mold, 2)
  expect_true(all(c("(Intercept)", "Species") %in% names(mold)))

  # Forces N-1 columns
  tflow <- replace_formula(tflow, Sepal.Length ~ Species, blueprint = bp2)
  mold <- workflows::pull_workflow_mold(fit(tflow)$fit$fit$wflow)$predictors
  expect_length(mold, 3)
  nm <- c("(Intercept)", "Speciesversicolor", "Speciesvirginica")
  expect_true(all(nm %in% names(mold)))

  # Forces N columns (one_hot)
  tflow <- replace_formula(tflow, Sepal.Length ~ Species, blueprint = bp3)
  mold <- workflows::pull_workflow_mold(fit(tflow)$fit$fit$wflow)$predictors
  expect_length(mold, 4)
  nm <- c("(Intercept)", "Speciessetosa", "Speciesversicolor", "Speciesvirginica")
  expect_true(all(nm %in% names(mold)))

})
