test_that("can `fit()` a workflow with a recipe", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  wflow <-
    mtcars %>%
    workflow() %>%
    add_recipe(~ recipes::recipe(mpg ~ cyl, .x)) %>%
    add_model(mod)

  result <- fit(wflow)

  expect_is(result$fit$fit, "model_fit")

  expect_equal(
    coef(result$fit$fit$fit),
    coef(lm(formula = mpg ~ cyl, data = mtcars))
  )
})

test_that("can `fit()` a workflow with a formula", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  result <- fit(workflow)

  expect_is(result$fit$fit, "model_fit")

  expect_equal(
    coef(result$fit$fit$fit),
    coef(lm(formula = mpg ~ cyl, data = mtcars))
  )
})

test_that("can `fit()` a workflow + split + formula", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  result <- fit(workflow)

  expect_is(result$fit$fit, "model_fit")

  semi_mold <- result$pre$mold
  converted_mold <- combine_outcome_preds(semi_mold)
  
  expect_equal(
    coef(result$fit$fit$fit),
    coef(lm(formula = mpg ~ cyl, data = converted_mold))
  )

  expect_equal(
    nobs(result$fit$fit$fit),
    nrow(converted_mold)
  )
  
})

test_that("cannot fit without a dataset", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  expect_error(
    fit(workflow),
    "`data` must be specified to fit a workflow; Do you need `add_data`?"
  )
})

test_that("cannot fit without a pre stage", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, mod)

  expect_error(fit(workflow), "must have a formula or recipe")
})

test_that("cannot fit without a fit stage", {
  workflow <- workflow(mtcars)
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_error(fit(workflow), "must have a model")
})

