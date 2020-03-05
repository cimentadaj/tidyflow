test_that("can `fit()` a tidyflow with a recipe", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  wflow <-
    mtcars %>%
    tidyflow() %>%
    add_recipe(~ recipes::recipe(mpg ~ cyl, .x)) %>%
    add_model(mod)

  result <- fit(wflow)

  expect_is(result$fit$fit, "model_fit")

  expect_equal(
    coef(result$fit$fit$fit),
    coef(lm(formula = mpg ~ cyl, data = mtcars))
  )
})

test_that("can `fit()` a tidyflow with a formula", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

  result <- fit(tidyflow)

  expect_is(result$fit$fit, "model_fit")

  expect_equal(
    coef(result$fit$fit$fit),
    coef(lm(formula = mpg ~ cyl, data = mtcars))
  )
})

test_that("can `fit()` a tidyflow + split + formula", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

  result <- fit(tidyflow)

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

  tidyflow <- tidyflow()
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, mod)

  expect_error(
    fit(tidyflow),
    "`data` must be specified to fit a tidyflow; Do you need `add_data`?"
  )
})

test_that("cannot fit without a pre stage", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_model(tidyflow, mod)

  expect_error(fit(tidyflow), "must have a formula or recipe")
})

test_that("cannot fit without a fit stage", {
  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)

  expect_error(fit(tidyflow), "must have a model")
})

