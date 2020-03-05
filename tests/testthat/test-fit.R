test_that("can `fit()` a tidyflow with a recipe", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tflow <-
    mtcars %>%
    tidyflow() %>%
    plug_recipe(~ recipes::recipe(mpg ~ cyl, .x)) %>%
    plug_model(mod)

  result <- fit(tflow)

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
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

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
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

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
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, mod)

  expect_error(
    fit(tidyflow),
    "`data` must be specified to fit a tidyflow; Do you need `plug_data`?"
  )
})

test_that("cannot fit without a pre stage", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, mod)

  expect_error(fit(tidyflow), "must have a formula or recipe")
})

test_that("cannot fit without a fit stage", {
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  expect_error(fit(tidyflow), "must have a model")
})

