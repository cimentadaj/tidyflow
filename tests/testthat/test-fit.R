test_that("can `fit()` a tidyflow with a recipe", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tflow <-
    mtcars %>%
    tidyflow() %>%
    plug_recipe(~ recipes::recipe(mpg ~ cyl, .x)) %>%
    plug_model(mod)

  result <- fit(tflow)
  expect_is(pull_tflow_fit(result), "model_fit")

  expect_equal(
    coef(pull_tflow_fit(result)$fit),
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

  expect_is(pull_tflow_fit(result), "model_fit")

  expect_equal(
    coef(pull_tflow_fit(result)$fit),
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

  expect_is(pull_tflow_fit(result), "model_fit")

  semi_mold <- result$pre$mold
  converted_mold <- combine_outcome_preds(semi_mold)
  
  expect_equal(
    coef(pull_tflow_fit(result)$fit),
    coef(lm(formula = mpg ~ cyl, data = converted_mold))
  )

  expect_equal(
    nobs(pull_tflow_fit(result)$fit),
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


test_that("can `fit()` regardless of order", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")
  rcp <- ~ recipes::recipe(.x, mpg ~ cyl)

  # 'Correct order'
  tflow <- tidyflow(mtcars, seed = 23151)
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_resample(tflow, rsample::vfold_cv)  
  tflow <- plug_model(tflow, mod)
  result <- fit(tflow)

  resample_class <- c("resample_results", "tune_results", "tbl_df", "tbl", "data.frame")
  expect_is(pull_tflow_fit_tuning(result), resample_class)

  # 'Random order'
  tflow2 <- tidyflow(seed = 23151)
  tflow2 <- plug_model(tflow2, mod)
  tflow2 <- plug_resample(tflow2, rsample::vfold_cv)  
  tflow2 <- plug_recipe(tflow2, rcp)
  tflow2 <- plug_split(tflow2, rsample::initial_split)
  tflow2 <- plug_data(tflow2, mtcars)
  result2 <- fit(tflow2)

  expect_is(pull_tflow_fit_tuning(result2), resample_class)

  expect_equal(rsplit2df(result),
               rsplit2df(result2))
})

test_that("Can replace model and fit without error", {
  lm_model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  regularized_model <- parsnip::set_engine(parsnip::linear_reg(), "glmnet")
  tflow <- plug_formula(tidyflow(mtcars), mpg ~ .)
  lm_mod <- fit(plug_model(tflow, lm_model))
  expect_type(fit(replace_model(lm_mod, regularized_model)), "list")
})
