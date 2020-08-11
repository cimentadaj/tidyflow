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

test_that("recipe preprocessing is done with predict_training/predict_testing", {
  check_predict <- function(fun) {
    mod <- parsnip::set_engine(parsnip::linear_reg(), "lm")
    rec <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .), cyl)
    tidyflow <- tidyflow(mtcars, seed = 23131)
    tidyflow <- plug_split(tidyflow, rsample::initial_split)
    tidyflow <- plug_recipe(tidyflow, rec)
    tidyflow <- plug_model(tidyflow, mod)
    fit_tidyflow <- fit(tidyflow)
    result1 <- fun(fit_tidyflow)

    # pre-log the data
    mtcars_with_log <- mtcars
    mtcars_with_log$cyl <- log(mtcars_with_log$cyl)
    tidyflow <- tidyflow(mtcars_with_log, seed = 23131)
    tidyflow <- plug_split(tidyflow, rsample::initial_split)
    tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
    tidyflow <- plug_model(tidyflow, mod)
    fit_tidyflow2 <- fit(tidyflow)
    result2 <- fun(fit_tidyflow)
    expect_equal(result1, result2)
  }

  check_predict(predict_training)
  check_predict(predict_testing)
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
  train <- data.frame(
    y = c(1L, 5L, 3L, 4L),
    x = factor(c("x", "y", "x", "y"))
  )

  test <- data.frame(
    x = factor(c("x", "y", "z"))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  bp1 <- hardhat::default_formula_blueprint(intercept = TRUE, allow_novel_levels = FALSE)
  bp2 <- hardhat::default_formula_blueprint(intercept = TRUE, allow_novel_levels = TRUE)

  tflow <- tidyflow(train, seed = 23151)
  tflow <- plug_model(tflow, spec)

  tflow1 <- plug_formula(tflow, y ~ x, blueprint = bp1)
  tflow2 <- plug_formula(tflow, y ~ x, blueprint = bp2)

  mod1 <- fit(tflow1)
  mod2 <- fit(tflow2)

  expect_warning(pred1 <- predict(mod1, test))
  expect_warning(pred2 <- predict(mod2, test), NA)

  expect_identical(
    pred1[[".pred"]],
    c(2, 4.5, NA)
  )

  expect_identical(
    pred2[[".pred"]],
    c(2, 4.5, 2)
  )
})

rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, data = .), cyl, base = 10) #nolintr
tflow <- tidyflow(mtcars)
tflow <- plug_split(tflow, rsample::initial_split)
tflow <- plug_recipe(tflow, rcp)
tflow <- plug_resample(tflow, rsample::vfold_cv, v = 2)
tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))
fit_tflow <- fit(tflow)

test_that("predict raises error when model not fit/tuned", {

  expect_error(
    predict(fit_tflow, new_data = mtcars),
    "You seem to have a model with tuning parameters or a resample but not a finalized model. Did you call complete_tflow()?" #nolintr
  )

  expect_error(
    predict(drop_resample(fit_tflow), new_data = mtcars),
    "Tidyflow has not yet been trained. Did you call fit()?"
  )

  expect_error(
    predict(fit(drop_resample(fit_tflow))),
    'argument "new_data" is missing, with no default'
  )

  res <- predict(fit(drop_resample(fit_tflow)),
                 new_data = pull_tflow_testing(fit_tflow))

  expect_equal(nrow(res), 8)

  res <- predict(fit(drop_resample(fit_tflow)),
                 new_data = pull_tflow_training(fit_tflow))

  expect_equal(nrow(res), 24)
})

test_that("Tuning + complete_tflow works just as well with predict", {
  rcp <- ~ recipes::step_ns(recipes::recipe(mpg ~ ., data = .), hp, deg_free = tune::tune())
  tflow <- replace_recipe(fit_tflow, rcp) #nolintr
  fit_tune <- fit(plug_grid(tflow, dials::grid_regular, levels = 2))
  finalized_mod <- complete_tflow(fit_tune, metric = "rmse")

  res <- predict(finalized_mod, new_data = pull_tflow_testing(finalized_mod))

  expect_equal(nrow(res), 8)
  expect_identical(predict_testing(finalized_mod)[".pred"], res)

  res <- predict(finalized_mod,
                 new_data = pull_tflow_training(finalized_mod))

  expect_equal(nrow(res), 24)
  expect_identical(predict_training(finalized_mod)[".pred"], res)
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
