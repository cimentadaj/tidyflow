test_that("can add a model to a tidyflow", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow()
  tidyflow <- plug_model(tidyflow, mod)

  expect_is(tidyflow$fit$actions$model, "action_model")
})

test_that("model is validated", {
  expect_error(plug_model(tidyflow(), 1), "`spec` must be a `model_spec`")
})

test_that("cannot add two models", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tidyflow <- tidyflow()
  tidyflow <- plug_model(tidyflow, mod)

  expect_error(plug_model(tidyflow, mod), "`model` action has already been added")
})

test_that("can provide a model formula override", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  tflow <-
    mtcars %>%
    tidyflow() %>%
    # disp is in the recipe, but excluded from the model formula
    plug_recipe(~ {
      .x %>%
        recipes::recipe(mpg ~ cyl + disp) %>%
        recipes::step_center(cyl)
    }) %>%
    plug_model(mod, formula = mpg ~ cyl)

  result <- fit(tflow)

  expect_equal(
    c("(Intercept)", "cyl"),
    names(pull_tflow_fit(result)$fit$coefficients)
  )
})


test_that("remove a model", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow_no_model <- tidyflow()
  tidyflow_no_model <- plug_formula(tidyflow_no_model, mpg ~ cyl)

  tidyflow_with_model  <- plug_model(tidyflow_no_model, lm_model)
  tidyflow_removed_model  <- drop_model(tidyflow_with_model)

  expect_equal(tidyflow_no_model$fit, tidyflow_removed_model$fit)
})

test_that("remove a model after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow_no_model <- tidyflow(mtcars)
  tidyflow_no_model <- plug_formula(tidyflow_no_model, mpg ~ cyl)

  tidyflow_with_model  <- plug_model(tidyflow_no_model, lm_model)
  tidyflow_with_model <- fit(tidyflow_with_model)

  tidyflow_removed_model  <- drop_model(tidyflow_with_model)

  expect_equal(tidyflow_no_model$fit, tidyflow_removed_model$fit)
  # The removed tidyflow still keeps the original mold
  expect_false(identical(tidyflow_removed_model$data,
                         tidyflow_removed_model$pre$mold))
})

test_that("remove a model after model fit keeps all results/actions intact", {
  # Check it with a recipe
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  tflow <- tidyflow(mtcars)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(.x, mpg ~ cyl))
  tflow_nofit  <- plug_model(tflow, lm_model)
  tflow_fit <- fit(tflow_nofit)
  tflow_drop  <- drop_model(tflow_fit)

  # Make sure data and pre are exactly the same in tflow without
  # model and with model
  expect_equal(tflow_fit[c("data", "pre")], tflow_drop[c("data", "pre")])

  # Now adds a split
  tflow_nofit <- plug_split(tflow_nofit, rsample::initial_split)
  tflow_fit <- fit(tflow_nofit)
  tflow_drop  <- drop_model(tflow_fit)
  expect_equal(tflow_fit[c("data", "pre")], tflow_drop[c("data", "pre")])

  # Now adds a resample
  tflow_nofit <- plug_resample(tflow_nofit, rsample::vfold_cv)
  tflow_fit <- fit(tflow_nofit)
  tflow_drop  <- drop_model(tflow_fit)
  expect_equal(rsplit2df(tflow_fit[c("data", "pre")]),
               rsplit2df(tflow_drop[c("data", "pre")]))

  # Removes the recipe and adds the formula
  tflow_nofit <- drop_recipe(tflow_nofit)
  tflow_nofit <- plug_formula(tflow_nofit, mpg ~ cyl)
  tflow_fit <- fit(tflow_nofit)
  tflow_drop  <- drop_model(tflow_fit)
  expect_equal(rsplit2df(tflow_fit[c("data", "pre")]),
               rsplit2df(tflow_drop[c("data", "pre")]))
})

test_that("update a model", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  glmn_model <- parsnip::set_engine(lm_model, "glmnet")

  tidyflow <- tidyflow()
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- replace_model(tidyflow, glmn_model)

  expect_equal(pull_tflow_spec(tidyflow)$engine, "glmnet")
})


test_that("update a model after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  no_model <- parsnip::set_engine(lm_model, "lm", model = FALSE)

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, no_model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  tidyflow <- fit(tidyflow)
  tidyflow <- replace_model(tidyflow, lm_model)

  # Should no longer have `model = FALSE` engine arg
  engine_args <- pull_tflow_spec(tidyflow)$eng_args
  expect_false(any(names(engine_args) == "model"))

  # The fitted model should be removed
  expect_error(pull_tflow_fit(tidyflow),
               "The tidyflow does not have a model fit. Have you called `fit[(][)]` yet?") #nolintr
})

test_that("If tune is specified in model, grid needs to be specified", {
  mod1 <-
    parsnip::set_engine(
      parsnip::linear_reg(penalty = tune::tune()),
      "glmnet"
    )

  tflow <-
    plug_formula(plug_split(tidyflow(mtcars), rsample::initial_split),
                 mpg ~ .)
  
  tflow <- plug_model(tflow, mod1)

  expect_error(
    fit(tflow),
    "The model contains parameters with `tune()` but no grid specification has been made. Did you want `plug_grid`?", #nolintr
    fixed = TRUE
  )

})

test_that("tune() parameters which need finalize are completed on the run", {
  # mtry is estimated from data as the number of predictors. If
  # you run tune_grid with a model with tune and haven't finalized
  # mtry, it will raise an error.
  rf_model <- parsnip::rand_forest(mode = "regression", mtry = tune::tune())
  rf_model <- parsnip::set_engine(rf_model, "randomForest")
  tflow <- tidyflow(mtcars)
  tflow <- plug_formula(tflow, mpg ~ .)
  tflow  <- plug_model(tflow, rf_model)
  tflow  <- plug_resample(tflow, rsample::vfold_cv, v = 2)
  tflow  <- plug_grid(tflow, dials::grid_regular, levels = 2)

  # Just check that it runs successfully
  tflow <- fit(tflow)
  expect_s3_class(tflow, "tidyflow")

  # Check that the mtry values are always capped at 10,
  # since the number of predictors are 10.
  expect_equal(c(1, 10), pull_tflow_grid(tflow)$mtry)

})
