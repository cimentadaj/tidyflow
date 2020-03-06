test_that("can add a recipe to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x))

  expect_is(tidyflow$pre$actions$recipe, "action_recipe")

  rcp_fun <- function(x) recipes::recipe(mpg ~ cyl, x)
  tidyflow <- replace_recipe(tidyflow, rcp_fun)
  expect_is(tidyflow$pre$actions$recipe, "action_recipe")

})

test_that("recipe is validated", {
  expect_error(plug_recipe(tidyflow(), 1),
               "`.f` must be a function with a recipe for applying to the dataset") #nolintr
})

test_that("cannot add a recipe if a formula already exists", {
  tidyflow <- tidyflow()
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  expect_error(plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x)),
               "cannot be added when a formula already exists")
})

test_that("Can add recipe after model fit and refit", {
  rcp <- ~ recipes::step_log(recipes::recipe(.x, mpg ~ cyl), cyl, base = 10)
  tflow <- tidyflow(mtcars, seed = 542)
  tflow <- plug_formula(tflow, mpg ~ cyl)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_no_rcp <- fit(tflow)
  tflow <- plug_recipe(drop_formula(mod1_no_rcp), rcp)
  mod2_rcp <- fit(tflow)

  expect_is(mod2_rcp$pre$results$recipe, "recipe")
  expect_true(mod2_rcp$trained)
})


test_that("recipe function must return a recipe object", {
  tidyflow <- tidyflow(mtcars)
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")


  rcp_fun <- function(x) {
    res <- recipes::recipe(mpg ~ cyl, data = x)
    class(res) <- "not_recipe"
    res
  }

  tidyflow <- plug_recipe(tidyflow, rcp_fun)
  tidyflow <- plug_model(tidyflow, lm_model)

  expect_error(
    fit(tidyflow),
    "The recipe function `.f` should return an object of class `recipe`"
  )
})

test_that("Can add recipe after model fit and refit", {
  rcp <- ~ recipes::step_log(recipes::recipe(.x, mpg ~ cyl), cyl, base = 10)
  tflow <- tidyflow(mtcars, seed = 542)
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_no_resample <- fit(tflow)
  resample_mod <- fit(plug_resample(mod1_no_resample, rsample::vfold_cv))
})

test_that("drop_recipe removes the action and the result", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow <- plug_recipe(tidyflow(mtcars),
                          ~ recipes::recipe(mpg ~ cyl, data = .x))
  
  tidyflow <- plug_model(tidyflow, lm_model)
  mod1 <- fit(tidyflow)
  tidyflow <- drop_recipe(tidyflow)

  # Both are null on dropped tidyflow
  expect_null(tidyflow$pre$results$recipe)
  expect_null(tidyflow$pre$actions$recipe)
  # Both are available on fitted tidyflow
  expect_is(mod1$pre$results$recipe, "recipe")
  expect_is(mod1$pre$actions$recipe[[1]], "function")
})


test_that("remove a recipe", {
  tidyflow_no_recipe <- tidyflow(mtcars)
  tidyflow_with_recipe <- plug_recipe(tidyflow_no_recipe,
                                     ~ recipes::recipe(mpg ~ cyl, .x)
                                     )
  
  tidyflow_removed_recipe <- drop_recipe(tidyflow_with_recipe)

  expect_equal(tidyflow_no_recipe$pre, tidyflow_removed_recipe$pre)
})

test_that("dropping a recipe and refitting gives same result", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_rcp <- fit(tflow)
  tflow <- drop_recipe(mod1_rcp)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl, .x))
  mod2_no_rcp <- fit(tflow)

  expect_equal(strip_elapsed(mod1_rcp),
               strip_elapsed(mod2_no_rcp))
})

test_that("remove a recipe after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow_no_recipe <- tidyflow(mtcars)
  tidyflow_no_recipe <- plug_model(tidyflow_no_recipe, lm_model)

  tidyflow_with_recipe  <- plug_recipe(tidyflow_no_recipe,
                                      ~ recipes::recipe(mpg ~ cyl, .x)
                                      )
  
  tidyflow_with_recipe <- fit(tidyflow_with_recipe)

  tidyflow_removed_recipe <- drop_recipe(tidyflow_with_recipe)

  expect_equal(tidyflow_no_recipe$pre, tidyflow_removed_recipe$pre)
})

test_that("update a recipe", {
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x))
  tidyflow <- replace_recipe(tidyflow, ~ recipes::recipe(mpg ~ disp, .x))

  expect_equal(tidyflow$pre$actions$recipe$recipe,
               rlang::as_function(~ recipes::recipe(mpg ~ disp, .x))
               )
})

test_that("update a recipe after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x))

  tidyflow <- fit(tidyflow)

  # Should clear fitted model
  tidyflow <- replace_recipe(tidyflow, ~ recipes::recipe(mpg ~ disp, .x))

  expect_equal(tidyflow$pre$actions$recipe$recipe,
               rlang::as_function(~ recipes::recipe(mpg ~ disp, .x)))

  expect_equal(tidyflow$fit$actions$model$spec, lm_model)
  expect_equal(tidyflow$fit$fit, NULL)
  expect_equal(tidyflow$data, tidyflow$pre$mold)
})


test_that("model fit works correctly after updating recipe", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x))

  tidyflow <- fit(tidyflow)

  # Should clear fitted model
  rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ disp, .x), disp, base = 10)
  tidyflow <- replace_recipe(tidyflow, rcp)
  tidyflow <- fit(tidyflow)

  expect_equal(tidyflow$pre$actions$recipe$recipe,
               rlang::as_function(rcp))

  prepped_data <- combine_outcome_preds(tidyflow$pre$mold)

  expect_equal(
    prepped_data$disp,
    log10(mtcars$disp)
  )
  
  log_step <- tidyflow$pre$mold$blueprint$recipe$steps[[1]]
  expect_true(recipes::is_trained(log_step))
})


test_that("recipe is prepped upon `fit()`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  rcp_fun <- function(.x) {
    recipes::step_center(recipes::recipe(mpg ~ cyl, .x), cyl)
  }

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_recipe(tidyflow, rcp_fun)
  tidyflow <- plug_model(tidyflow, mod)

  result <- fit(tidyflow)

  expect_equal(
    result$pre$mold$outcomes$mpg,
    mtcars$mpg
  )

  expect_equal(
    result$pre$mold$predictors$cyl,
    mtcars$cyl - mean(mtcars$cyl)
  )

  center_step <- result$pre$mold$blueprint$recipe$steps[[1]]

  expect_true(recipes::is_trained(center_step))
})

test_that("cannot add two recipe", {
  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x))

  expect_error(plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x)),
               "`recipe` action has already been added")
})

test_that("ignores further arguments in recipe function", {
  tidyflow <- tidyflow(mtcars)

  rcp_fun <- function(x) {
    recipes::recipe(mpg ~ cyl, x)
  }
  
  tidyflow <- plug_model(plug_recipe(tidyflow, rcp_fun),
                        parsnip::set_engine(parsnip::linear_reg(), "lm"))

  expect_error(fit(tidyflow, x = 5))
})

test_that("can pass a blueprint through to hardhat::mold()", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  blueprint <- hardhat::default_recipe_blueprint(intercept = TRUE)

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(mpg ~ cyl, .x),
                         blueprint = blueprint)

  tidyflow <- fit(tidyflow)

  expect_true("(Intercept)" %in% colnames(tidyflow$pre$mold$predictors))
  expect_equal(tidyflow$pre$actions$recipe$blueprint, blueprint)
  expect_true(tidyflow$pre$mold$blueprint$intercept)
})

test_that("can only use a 'recipe_blueprint' blueprint", {
  rec <- ~ recipes::recipe(mpg ~ cyl, .x)
  blueprint <- hardhat::default_formula_blueprint()

  tidyflow <- tidyflow(mtcars)

  expect_error(
    plug_recipe(tidyflow, rec, blueprint = blueprint),
    "must be a hardhat 'recipe_blueprint'"
  )
})

test_that("recipe is applied on training data", {
  tidyflow <- tidyflow(mtcars)
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .x), cyl, base = 10)
  tidyflow <- plug_recipe(tidyflow, rcp)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_split(tidyflow, rsample::initial_split)

  fit_mod <- fit(tidyflow)

  prepped_data <- combine_outcome_preds(fit_mod$pre$mold)
  
  expect_equal(
    nrow(prepped_data),
    # 75% of the whole data is 24 rows. Make sure it always matches
    # that.
    24
  )
  
})

