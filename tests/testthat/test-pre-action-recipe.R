test_that("can add a recipe to a workflow", {
  workflow <- workflow()
  workflow <- add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x))

  expect_is(workflow$pre$actions$recipe, "action_recipe")

  rcp_fun <- function(x) recipes::recipe(mpg ~ cyl, x)
  workflow <- update_recipe(workflow, rcp_fun)
  expect_is(workflow$pre$actions$recipe, "action_recipe")

})

test_that("recipe is validated", {
  expect_error(add_recipe(workflow(), 1),
               "`.f` must be a function with a recipe for applying to the dataset") #nolintr
})

test_that("cannot add a recipe if a formula already exists", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_error(add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x)),
               "cannot be added when a formula already exists")
})

test_that("recipe function must return a recipe object", {
  workflow <- workflow(mtcars)
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")


  rcp_fun <- function(x) {
    res <- recipes::recipe(mpg ~ cyl, data = x)
    class(res) <- "not_recipe"
    res
  }

  workflow <- add_recipe(workflow, rcp_fun)
  workflow <- add_model(workflow, lm_model)

  expect_error(
    fit(workflow),
    "The recipe function `.f` should return an object of class `recipe`"
  )
})

test_that("remove a recipe", {
  workflow_no_recipe <- workflow(mtcars)
  workflow_with_recipe <- add_recipe(workflow_no_recipe,
                                     ~ recipes::recipe(mpg ~ cyl, .x)
                                     )
  
  workflow_removed_recipe <- remove_recipe(workflow_with_recipe)

  expect_equal(workflow_no_recipe$pre, workflow_removed_recipe$pre)
})

test_that("remove a recipe after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow_no_recipe <- workflow(mtcars)
  workflow_no_recipe <- add_model(workflow_no_recipe, lm_model)

  workflow_with_recipe  <- add_recipe(workflow_no_recipe,
                                      ~ recipes::recipe(mpg ~ cyl, .x)
                                      )
  
  workflow_with_recipe <- fit(workflow_with_recipe)

  workflow_removed_recipe <- remove_recipe(workflow_with_recipe)

  expect_equal(workflow_no_recipe$pre, workflow_removed_recipe$pre)
})

test_that("update a recipe", {
  workflow <- workflow(mtcars)
  workflow <- add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x))
  workflow <- update_recipe(workflow, ~ recipes::recipe(mpg ~ disp, .x))

  expect_equal(workflow$pre$actions$recipe$recipe,
               rlang::as_function(~ recipes::recipe(mpg ~ disp, .x))
               )
})

test_that("update a recipe after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, lm_model)
  workflow <- add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x))

  workflow <- fit(workflow)

  # Should clear fitted model
  workflow <- update_recipe(workflow, ~ recipes::recipe(mpg ~ disp, .x))

  expect_equal(workflow$pre$actions$recipe$recipe,
               rlang::as_function(~ recipes::recipe(mpg ~ disp, .x)))

  expect_equal(workflow$fit$actions$model$spec, lm_model)
  expect_equal(workflow$fit$fit, NULL)
  expect_equal(workflow$data, workflow$pre$mold)
})


test_that("model fit works correctly after updating recipe", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, lm_model)
  workflow <- add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x))

  workflow <- fit(workflow)

  # Should clear fitted model
  rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ disp, .x), disp, base = 10)
  workflow <- update_recipe(workflow, rcp)
  workflow <- fit(workflow)

  expect_equal(workflow$pre$actions$recipe$recipe,
               rlang::as_function(rcp))

  prepped_data <- combine_outcome_preds(workflow$pre$mold)

  expect_equal(
    prepped_data$disp,
    log10(mtcars$disp)
  )
  
  log_step <- workflow$pre$mold$blueprint$recipe$steps[[1]]
  expect_true(recipes::is_trained(log_step))
})


test_that("recipe is prepped upon `fit()`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  rcp_fun <- function(.x) {
    recipes::step_center(recipes::recipe(mpg ~ cyl, .x), cyl)
  }

  workflow <- workflow(mtcars)
  workflow <- add_recipe(workflow, rcp_fun)
  workflow <- add_model(workflow, mod)

  result <- fit(workflow)

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
  workflow <- workflow(mtcars)
  workflow <- add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x))

  expect_error(add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x)),
               "`recipe` action has already been added")
})

test_that("ignores further arguments in recipe function", {
  workflow <- workflow(mtcars)

  rcp_fun <- function(x) {
    recipes::recipe(mpg ~ cyl, x)
  }
  
  workflow <- add_model(add_recipe(workflow, rcp_fun),
                        parsnip::set_engine(parsnip::linear_reg(), "lm"))

  expect_error(fit(workflow, x = 5))
})

test_that("can pass a blueprint through to hardhat::mold()", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  blueprint <- hardhat::default_recipe_blueprint(intercept = TRUE)

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, lm_model)
  workflow <- add_recipe(workflow, ~ recipes::recipe(mpg ~ cyl, .x),
                         blueprint = blueprint)

  workflow <- fit(workflow)

  expect_true("(Intercept)" %in% colnames(workflow$pre$mold$predictors))
  expect_equal(workflow$pre$actions$recipe$blueprint, blueprint)
  expect_true(workflow$pre$mold$blueprint$intercept)
})

test_that("can only use a 'recipe_blueprint' blueprint", {
  rec <- ~ recipes::recipe(mpg ~ cyl, .x)
  blueprint <- hardhat::default_formula_blueprint()

  workflow <- workflow(mtcars)

  expect_error(
    add_recipe(workflow, rec, blueprint = blueprint),
    "must be a hardhat 'recipe_blueprint'"
  )
})

test_that("recipe is applied on training data", {
  workflow <- workflow(mtcars)
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, .x), cyl, base = 10)
  workflow <- add_recipe(workflow, rcp)
  workflow <- add_model(workflow, lm_model)
  workflow <- add_split(workflow, rsample::initial_split)

  fit_mod <- fit(workflow)

  prepped_data <- combine_outcome_preds(fit_mod$pre$mold)
  
  expect_equal(
    nrow(prepped_data),
    # 75% of the whole data is 24 rows. Make sure it always matches
    # that.
    24
  )
  
})

