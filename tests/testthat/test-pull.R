# ------------------------------------------------------------------------------
# pull_workflow_preprocessor()

test_that("can pull a formula preprocessor", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_equal(
    pull_workflow_preprocessor(workflow),
    mpg ~ cyl
  )
})

test_that("can pull a recipe preprocessor", {
  recipe <- ~ recipes::recipe(mpg ~ cyl, .x)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_equal(
    pull_workflow_preprocessor(workflow),
    rlang::as_function(recipe)
  )
})

test_that("error if no preprocessor", {
  expect_error(
    pull_workflow_preprocessor(workflow()),
    "does not have a preprocessor"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_preprocessor(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_workflow_spec()

test_that("can pull a model spec", {
  model <- parsnip::linear_reg()

  workflow <- workflow()
  workflow <- add_model(workflow, model)

  expect_equal(
    pull_workflow_spec(workflow),
    model
  )
})

test_that("error if no spec", {
  expect_error(
    pull_workflow_spec(workflow()),
    "does not have a model spec"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_spec(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tflow_fit()

test_that("can pull a model fit", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow)

  expect_equal(
    pull_tflow_fit(workflow),
    workflow$fit$fit
  )
})

test_that("error if no fit", {
  expect_error(
    pull_tflow_fit(workflow()),
    "does not have a model fit. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_tflow_fit(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_workflow_mold()

test_that("can pull a mold", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow)

  expect_is(pull_workflow_mold(workflow), "list")

  expect_equal(
    pull_workflow_mold(workflow),
    workflow$pre$mold
  )
})

test_that("error if no mold", {
  expect_error(
    pull_workflow_mold(workflow()),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_mold(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_workflow_prepped_recipe()

test_that("can pull a prepped recipe", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  recipe <-
    ~ recipes::recipe(mpg ~ cyl, .x) %>%
      recipes::step_log(cyl)

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, model)
  workflow <- add_recipe(workflow, recipe)

  workflow <- fit(workflow)

  expect_is(pull_workflow_prepped_recipe(workflow), "recipe")

  expect_equal(
    pull_workflow_prepped_recipe(workflow),
    workflow$pre$mold$blueprint$recipe
  )
})

test_that("error if no recipe preprocessor", {
  expect_error(
    pull_workflow_prepped_recipe(workflow()),
    "must have a recipe preprocessor"
  )
})

test_that("error if no mold", {
  recipe <- ~ recipes::recipe(mpg ~ cyl, .x)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_error(
    pull_workflow_prepped_recipe(workflow),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_prepped_recipe(1),
    "must be a workflow"
  )
})

test_that("can pull the raw data", {
  workflow <- workflow(mtcars)

  expect_equal(
    pull_workflow_rawdata(workflow),
    mtcars
  )
})

test_that("error if no raw data", {
  expect_error(
    pull_workflow_rawdata(workflow()),
    "The workflow does not have data"
  )
})


check_testing <- function(x) {
  expect_equal(
    nrow(pull_workflow_testing(x)),
    8
  )

  expect_equal(
    ncol(pull_workflow_testing(x)),
    11
  )

}

test_that("can pull testing data", {
  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  workflow <- add_recipe(workflow(mtcars), ~ recipes::recipe(mpg ~ cyl, .))
  workflow <- add_model(workflow, model)

  res <- fit(workflow)

  expect_error(
    pull_workflow_testing(res),
    "The workflow must have a split preprocessor."
  )

  # Add split. The testing should have 8 rows (75%)
  res <- fit(add_split(res, rsample::initial_split))

  check_testing(res)

  expect_error(
    pull_workflow_testing(remove_recipe(res)),
    "Workflow has not yet been trained. Do you need to call `fit()`?"
  )

  res <- fit(add_formula(remove_recipe(res), mpg ~ cyl))
  # The data is refit the same way with a formula
  check_testing(res)

})
