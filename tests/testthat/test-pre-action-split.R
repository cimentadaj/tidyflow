test_that("can add a split to a workflow", {
  workflow <- workflow()
  workflow <- add_split(workflow, rsample::initial_split)

  expect_is(workflow$pre$actions$split, "action_split")
})

test_that("split is validated", {
  expect_error(add_split(workflow(), 1),
               "`.f` must be a function for splitting the dataset")
})

test_that("remove a split specification", {
  workflow_no_split <- workflow()
  workflow_with_split <- add_split(workflow_no_split, rsample::initial_split)
  workflow_removed_split <- remove_split(workflow_with_split)

  expect_equal(workflow_no_split$pre, workflow_removed_split$pre)
})


test_that("Saves testing data after split", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  workflow_with_split  <- add_split(workflow(mtcars), rsample::initial_split)
  workflow_with_split <- add_recipe(workflow_with_split, ~ recipes::recipe(mpg ~ cyl, .))
  workflow_with_split  <- add_model(workflow_with_split, lm_model)
  workflow_with_split <- fit(workflow_with_split)

  expect_true(!is.null(workflow_with_split$pre$actions$split$testing))
})


test_that("remove a split after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow_no_split <- add_formula(workflow(mtcars), mpg ~ cyl)
  workflow_no_split <- add_model(workflow_no_split, lm_model)

  workflow_with_split  <- add_split(workflow_no_split, rsample::initial_split)
  workflow_with_split <- fit(workflow_with_split)

  workflow_removed_split <- remove_split(workflow_with_split)

  expect_equal(workflow_no_split$pre, workflow_removed_split$pre)
})

test_that("update a split specification", {
  workflow <- workflow()
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- update_split(workflow, rsample::initial_time_split)

  expect_equal(workflow$pre$actions$split$`rsample::initial_time_split`,
               rsample::initial_time_split)
})

test_that("update a split after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow <- workflow(mtcars)
  workflow <- add_model(workflow, lm_model)
  workflow <- add_formula(workflow, mpg ~ cyl)  
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- fit(workflow)

  # Should clear fitted model
  workflow <- update_split(workflow, rsample::initial_time_split)

  expect_equal(workflow$pre$actions$split$`rsample::initial_time_split`,
               rsample::initial_time_split)

  expect_equal(workflow$fit$actions$model$spec, lm_model)
  expect_equal(workflow$data, workflow$pre$mold)
})

test_that("cannot add two split specifications", {
  workflow <- workflow()
  workflow <- add_split(workflow, rsample::initial_split)

  expect_error(add_split(workflow, rsample::initial_time_split),
               "A `split` action has already been added to this workflow")
})

test_that("add/update_split check if `...` are named", {
  workflow <- workflow()

  expect_error(
    add_split(workflow, rsample::initial_split, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )

  workflow <- add_split(workflow, rsample::initial_split)

  expect_error(
    update_split(workflow, rsample::initial_time_split, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )
  
})

test_that("Updating a split after removing one, warns", {
  workflow <- add_split(workflow(), rsample::initial_split)

  expect_warning(
    update_split(remove_split(workflow), rsample::initial_time_split),
    "The workflow does not have a split specification."
  )

})

test_that("Updating a split doesn't remove anything else", {

  # The recipe
  workflow <- add_recipe(workflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- update_split(workflow, rsample::initial_time_split)
  expect_true(has_preprocessor_recipe(workflow))
  expect_true(has_preprocessor_split(workflow))

  # The CV fold
  workflow <- add_resample(workflow(), rsample::bootstraps)
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- update_resample(workflow, rsample::vfold_cv)
  expect_true(has_preprocessor_resample(workflow))
  expect_true(has_preprocessor_split(workflow))
})

test_that("Removing a split doesn't remove anything else", {

  # The recipe
  workflow <- add_recipe(workflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- remove_split(workflow)
  expect_true(has_preprocessor_recipe(workflow))

  # The CV fold
  workflow <- add_resample(workflow(), rsample::bootstraps)
  workflow <- add_split(workflow, rsample::initial_split)
  workflow <- remove_split(workflow)
  expect_true(has_preprocessor_resample(workflow))

})

test_that("Name of split function is always saved as name in the list", {
  # For add_split
  workflow <- add_split(workflow(), rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(workflow$pre$actions$split))

  # For update_split
  workflow <- add_split(workflow(), rsample::initial_time_split)
  workflow <- update_split(workflow, rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(workflow$pre$actions$split))
})


test_that("add_split should return an object of class `rsplit`", {
  # For add_split
  fake_split <- function(x) unclass(rsample::initial_split(x))

  workflow <- add_split(workflow(mtcars), fake_split)
  workflow <- add_formula(workflow, mpg ~ cyl)
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  workflow <- add_model(workflow, lm_model)

  expect_error(
    fit(workflow),
    regexp = "The split function should return an object of class `rsplit`"
  )
})

test_that("add_split resets model fit if trained before adding the split", {
  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  workflow <- add_recipe(workflow(mtcars), ~ recipes::recipe(mpg ~ cyl, .))
  workflow <- add_model(workflow, model)
  workflow <- fit(workflow)

  workflow <- add_split(workflow, rsample::initial_split)
  expect_false(workflow$trained)
  expect_equal(workflow$data, workflow$pre$mold)
  expect_null(workflow$fit$fit)

  res <- fit(workflow)
  # Fitted on the training data
  expect_equal(nrow(pull_workflow_mold(res)$predictors), 24)
  expect_equal(nrow(pull_workflow_mold(res)$outcomes), 24)
})


# TODO
# When you figure if you'll limit the order of operations
# so that a split must by always set before an rsample
# fix this test
## test_that("Error when a resample is defined before a split", {
##   # For add_split
##   workflow <- add_resample(workflow(), rsample::vfold_cv)
##   expect_error(workflow %>% add_split(rsample::initial_split),
##                regexp = "A workflow must never have a resample before splitting the data") # nolintr
## })
