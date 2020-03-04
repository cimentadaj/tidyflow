test_that("can add a resample to a workflow", {
  workflow <- workflow()
  workflow <- add_resample(workflow, rsample::bootstraps)

  expect_is(workflow$pre$actions$resample, "action_resample")
})

test_that("resample is validated", {
  expect_error(add_resample(workflow(), 1),
               "`.f` must be a function for resampling the dataset")
})

test_that("remove a resample specification", {
  workflow_no_resample <- workflow()
  workflow_with_resample <- add_resample(workflow_no_resample,
                                         rsample::bootstraps)
  
  workflow_removed_resample <- remove_resample(workflow_with_resample)

  expect_equal(workflow_no_resample$pre, workflow_removed_resample$pre)
})

# TODO
# After you define a fit method for resample, adapt this
# example to remove the fit object after resample
## test_that("remove a recipe after model fit", {
##   lm_model <- parsnip::linear_reg()
##   lm_model <- parsnip::set_engine(lm_model, "lm")

##   rec <- recipes::recipe(mpg ~ cyl, mtcars)

##   workflow_no_recipe <- workflow()
##   workflow_no_recipe <- add_model(workflow_no_recipe, lm_model)

##   workflow_with_recipe  <- add_recipe(workflow_no_recipe, rec)
##   workflow_with_recipe <- fit(workflow_with_recipe, data = mtcars)

##   workflow_removed_recipe <- remove_recipe(workflow_with_recipe)

##   expect_equal(workflow_no_recipe$pre, workflow_removed_recipe$pre)
## })

test_that("update a resample specification", {
  workflow <- workflow()
  workflow <- add_resample(workflow, rsample::bootstraps)
  workflow <- update_resample(workflow, rsample::vfold_cv)

  expect_equal(workflow$pre$actions$resample$`rsample::vfold_cv`,
               rsample::vfold_cv)
})

# TODO
# After you define a fit method for resample, adapt this
# example to remove the fit object after resample
## test_that("update a recipe after model fit", {
##   rec <- recipes::recipe(mpg ~ cyl, mtcars)
##   rec2 <- recipes::recipe(mpg ~ disp, mtcars)

##   lm_model <- parsnip::linear_reg()
##   lm_model <- parsnip::set_engine(lm_model, "lm")

##   workflow <- workflow()
##   workflow <- add_model(workflow, lm_model)
##   workflow <- add_recipe(workflow, rec)

##   workflow <- fit(workflow, data = mtcars)

##   # Should clear fitted model
##   workflow <- update_recipe(workflow, rec2)

##   expect_equal(workflow$pre$actions$recipe$recipe, rec2)

##   expect_equal(workflow$fit$actions$model$spec, lm_model)
##   expect_null(workflow$pre$mold)
## })

test_that("cannot add two resample specifications", {
  workflow <- workflow()
  workflow <- add_resample(workflow, rsample::bootstraps)

  expect_error(add_resample(workflow, rsample::vfold_cv),
               "A `resample` action has already been added to this workflow")
})

test_that("add/update_resample check if `...` are named", {
  workflow <- workflow()

  expect_error(
    add_resample(workflow, rsample::bootstraps, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )

  workflow <- add_resample(workflow, rsample::bootstraps)

  expect_error(
    update_resample(workflow, rsample::vfold_cv, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )
  
})

test_that("Updating a resample after removing one, warns", {
  workflow <- add_resample(workflow(), rsample::bootstraps)

  expect_warning(
    update_resample(remove_resample(workflow), rsample::vfold_cv),
    "The workflow does not have a resample specification."
  )

})

test_that("Updating a resample doesn't remove anything else", {

  # The split
  workflow <- add_split(workflow(), rsample::initial_split)
  workflow <- add_resample(workflow, rsample::bootstraps)
  workflow <- update_resample(workflow, rsample::vfold_cv)
  expect_true(has_preprocessor_split(workflow))
  expect_true(has_preprocessor_resample(workflow))

  
  # The recipe
  workflow <- add_recipe(workflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  workflow <- add_resample(workflow, rsample::bootstraps)
  workflow <- update_resample(workflow, rsample::vfold_cv)
  expect_true(has_preprocessor_recipe(workflow))
  expect_true(has_preprocessor_resample(workflow))

})

test_that("Removing a resample doesn't remove anything else", {
  
  # The split
  workflow <- add_split(workflow(), rsample::initial_split)
  workflow <- add_resample(workflow, rsample::bootstraps)
  workflow <- remove_resample(workflow)
  expect_true(has_preprocessor_split(workflow))

  # The recipe
  workflow <- add_recipe(workflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  workflow <- add_resample(workflow, rsample::bootstraps)
  workflow <- remove_resample(workflow)
  expect_true(has_preprocessor_recipe(workflow))

})

test_that("Name of resample function is always saved as name in the list", {
  # For add_resample
  workflow <- add_resample(workflow(), rsample::bootstraps)
  expect_true("rsample::bootstraps" %in% names(workflow$pre$actions$resample))

  # For update_resample
  workflow <- add_resample(workflow(), rsample::vfold_cv)
  workflow <- update_resample(workflow, rsample::bootstraps)
  expect_true("rsample::bootstraps" %in% names(workflow$pre$actions$resample))
})
