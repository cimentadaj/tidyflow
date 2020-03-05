test_that("can add a resample to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- add_resample(tidyflow, rsample::bootstraps)

  expect_is(tidyflow$pre$actions$resample, "action_resample")
})

test_that("resample is validated", {
  expect_error(add_resample(tidyflow(), 1),
               "`.f` must be a function for resampling the dataset")
})

test_that("remove a resample specification", {
  tidyflow_no_resample <- tidyflow()
  tidyflow_with_resample <- add_resample(tidyflow_no_resample,
                                         rsample::bootstraps)
  
  tidyflow_removed_resample <- remove_resample(tidyflow_with_resample)

  expect_equal(tidyflow_no_resample$pre, tidyflow_removed_resample$pre)
})

# TODO
# After you define a fit method for resample, adapt this
# example to remove the fit object after resample
## test_that("remove a recipe after model fit", {
##   lm_model <- parsnip::linear_reg()
##   lm_model <- parsnip::set_engine(lm_model, "lm")

##   rec <- recipes::recipe(mpg ~ cyl, mtcars)

##   tidyflow_no_recipe <- tidyflow()
##   tidyflow_no_recipe <- add_model(tidyflow_no_recipe, lm_model)

##   tidyflow_with_recipe  <- add_recipe(tidyflow_no_recipe, rec)
##   tidyflow_with_recipe <- fit(tidyflow_with_recipe, data = mtcars)

##   tidyflow_removed_recipe <- remove_recipe(tidyflow_with_recipe)

##   expect_equal(tidyflow_no_recipe$pre, tidyflow_removed_recipe$pre)
## })

test_that("update a resample specification", {
  tidyflow <- tidyflow()
  tidyflow <- add_resample(tidyflow, rsample::bootstraps)
  tidyflow <- update_resample(tidyflow, rsample::vfold_cv)

  expect_equal(tidyflow$pre$actions$resample$`rsample::vfold_cv`,
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

##   tidyflow <- tidyflow()
##   tidyflow <- add_model(tidyflow, lm_model)
##   tidyflow <- add_recipe(tidyflow, rec)

##   tidyflow <- fit(tidyflow, data = mtcars)

##   # Should clear fitted model
##   tidyflow <- update_recipe(tidyflow, rec2)

##   expect_equal(tidyflow$pre$actions$recipe$recipe, rec2)

##   expect_equal(tidyflow$fit$actions$model$spec, lm_model)
##   expect_null(tidyflow$pre$mold)
## })

test_that("cannot add two resample specifications", {
  tidyflow <- tidyflow()
  tidyflow <- add_resample(tidyflow, rsample::bootstraps)

  expect_error(add_resample(tidyflow, rsample::vfold_cv),
               "A `resample` action has already been added to this tidyflow")
})

test_that("add/update_resample check if `...` are named", {
  tidyflow <- tidyflow()

  expect_error(
    add_resample(tidyflow, rsample::bootstraps, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )

  tidyflow <- add_resample(tidyflow, rsample::bootstraps)

  expect_error(
    update_resample(tidyflow, rsample::vfold_cv, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )
  
})

test_that("Updating a resample after removing one, warns", {
  tidyflow <- add_resample(tidyflow(), rsample::bootstraps)

  expect_warning(
    update_resample(remove_resample(tidyflow), rsample::vfold_cv),
    "The tidyflow does not have a resample specification."
  )

})

test_that("Updating a resample doesn't remove anything else", {

  # The split
  tidyflow <- add_split(tidyflow(), rsample::initial_split)
  tidyflow <- add_resample(tidyflow, rsample::bootstraps)
  tidyflow <- update_resample(tidyflow, rsample::vfold_cv)
  expect_true(has_preprocessor_split(tidyflow))
  expect_true(has_preprocessor_resample(tidyflow))

  
  # The recipe
  tidyflow <- add_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- add_resample(tidyflow, rsample::bootstraps)
  tidyflow <- update_resample(tidyflow, rsample::vfold_cv)
  expect_true(has_preprocessor_recipe(tidyflow))
  expect_true(has_preprocessor_resample(tidyflow))

})

test_that("Removing a resample doesn't remove anything else", {
  
  # The split
  tidyflow <- add_split(tidyflow(), rsample::initial_split)
  tidyflow <- add_resample(tidyflow, rsample::bootstraps)
  tidyflow <- remove_resample(tidyflow)
  expect_true(has_preprocessor_split(tidyflow))

  # The recipe
  tidyflow <- add_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- add_resample(tidyflow, rsample::bootstraps)
  tidyflow <- remove_resample(tidyflow)
  expect_true(has_preprocessor_recipe(tidyflow))

})

test_that("Name of resample function is always saved as name in the list", {
  # For add_resample
  tidyflow <- add_resample(tidyflow(), rsample::bootstraps)
  expect_true("rsample::bootstraps" %in% names(tidyflow$pre$actions$resample))

  # For update_resample
  tidyflow <- add_resample(tidyflow(), rsample::vfold_cv)
  tidyflow <- update_resample(tidyflow, rsample::bootstraps)
  expect_true("rsample::bootstraps" %in% names(tidyflow$pre$actions$resample))
})
