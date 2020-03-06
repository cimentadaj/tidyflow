test_that("can add a resample to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)

  expect_is(tidyflow$pre$actions$resample, "action_resample")
})

test_that("resample is validated", {
  expect_error(plug_resample(tidyflow(), 1),
               "`.f` must be a function for resampling the dataset")
})

test_that("remove a resample specification", {
  tidyflow_no_resample <- tidyflow()
  tidyflow_with_resample <- plug_resample(tidyflow_no_resample,
                                         rsample::bootstraps)
  
  tidyflow_removed_resample <- drop_resample(tidyflow_with_resample)

  expect_equal(tidyflow_no_resample$pre, tidyflow_removed_resample$pre)
})

test_that("Dropping a resample and refitting is the same as normal fitting", {

  rcp <-
    ~ .x %>%
    recipes::recipe(mpg ~ cyl) %>%
    recipes::step_log(cyl, base = 10)

  mod <-
    mtcars %>%
    tidyflow() %>%
    plug_split(rsample::initial_split) %>%
    plug_recipe(rcp) %>%
    plug_model(parsnip::set_engine(parsnip::linear_reg(), "lm"))

    set.seed(5421)
    non_resample_mod <- mod %>% fit()
    resample_mod <- mod %>% plug_resample(rsample::vfold_cv) %>% fit()
    set.seed(5421)
    dropped_resample_mod <- resample_mod %>% drop_resample() %>% fit()

    # Setting the time elapsed to NULL, since there can be very minor
    # differences in time fitting the model for comparison.
    dropped_resample_mod$fit$fit$elapsed <- NULL
    non_resample_mod$fit$fit$elapsed <- NULL
    expect_equal(non_resample_mod, dropped_resample_mod)
})

# TODO
# After you define a fit method for resample, adapt this
# example to remove the fit object after resample
## test_that("remove a recipe after model fit", {
##   lm_model <- parsnip::linear_reg()
##   lm_model <- parsnip::set_engine(lm_model, "lm")

##   rec <- recipes::recipe(mpg ~ cyl, mtcars)

##   tidyflow_no_recipe <- tidyflow()
##   tidyflow_no_recipe <- plug_model(tidyflow_no_recipe, lm_model)

##   tidyflow_with_recipe  <- plug_recipe(tidyflow_no_recipe, rec)
##   tidyflow_with_recipe <- fit(tidyflow_with_recipe, data = mtcars)

##   tidyflow_removed_recipe <- drop_recipe(tidyflow_with_recipe)

##   expect_equal(tidyflow_no_recipe$pre, tidyflow_removed_recipe$pre)
## })

test_that("update a resample specification", {
  tidyflow <- tidyflow()
  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)
  tidyflow <- replace_resample(tidyflow, rsample::vfold_cv)

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
##   tidyflow <- plug_model(tidyflow, lm_model)
##   tidyflow <- plug_recipe(tidyflow, rec)

##   tidyflow <- fit(tidyflow, data = mtcars)

##   # Should clear fitted model
##   tidyflow <- replace_recipe(tidyflow, rec2)

##   expect_equal(tidyflow$pre$actions$recipe$recipe, rec2)

##   expect_equal(tidyflow$fit$actions$model$spec, lm_model)
##   expect_null(tidyflow$pre$mold)
## })

test_that("cannot add two resample specifications", {
  tidyflow <- tidyflow()
  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)

  expect_error(plug_resample(tidyflow, rsample::vfold_cv),
               "A `resample` action has already been added to this tidyflow")
})

test_that("add/replace_resample check if `...` are named", {
  tidyflow <- tidyflow()

  expect_error(
    plug_resample(tidyflow, rsample::bootstraps, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )

  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)

  expect_error(
    replace_resample(tidyflow, rsample::vfold_cv, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )
  
})

test_that("Updating a resample after removing one, warns", {
  tidyflow <- plug_resample(tidyflow(), rsample::bootstraps)

  expect_warning(
    replace_resample(drop_resample(tidyflow), rsample::vfold_cv),
    "The tidyflow does not have a resample specification."
  )

})

test_that("Updating a resample doesn't remove anything else", {

  # The split
  tidyflow <- plug_split(tidyflow(), rsample::initial_split)
  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)
  tidyflow <- replace_resample(tidyflow, rsample::vfold_cv)
  expect_true(has_preprocessor_split(tidyflow))
  expect_true(has_preprocessor_resample(tidyflow))

  
  # The recipe
  tidyflow <- plug_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)
  tidyflow <- replace_resample(tidyflow, rsample::vfold_cv)
  expect_true(has_preprocessor_recipe(tidyflow))
  expect_true(has_preprocessor_resample(tidyflow))

})

test_that("Removing a resample doesn't remove anything else", {
  
  # The split
  tidyflow <- plug_split(tidyflow(), rsample::initial_split)
  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)
  tidyflow <- drop_resample(tidyflow)
  expect_true(has_preprocessor_split(tidyflow))

  # The recipe
  tidyflow <- plug_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- plug_resample(tidyflow, rsample::bootstraps)
  tidyflow <- drop_resample(tidyflow)
  expect_true(has_preprocessor_recipe(tidyflow))

})

test_that("Name of resample function is always saved as name in the list", {
  # For plug_resample
  tidyflow <- plug_resample(tidyflow(), rsample::bootstraps)
  expect_true("rsample::bootstraps" %in% names(tidyflow$pre$actions$resample))

  # For replace_resample
  tidyflow <- plug_resample(tidyflow(), rsample::vfold_cv)
  tidyflow <- replace_resample(tidyflow, rsample::bootstraps)
  expect_true("rsample::bootstraps" %in% names(tidyflow$pre$actions$resample))
})
