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

test_that("dropping a resample and refitting gives same result", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_resample <- fit(tflow)
  tflow <- drop_resample(mod1_resample)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  mod2_no_resample <- fit(tflow)

  expect_equal(rsplit2df(strip_elapsed(mod1_resample)),
               rsplit2df(strip_elapsed(mod2_no_resample)))
})


test_that("plug_resample can work with recipe or formula", {
  # This test is more about fit.action_model which calles fit_resample
  # in which it decides to user recipe or formula on whether their NULL
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_recipe <- fit(tflow)

  tflow <- drop_recipe(tflow)
  tflow <- plug_formula(tflow, mpg ~ cyl)
  mod1_formula <- fit(tflow)

  # rsplit can be compared because all.equal doesn't support
  # it. Instead, we convert to data frame to compare.
  expect_equal(as.data.frame(pull_tflow_tuning(mod1_recipe)),
               as.data.frame(pull_tflow_tuning(mod1_formula)))

})

test_that("Fit resample, drop a resample and refit is the same as normal fitting", { #nolintr
  rcp <- ~ recipes::step_log(recipes::recipe(.x, mpg ~ cyl), cyl, base = 10)
  tflow <- tidyflow(mtcars, seed = 542)
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_no_resample <- fit(tflow)
  resample_mod <- fit(plug_resample(mod1_no_resample, rsample::vfold_cv))
  mod2_no_resample <- fit(drop_resample(resample_mod))

  # Setting the time elapsed to NULL, since there can be very minor
  # differences in time fitting the model for comparison.
  expect_equal(strip_elapsed(mod1_no_resample),
               strip_elapsed(mod2_no_resample))
})

test_that("Can add resample after model fit and refit", {
  rcp <- ~ recipes::step_log(recipes::recipe(.x, mpg ~ cyl), cyl, base = 10)
  tflow <- tidyflow(mtcars, seed = 542)
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_no_resample <- fit(tflow)
  mod2_resample <- fit(plug_resample(mod1_no_resample, rsample::vfold_cv))

  expect_is(mod2_resample$fit$fit$tuning, "rset")
  expect_false(mod2_resample$trained)
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
