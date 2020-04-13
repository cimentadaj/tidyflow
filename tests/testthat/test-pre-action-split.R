test_that("can add a split to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- plug_split(tidyflow, rsample::initial_split)

  expect_is(tidyflow$pre$actions$split, "action_split")
})

test_that("split is validated", {
  expect_error(plug_split(tidyflow(), 1),
               "`.f` must be a function for splitting the dataset")
})

test_that("remove a split specification", {
  tidyflow_no_split <- tidyflow()
  tidyflow_with_split <- plug_split(tidyflow_no_split, rsample::initial_split)
  tidyflow_removed_split <- drop_split(tidyflow_with_split)

  expect_equal(tidyflow_no_split$pre, tidyflow_removed_split$pre)
})

test_that("dropping a split and refitting gives same result", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_split <- fit(tflow)
  tflow <- drop_split(mod1_split)
  tflow <- plug_split(tflow, rsample::initial_split)
  mod2_no_split <- fit(tflow)

  expect_equal(strip_elapsed(mod1_split),
               strip_elapsed(mod2_no_split))
})


test_that("plug_split resets model fit if trained before adding the split", {
  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  tflow <- plug_recipe(tidyflow(mtcars), ~ recipes::recipe(mpg ~ cyl, .))
  tflow <- plug_model(tflow, model)
  tflow <- fit(tflow)

  tflow <- plug_split(tflow, rsample::initial_split)
  expect_false(tflow$trained)
  expect_equal(tflow$data, tflow$pre$mold)
  expect_error(pull_tflow_fit(tflow))

  res <- fit(tflow)
  # Fitted on the training data
  expect_equal(nrow(pull_tflow_mold(res)$predictors), 24)
  expect_equal(nrow(pull_tflow_mold(res)$outcomes), 24)
})


test_that("Can add split after model fit and refit", {
  rcp <- ~ recipes::step_log(recipes::recipe(.x, mpg ~ cyl), cyl, base = 10)
  tflow <- tidyflow(mtcars, seed = 542)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  mod1_no_split <- fit(tflow)
  mod2_split <- fit(plug_split(mod1_no_split, rsample::initial_split))

  expect_equal(nobs(pull_tflow_fit(mod2_split)$fit),
               nrow(pull_tflow_mold(mod2_split)$predictors))

  expect_true(mod2_split$trained)
})

test_that("Saves testing data after split", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  tidyflow_with_split  <- plug_split(tidyflow(mtcars), rsample::initial_split)
  tidyflow_with_split <- plug_recipe(tidyflow_with_split, ~ recipes::recipe(mpg ~ cyl, .))
  tidyflow_with_split  <- plug_model(tidyflow_with_split, lm_model)
  tidyflow_with_split <- fit(tidyflow_with_split)

  expect_true(!is.null(pull_tflow_testing(tidyflow_with_split)))
})


test_that("remove a split after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow_no_split <- plug_formula(tidyflow(mtcars), mpg ~ cyl)
  tidyflow_no_split <- plug_model(tidyflow_no_split, lm_model)

  tidyflow_with_split  <- plug_split(tidyflow_no_split, rsample::initial_split)
  tidyflow_with_split <- fit(tidyflow_with_split)

  tidyflow_removed_split <- drop_split(tidyflow_with_split)
  expect_equal(tidyflow_no_split$pre$actions, tidyflow_removed_split$pre$actions)
})

test_that("update a split specification", {
  tidyflow <- tidyflow()
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- replace_split(tidyflow, rsample::initial_time_split)

  expect_equal(tidyflow$pre$actions$split$`rsample::initial_time_split`,
               rsample::initial_time_split)
})

test_that("update a split after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)  
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- fit(tidyflow)

  # Should clear fitted model
  tidyflow <- replace_split(tidyflow, rsample::initial_time_split)

  expect_equal(tidyflow$pre$actions$split$`rsample::initial_time_split`,
               rsample::initial_time_split)

  expect_equal(pull_tflow_spec(tidyflow), lm_model)
  expect_equal(pull_tflow_rawdata(tidyflow), tidyflow$pre$mold)
})

test_that("cannot add two split specifications", {
  tidyflow <- tidyflow()
  tidyflow <- plug_split(tidyflow, rsample::initial_split)

  expect_error(plug_split(tidyflow, rsample::initial_time_split),
               "A `split` action has already been added to this tidyflow")
})

test_that("add/replace_split check if `...` are named", {
  tidyflow <- tidyflow()

  expect_error(
    plug_split(tidyflow, rsample::initial_split, 0.8),
    regexp = "Arguments in `...` for `plug_split` should be uniquely named"
  )

  tidyflow <- plug_split(tidyflow, rsample::initial_split)

  expect_error(
    replace_split(tidyflow, rsample::initial_time_split, 0.8),
    regexp = "Arguments in `...` for `plug_split` should be uniquely named"
  )
  
})

test_that("Updating a split after removing one, warns", {
  tidyflow <- plug_split(tidyflow(), rsample::initial_split)

  expect_warning(
    replace_split(drop_split(tidyflow), rsample::initial_time_split),
    "The tidyflow does not have a split specification."
  )

})

test_that("drop_split removes the action and the result", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow <- plug_recipe(tidyflow(mtcars),
                          ~ recipes::recipe(mpg ~ cyl, data = .x))
  
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- plug_model(tidyflow, lm_model)
  mod1 <- fit(tidyflow)
  tidyflow <- drop_split(tidyflow)

  # Both are null on dropped tidyflow
  expect_null(tidyflow$pre$results$split)
  expect_null(tidyflow$pre$actions$split)
  # Both are available on fitted tidyflow
  expect_is(mod1$pre$results$split, "rsplit")
  expect_is(mod1$pre$actions$split[[1]], "function")
})

test_that("Updating a split doesn't remove anything else", {

  # The recipe
  tidyflow <- plug_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- replace_split(tidyflow, rsample::initial_time_split)
  expect_true(has_preprocessor_recipe(tidyflow))
  expect_true(has_preprocessor_split(tidyflow))

  # The CV fold
  tidyflow <- plug_resample(tidyflow(), rsample::bootstraps)
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- replace_resample(tidyflow, rsample::vfold_cv)
  expect_true(has_preprocessor_resample(tidyflow))
  expect_true(has_preprocessor_split(tidyflow))
})

test_that("Removing a split doesn't remove anything else", {

  # The recipe
  tidyflow <- plug_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- drop_split(tidyflow)
  expect_true(has_preprocessor_recipe(tidyflow))

  # The CV fold
  tidyflow <- plug_resample(tidyflow(), rsample::bootstraps)
  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  tidyflow <- drop_split(tidyflow)
  expect_true(has_preprocessor_resample(tidyflow))

})

test_that("Name of split function is always saved as name in the list", {
  # For plug_split
  tidyflow <- plug_split(tidyflow(), rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(tidyflow$pre$actions$split))

  # For replace_split
  tidyflow <- plug_split(tidyflow(), rsample::initial_time_split)
  tidyflow <- replace_split(tidyflow, rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(tidyflow$pre$actions$split))
})


test_that("plug_split should return an object of class `rsplit`", {
  # For plug_split
  fake_split <- function(x) unclass(rsample::initial_split(x))

  tidyflow <- plug_split(tidyflow(mtcars), fake_split)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  tidyflow <- plug_model(tidyflow, lm_model)

  expect_error(
    fit(tidyflow),
    regexp = "The split function should return an object of class `rsplit`"
  )
})

test_that("plug_split resets model fit if trained before adding the split", {
  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  tidyflow <- plug_recipe(tidyflow(mtcars), ~ recipes::recipe(mpg ~ cyl, .))
  tidyflow <- plug_model(tidyflow, model)
  tidyflow <- fit(tidyflow)

  tidyflow <- plug_split(tidyflow, rsample::initial_split)
  expect_false(tidyflow$trained)
  expect_equal(pull_tflow_rawdata(tidyflow),
               pull_tflow_mold(tidyflow))
  expect_error(pull_tflow_fit(tidyflow))

  res <- fit(tidyflow)
  # Fitted on the training data
  expect_equal(nrow(pull_tflow_mold(res)$predictors), 24)
  expect_equal(nrow(pull_tflow_mold(res)$outcomes), 24)
})


# TODO
# When you figure if you'll limit the order of operations
# so that a split must by always set before an rsample
# fix this test
## test_that("Error when a resample is defined before a split", {
##   # For plug_split
##   tidyflow <- plug_resample(tidyflow(), rsample::vfold_cv)
##   expect_error(tidyflow %>% plug_split(rsample::initial_split),
##                regexp = "A tidyflow must never have a resample before splitting the data") # nolintr
## })
