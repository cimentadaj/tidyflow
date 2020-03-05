test_that("can add a split to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- add_split(tidyflow, rsample::initial_split)

  expect_is(tidyflow$pre$actions$split, "action_split")
})

test_that("split is validated", {
  expect_error(add_split(tidyflow(), 1),
               "`.f` must be a function for splitting the dataset")
})

test_that("remove a split specification", {
  tidyflow_no_split <- tidyflow()
  tidyflow_with_split <- add_split(tidyflow_no_split, rsample::initial_split)
  tidyflow_removed_split <- drop_split(tidyflow_with_split)

  expect_equal(tidyflow_no_split$pre, tidyflow_removed_split$pre)
})


test_that("Saves testing data after split", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  tidyflow_with_split  <- add_split(tidyflow(mtcars), rsample::initial_split)
  tidyflow_with_split <- add_recipe(tidyflow_with_split, ~ recipes::recipe(mpg ~ cyl, .))
  tidyflow_with_split  <- add_model(tidyflow_with_split, lm_model)
  tidyflow_with_split <- fit(tidyflow_with_split)

  expect_true(!is.null(tidyflow_with_split$pre$actions$split$testing))
})


test_that("remove a split after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow_no_split <- add_formula(tidyflow(mtcars), mpg ~ cyl)
  tidyflow_no_split <- add_model(tidyflow_no_split, lm_model)

  tidyflow_with_split  <- add_split(tidyflow_no_split, rsample::initial_split)
  tidyflow_with_split <- fit(tidyflow_with_split)

  tidyflow_removed_split <- drop_split(tidyflow_with_split)

  expect_equal(tidyflow_no_split$pre, tidyflow_removed_split$pre)
})

test_that("update a split specification", {
  tidyflow <- tidyflow()
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- update_split(tidyflow, rsample::initial_time_split)

  expect_equal(tidyflow$pre$actions$split$`rsample::initial_time_split`,
               rsample::initial_time_split)
})

test_that("update a split after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- add_model(tidyflow, lm_model)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)  
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- fit(tidyflow)

  # Should clear fitted model
  tidyflow <- update_split(tidyflow, rsample::initial_time_split)

  expect_equal(tidyflow$pre$actions$split$`rsample::initial_time_split`,
               rsample::initial_time_split)

  expect_equal(tidyflow$fit$actions$model$spec, lm_model)
  expect_equal(tidyflow$data, tidyflow$pre$mold)
})

test_that("cannot add two split specifications", {
  tidyflow <- tidyflow()
  tidyflow <- add_split(tidyflow, rsample::initial_split)

  expect_error(add_split(tidyflow, rsample::initial_time_split),
               "A `split` action has already been added to this tidyflow")
})

test_that("add/update_split check if `...` are named", {
  tidyflow <- tidyflow()

  expect_error(
    add_split(tidyflow, rsample::initial_split, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )

  tidyflow <- add_split(tidyflow, rsample::initial_split)

  expect_error(
    update_split(tidyflow, rsample::initial_time_split, 0.8),
    regexp = "Arguments in `...` for `.f` should be named"
  )
  
})

test_that("Updating a split after removing one, warns", {
  tidyflow <- add_split(tidyflow(), rsample::initial_split)

  expect_warning(
    update_split(drop_split(tidyflow), rsample::initial_time_split),
    "The tidyflow does not have a split specification."
  )

})

test_that("Updating a split doesn't remove anything else", {

  # The recipe
  tidyflow <- add_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- update_split(tidyflow, rsample::initial_time_split)
  expect_true(has_preprocessor_recipe(tidyflow))
  expect_true(has_preprocessor_split(tidyflow))

  # The CV fold
  tidyflow <- add_resample(tidyflow(), rsample::bootstraps)
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- update_resample(tidyflow, rsample::vfold_cv)
  expect_true(has_preprocessor_resample(tidyflow))
  expect_true(has_preprocessor_split(tidyflow))
})

test_that("Removing a split doesn't remove anything else", {

  # The recipe
  tidyflow <- add_recipe(tidyflow(), ~ recipes::recipe(mpg ~ cyl, data = .x))
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- drop_split(tidyflow)
  expect_true(has_preprocessor_recipe(tidyflow))

  # The CV fold
  tidyflow <- add_resample(tidyflow(), rsample::bootstraps)
  tidyflow <- add_split(tidyflow, rsample::initial_split)
  tidyflow <- drop_split(tidyflow)
  expect_true(has_preprocessor_resample(tidyflow))

})

test_that("Name of split function is always saved as name in the list", {
  # For add_split
  tidyflow <- add_split(tidyflow(), rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(tidyflow$pre$actions$split))

  # For update_split
  tidyflow <- add_split(tidyflow(), rsample::initial_time_split)
  tidyflow <- update_split(tidyflow, rsample::initial_split)
  expect_true("rsample::initial_split" %in% names(tidyflow$pre$actions$split))
})


test_that("add_split should return an object of class `rsplit`", {
  # For add_split
  fake_split <- function(x) unclass(rsample::initial_split(x))

  tidyflow <- add_split(tidyflow(mtcars), fake_split)
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  tidyflow <- add_model(tidyflow, lm_model)

  expect_error(
    fit(tidyflow),
    regexp = "The split function should return an object of class `rsplit`"
  )
})

test_that("add_split resets model fit if trained before adding the split", {
  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  tidyflow <- add_recipe(tidyflow(mtcars), ~ recipes::recipe(mpg ~ cyl, .))
  tidyflow <- add_model(tidyflow, model)
  tidyflow <- fit(tidyflow)

  tidyflow <- add_split(tidyflow, rsample::initial_split)
  expect_false(tidyflow$trained)
  expect_equal(tidyflow$data, tidyflow$pre$mold)
  expect_null(tidyflow$fit$fit)

  res <- fit(tidyflow)
  # Fitted on the training data
  expect_equal(nrow(pull_tidyflow_mold(res)$predictors), 24)
  expect_equal(nrow(pull_tidyflow_mold(res)$outcomes), 24)
})


# TODO
# When you figure if you'll limit the order of operations
# so that a split must by always set before an rsample
# fix this test
## test_that("Error when a resample is defined before a split", {
##   # For add_split
##   tidyflow <- add_resample(tidyflow(), rsample::vfold_cv)
##   expect_error(tidyflow %>% add_split(rsample::initial_split),
##                regexp = "A tidyflow must never have a resample before splitting the data") # nolintr
## })
