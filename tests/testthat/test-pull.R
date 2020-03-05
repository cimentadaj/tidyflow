# ------------------------------------------------------------------------------
# pull_tidyflow_preprocessor()

test_that("can pull a formula preprocessor", {
  tidyflow <- tidyflow()
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  expect_equal(
    pull_tidyflow_preprocessor(tidyflow),
    mpg ~ cyl
  )
})

test_that("can pull a recipe preprocessor", {
  recipe <- ~ recipes::recipe(mpg ~ cyl, .x)

  tidyflow <- tidyflow()
  tidyflow <- plug_recipe(tidyflow, recipe)

  expect_equal(
    pull_tidyflow_preprocessor(tidyflow),
    rlang::as_function(recipe)
  )
})

test_that("error if no preprocessor", {
  expect_error(
    pull_tidyflow_preprocessor(tidyflow()),
    "does not have a preprocessor"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tidyflow_preprocessor(1),
    "must be a tidyflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tidyflow_spec()

test_that("can pull a model spec", {
  model <- parsnip::linear_reg()

  tidyflow <- tidyflow()
  tidyflow <- plug_model(tidyflow, model)

  expect_equal(
    pull_tidyflow_spec(tidyflow),
    model
  )
})

test_that("error if no spec", {
  expect_error(
    pull_tidyflow_spec(tidyflow()),
    "does not have a model spec"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tidyflow_spec(1),
    "must be a tidyflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tflow_fit()

test_that("can pull a model fit", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  tidyflow <- fit(tidyflow)

  expect_equal(
    pull_tflow_fit(tidyflow),
    tidyflow$fit$fit
  )
})

test_that("error if no fit", {
  expect_error(
    pull_tflow_fit(tidyflow()),
    "does not have a model fit. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tflow_fit(1),
    "must be a tidyflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tidyflow_mold()

test_that("can pull a mold", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  tidyflow <- fit(tidyflow)

  expect_is(pull_tidyflow_mold(tidyflow), "list")

  expect_equal(
    pull_tidyflow_mold(tidyflow),
    tidyflow$pre$mold
  )
})

test_that("error if no mold", {
  expect_error(
    pull_tidyflow_mold(tidyflow()),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tidyflow_mold(1),
    "must be a tidyflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tidyflow_prepped_recipe()

test_that("can pull a prepped recipe", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  recipe <-
    ~ recipes::recipe(mpg ~ cyl, .x) %>%
      recipes::step_log(cyl)

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, model)
  tidyflow <- plug_recipe(tidyflow, recipe)

  tidyflow <- fit(tidyflow)

  expect_is(pull_tidyflow_prepped_recipe(tidyflow), "recipe")

  expect_equal(
    pull_tidyflow_prepped_recipe(tidyflow),
    tidyflow$pre$mold$blueprint$recipe
  )
})

test_that("error if no recipe preprocessor", {
  expect_error(
    pull_tidyflow_prepped_recipe(tidyflow()),
    "must have a recipe preprocessor"
  )
})

test_that("error if no mold", {
  recipe <- ~ recipes::recipe(mpg ~ cyl, .x)

  tidyflow <- tidyflow()
  tidyflow <- plug_recipe(tidyflow, recipe)

  expect_error(
    pull_tidyflow_prepped_recipe(tidyflow),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tidyflow_prepped_recipe(1),
    "must be a tidyflow"
  )
})

test_that("can pull the raw data", {
  tidyflow <- tidyflow(mtcars)

  expect_equal(
    pull_tidyflow_rawdata(tidyflow),
    mtcars
  )
})

test_that("error if no raw data", {
  expect_error(
    pull_tidyflow_rawdata(tidyflow()),
    "The tidyflow does not have data"
  )
})


check_testing <- function(x) {
  expect_equal(
    nrow(pull_tidyflow_testing(x)),
    8
  )

  expect_equal(
    ncol(pull_tidyflow_testing(x)),
    11
  )

}

test_that("can pull testing data", {
  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  tidyflow <- plug_recipe(tidyflow(mtcars), ~ recipes::recipe(mpg ~ cyl, .))
  tidyflow <- plug_model(tidyflow, model)

  res <- fit(tidyflow)

  expect_error(
    pull_tidyflow_testing(res),
    "The tidyflow must have a split preprocessor."
  )

  # Add split. The testing should have 8 rows (75%)
  res <- fit(plug_split(res, rsample::initial_split))

  check_testing(res)

  expect_error(
    pull_tidyflow_testing(drop_recipe(res)),
    "Tidyflow has not yet been trained. Do you need to call `fit()`?"
  )

  res <- fit(plug_formula(drop_recipe(res), mpg ~ cyl))
  # The data is refit the same way with a formula
  check_testing(res)

})
