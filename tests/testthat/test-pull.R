# ------------------------------------------------------------------------------
# pull_tflow_preprocessor()

test_that("can pull a formula preprocessor", {
  tidyflow <- tidyflow()
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  expect_equal(
    pull_tflow_preprocessor(tidyflow),
    mpg ~ cyl
  )
})

test_that("can pull a recipe preprocessor", {
  recipe <- ~ recipes::recipe(mpg ~ cyl, .x)

  tidyflow <- tidyflow()
  tidyflow <- plug_recipe(tidyflow, recipe)

  expect_equal(
    pull_tflow_preprocessor(tidyflow),
    rlang::as_function(recipe)
  )
})

test_that("error if no preprocessor", {
  expect_error(
    pull_tflow_preprocessor(tidyflow()),
    "does not have a preprocessor"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tflow_preprocessor(1),
    "must be a tidyflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tflow_spec()

test_that("can pull a model spec", {
  model <- parsnip::linear_reg()

  tidyflow <- tidyflow()
  tidyflow <- plug_model(tidyflow, model)

  expect_equal(
    pull_tflow_spec(tidyflow),
    model
  )
})

test_that("error if no spec", {
  expect_error(
    pull_tflow_spec(tidyflow()),
    "does not have a model spec"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tflow_spec(1),
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
    tidyflow$fit$fit$fit
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
# pull_tflow_mold()

test_that("can pull a mold", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, model)
  tidyflow <- plug_formula(tidyflow, mpg ~ cyl)

  tidyflow <- fit(tidyflow)

  expect_is(pull_tflow_mold(tidyflow), "list")

  expect_equal(
    pull_tflow_mold(tidyflow),
    tidyflow$pre$mold
  )
})

test_that("error if no mold", {
  expect_error(
    pull_tflow_mold(tidyflow()),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tflow_mold(1),
    "must be a tidyflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tflow_prepped_recipe()

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

  expect_is(pull_tflow_prepped_recipe(tidyflow), "recipe")

  expect_equal(
    pull_tflow_prepped_recipe(tidyflow),
    tidyflow$pre$mold$blueprint$recipe
  )
})

test_that("error if no recipe preprocessor", {
  expect_error(
    pull_tflow_prepped_recipe(tidyflow()),
    "must have a recipe preprocessor"
  )
})

test_that("error if no mold", {
  recipe <- ~ recipes::recipe(mpg ~ cyl, .x)

  tidyflow <- tidyflow()
  tidyflow <- plug_recipe(tidyflow, recipe)

  expect_error(
    pull_tflow_prepped_recipe(tidyflow),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a tidyflow", {
  expect_error(
    pull_tflow_prepped_recipe(1),
    "must be a tidyflow"
  )
})

# ------------------------------------------------------------------------------
# pull_tflow_resample()
tidyflow <- tidyflow(mtcars, seed = 54132)
tidyflow <- plug_split(tidyflow, rsample::initial_split)
tidyflow <- plug_resample(tidyflow, rsample::vfold_cv)
tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(.x, mpg ~ cyl))
tidyflow <- plug_model(tidyflow,
                       parsnip::set_engine(parsnip::linear_reg(), "lm"))

test_that("pull_tflow_resample can pull a resample", {
  tidyflow <- fit(tidyflow)
  expect_is(pull_tflow_resample(tidyflow), "rset")
})

test_that("pull_tflow_resample error if no resample", {
  expect_error(pull_tflow_resample(fit(drop_resample(tidyflow))),
               "The tidyflow must have a resample preprocessor")
})

test_that("pull_tflow_resample error if not a tidyflow", {
  expect_error(pull_tflow_resample(tidyflow),
               "Tidyflow has not yet been trained. Do you need to call `fit()`?"
               )
})

# ------------------------------------------------------------------------------
# pull_tflow_grid()
tidyflow <- tidyflow(mtcars, seed = 54132)
tidyflow <- plug_split(tidyflow, rsample::initial_split)
tidyflow <- plug_resample(tidyflow, rsample::vfold_cv)
tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(.x, mpg ~ .))
mod <- parsnip::set_engine(parsnip::linear_reg(penalty = tune::tune(),
                                               mixture = tune::tune()),
                           "glmnet")

tidyflow <- plug_model(tidyflow, mod)

tidyflow <- plug_grid(tidyflow, dials::grid_regular, levels = 2)

test_that("pull_tflow_grid can pull a resample", {
  tidyflow <- fit(tidyflow)
  expect_is(pull_tflow_grid(tidyflow), "param_grid")
})

test_that("pull_tflow_resample error if not a tidyflow", {
  expect_error(pull_tflow_grid(tidyflow),
               "Tidyflow has not yet been trained. Do you need to call `fit()`?"
               )
})

mod <- parsnip::set_engine(parsnip::linear_reg(), "lm")
tidyflow <- plug_model(drop_model(drop_grid(tidyflow)), mod)

test_that("pull_tflow_grid error if no grid but tuning values", {
  expect_error(pull_tflow_grid(fit(tidyflow)),
               "The tidyflow must have a grid preprocessor")
})


# ------------------------------------------------------------------------------
# pull_tflow_split()

tidyflow <- tidyflow(mtcars, seed = 54132)
tidyflow <- plug_split(tidyflow, rsample::initial_split)
tidyflow <- plug_recipe(tidyflow, ~ recipes::recipe(.x, mpg ~ cyl))
tidyflow <- plug_model(tidyflow,
                       parsnip::set_engine(parsnip::linear_reg(), "lm"))

test_that("pull_tflow_split can pull a split", {
  tidyflow <- fit(tidyflow)
  expect_is(pull_tflow_split(tidyflow), "rsplit")
})

test_that("pull_tflow_split error if no split", {
  expect_error(pull_tflow_split(fit(drop_split(tidyflow))),
               "The tidyflow must have a split preprocessor")
})

test_that("error if not a tidyflow", {
  expect_error(pull_tflow_split(tidyflow),
               "Tidyflow has not yet been trained. Do you need to call `fit()`?"
               )
})

# ------------------------------------------------------------------------------
# pull_tflow_training()

test_training <- function(untrained_tflow) {
  test_that("pull_tflow_training can pull the raw training", {
    tidyflow <- fit(untrained_tflow)
    expect_equal(pull_tflow_training(tidyflow),
                 rsample::training(pull_tflow_split(tidyflow)))
  })

  
  test_that("pull_tflow_training can pull the prepped training data", {
    tidyflow <- fit(untrained_tflow)

    training_data <- pull_tflow_training(tidyflow)
    training_data <- hardhat::forge(training_data,
                                    pull_tflow_mold(tidyflow)$blueprint,
                                    outcomes = TRUE)
    training_data <- combine_outcome_preds(training_data)
    
    expect_equal(pull_tflow_training(tidyflow, prep = TRUE),
                 training_data)
  })

  test_that("pull_tflow_training error if no split", {
    expect_error(pull_tflow_training(fit(drop_split(untrained_tflow))),
                 "The tidyflow must have a split preprocessor")
  })

  test_that("pull_tflow_training error if not a tidyflow", {
    expect_error(pull_tflow_training(untrained_tflow),
                 "Tidyflow has not yet been trained. Do you need to call `fit()`?"
                 )
  })
}

# With recipe
test_training(tidyflow)

# With formula
test_training(plug_formula(drop_recipe(tidyflow), mpg ~ cyl))

# With recipe + resample
test_training(plug_resample(tidyflow, rsample::vfold_cv))

# With formula + resample
tflow <- plug_resample(plug_formula(drop_recipe(tidyflow), mpg ~ cyl),
                       rsample::vfold_cv)
test_training(tflow)

# ------------------------------------------------------------------------------
# pull_tflow_rawdata()

test_that("pull_tflow_rawdata can pull the raw data", {
  tidyflow <- tidyflow(mtcars)

  expect_equal(
    pull_tflow_rawdata(tidyflow),
    mtcars
  )
})

test_that("pull_tflow_rawdata error if no raw data", {
  expect_error(
    pull_tflow_rawdata(tidyflow()),
    "The tidyflow does not have data"
  )
})

# ------------------------------------------------------------------------------
# pull_tflow_testing()

test_testing <- function(untrained_tflow) {
  test_that("pull_tflow_testing can pull the raw testing", {
    tidyflow <- fit(untrained_tflow)
    expect_equal(pull_tflow_testing(tidyflow),
                 rsample::testing(pull_tflow_split(tidyflow)))

    expect_equal(
      nrow(pull_tflow_testing(tidyflow)),
      8
    )

    expect_equal(
      ncol(pull_tflow_testing(tidyflow)),
      11
    )
  })
  
  test_that("pull_tflow_testing can pull the prepped testing data", {
    tidyflow <- fit(untrained_tflow)

    testing_data <- pull_tflow_testing(tidyflow)
    testing_data <- hardhat::forge(testing_data,
                                   pull_tflow_mold(tidyflow)$blueprint,
                                   outcomes = TRUE)
    testing_data <- combine_outcome_preds(testing_data)
    
    expect_equal(pull_tflow_testing(tidyflow, prep = TRUE),
                 testing_data)
  })

  test_that("pull_tflow_testing error if no split", {
    expect_error(pull_tflow_testing(fit(drop_split(untrained_tflow))),
                 "The tidyflow must have a split preprocessor")
  })

  test_that("pull_tflow_testing error if not a tidyflow", {
    expect_error(pull_tflow_testing(untrained_tflow),
                 "Tidyflow has not yet been trained. Do you need to call `fit()`?"
                 )
  })
}

# With recipe
test_testing(tidyflow)

# With formula
test_testing(plug_formula(drop_recipe(tidyflow), mpg ~ cyl))

# With recipe + resample
test_testing(plug_resample(tidyflow, rsample::vfold_cv))

# With formula + resample
tflow <- plug_resample(plug_formula(drop_recipe(tidyflow), mpg ~ cyl),
                       rsample::vfold_cv)

test_training(tflow)
# ------------------------------------------------------------------------------
# pull_tflow_fit_tuning()
model <- parsnip::linear_reg()
model <- parsnip::set_engine(model, "lm")

tidyflow <- tidyflow(mtcars)
tidyflow <- plug_model(tidyflow, model)
tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
tidyflow <- plug_resample(tidyflow, rsample::vfold_cv)

test_that("pull_tflow_fit_tuning can pull a model tuning result", {
  tidyflow <- fit(tidyflow)
  res <- pull_tflow_fit_tuning(tidyflow)

  expect_is(
    res,
    "resample_results"
  )

  expect_is(
    res,
    "tune_results"
  )

  expect_equal(
    nrow(res),
    10
  )
})

test_that("pull_tflow_fit_tuning error if no fit", {
  expect_error(
    pull_tflow_fit_tuning(tidyflow),
    "The tidyflow does not have a tuning fit. Have you called `fit[(][)]` yet?"
  )
})

test_that("pull_tflow_fit_tuning error if not a tidyflow", {
  expect_error(
    pull_tflow_fit_tuning(1),
    "must be a tidyflow"
  )
})
