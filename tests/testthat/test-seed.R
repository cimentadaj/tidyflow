rcp <- ~ recipes::step_log(recipes::recipe(mpg ~ cyl, data = .x),
                           cyl,
                           base = 10)

test_that("Not specifying the seed returns different results using split", {

  tflow <- plug_recipe(plug_split(tidyflow(mtcars), rsample::initial_split), rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  first_mod <- fit(tflow)
  second_mod <- fit(tflow)

  expect_false(identical(first_mod, second_mod))
})

test_that("Specifying the seed returns the same results using split", {

  tflow <- plug_split(tidyflow(mtcars, seed = 25131), rsample::initial_split)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))
  

  first_mod <- fit(tflow)
  second_mod <- fit(tflow)

  # Setting the time elapsed to NULL, since there can be very minor
  # differences in time fitting the model for comparison.
  expect_equal(strip_elapsed(first_mod), strip_elapsed(second_mod))
})

test_that("Not specifying the seed returns different results using resample", {
  tflow <- plug_recipe(plug_resample(tidyflow(mtcars), rsample::vfold_cv), rcp)
  tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

  first_mod <- fit(tflow)
  second_mod <- fit(tflow)

  expect_false(identical(first_mod, second_mod))
})

test_that("Specifying the seed returns the same results using resample", {

    tflow <- plug_resample(tidyflow(mtcars, seed = 25131), rsample::vfold_cv)
    tflow <- plug_recipe(tflow, rcp)
    tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

    ## all.equal doesn't work with list columns from rsplit
    ## `rsplit2df` simply accesses the rset_res and tuning_res
    ## and converts them to data frame so that expect_equal
    ## can pass.
    first_mod <- rsplit2df(fit(tflow))
    second_mod <- rsplit2df(fit(tflow))

    # You were using expect_equal but expect equal
    # uses some sort of all.equal for tibble
    # and throws an error. This will be fixed
    # when this issue is resolved:
    # https://github.com/r-lib/testthat/issues/447
    expect_true(
      all.equal.list(first_mod, second_mod)
    )

})

test_that("tidyflow with random numbers across each pre step gives same result as tidymodels", {

  ## Tidyflow modeling
  mtcars$cyl <- as.factor(mtcars$cyl)
  svm_mod <-
    parsnip::set_engine(
      parsnip::rand_forest("classification", mtry = tune::tune()),
      "randomForest",
      )

  rec <-
    ~ recipes::step_ns(recipes::recipe(cyl ~ ., data = .x),
                       qsec,
                       deg_free = round(runif(1, 0, 10)))

  tflow <- tidyflow(mtcars, seed = 4943)
  tflow <- plug_recipe(plug_split(tflow, rsample::initial_split), rec)
  tflow <- plug_grid(plug_resample(tflow, rsample::bootstraps, times = 1),
                     dials::grid_latin_hypercube,
                     size = 2)
  tflow <- plug_model(tflow, svm_mod)

  # Final result
  tflow <- fit(tflow)

  ## Tidymodels modeling
  set.seed(4943)

  # These are steps to make sure that the final result matches perfectly.
  # Since we run mold inside tidyflow, that reorders some columns.
  # Here we take some steps to make sure evrything is the same
  ## rownames(mtcars) <- NULL
  col_order <- c("mpg", "cyl", "disp", "hp",
                 "drat", "wt", "qsec", "vs", "am",
                 "gear", "carb")
  mtcars <- mtcars[col_order]
  mtcars <- rsample::training(rsample::initial_split(mtcars))

  set.seed(4943)
  mtcars_rs <- rsample::bootstraps(mtcars, times = 1)

  set.seed(4943)
  rec <-
    recipes::step_ns(recipes::recipe(cyl ~ ., data = mtcars),
                     qsec,
                     deg_free = round(runif(1, 0, 10)))

  set.seed(4943)
  grid <- dials::grid_latin_hypercube(dials::finalize(dials::mtry(), mtcars[-1]),
                                      size = 2)

  set.seed(4943)
  rec_form <-
    tune::tune_grid(
      object = svm_mod,
      preprocessor = rec,
      resamples = mtcars_rs,
      control = control_grid(),
      grid = grid
    )

  x <- tblattr_2_df(pull_tflow_fit_tuning(tflow))
  y <- tblattr_2_df(rec_form)

  # Both dataframes should be exactly the same
  # This means that running the same iteration
  # with random steps at the initial split,
  # the recipe, the resample and the grid,
  # it will returns the same result in both instances

  expect_equal(as.data.frame(x),
               as.data.frame(y),
               check.attributes = FALSE)
})
