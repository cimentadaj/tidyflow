between <- function(x, start, end) all(x >= start & x <= end)

test_that("can add a grid to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- plug_grid(tidyflow, dials::grid_regular)

  expect_is(tidyflow$pre$actions$grid, "action_grid")
})

test_that("grid is validated", {
  expect_error(plug_grid(tidyflow(), 1),
               "`.f` must be a function for creating a grid of tuning parameters")
})

test_that("remove a grid specification", {
  tidyflow_no_grid <- tidyflow()
  tidyflow_with_grid <- plug_grid(tidyflow_no_grid, dials::grid_regular)
  tidyflow_removed_grid <- drop_grid(tidyflow_with_grid)

  expect_equal(tidyflow_no_grid$pre, tidyflow_removed_grid$pre)
})

test_that("dropping a grid and refitting gives same result", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl + am, .x))
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_grid(tflow, dials::grid_regular)

  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )
  
  tflow <- plug_model(tflow, model)

  mod1_grid <- fit(tflow)
  tflow <- drop_grid(mod1_grid)
  tflow <- plug_grid(tflow, dials::grid_regular)
  mod2_no_grid <- fit(tflow)

  expect_equal(rsplit2df(mod1_grid),
               rsplit2df(mod2_no_grid))
})

test_that("plug_grid resets model fit if trained before adding the grid", {
  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  tflow <- plug_recipe(tidyflow(mtcars), ~ recipes::recipe(mpg ~ cyl + am, .))
  tflow <- plug_model(tflow, model)
  tflow <- fit(tflow)

  tflow <- plug_grid(tflow, dials::grid_regular)
  expect_false(tflow$trained)
  expect_equal(tflow$data, tflow$pre$mold)
  expect_error(pull_tflow_fit(tflow))
})


test_that("Can add plug_grid after model fit and refit", {
  rcp <- ~ recipes::step_log(recipes::recipe(.x, mpg ~ cyl + am), cyl, base = 10)
  tflow <- plug_recipe(tidyflow(mtcars), rcp)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  model <- parsnip::set_engine(
    parsnip::linear_reg(),
    "lm"
  )
  tflow <- plug_model(tflow, model)
  mod1_no_grid <- fit(tflow)

  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )

  tflow2 <- plug_grid(mod1_no_grid, dials::grid_regular)
  tflow2 <- plug_model(drop_model(tflow2), model)
  mod2_grid <- fit(tflow2)

  expect_equal(
    # Compare that the total number of metrics is 18. This means that
    # there's tuning being made for both penalty and mixture and we get
    # metric combinations for all of them.
    unique(vapply(pull_tflow_fit_tuning(mod2_grid)$`.metrics`, nrow,
                  FUN.VALUE = numeric(1))),
    18
  )
})

test_that("plug_grid accepts extra args for the grid function", {
  rcp <- ~ recipes::step_log(recipes::recipe(.x, mpg ~ cyl + am), cyl, base = 10)
  tflow <- plug_recipe(tidyflow(mtcars), rcp)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )

  tflow <- plug_grid(tflow, dials::grid_regular, levels = 4)
  tflow <- plug_model(tflow, model)
  mod <- fit(tflow)

  expect_equal(
    # Compare that the total number of metrics is 18. This means that
    # there's tuning being made for both penalty and mixture and we get
    # metric combinations for all of them.
    unique(vapply(pull_tflow_fit_tuning(mod)$`.metrics`, nrow,
                  FUN.VALUE = numeric(1))),
    32
  )
})

test_that("plug_grid can work with recipe or formula", {
  # This test is more about fit.action_model which calles fit_resample
  # in which it decides to user recipe or formula on whether their NULL
  rcp <- ~ recipes::recipe(.x, mpg ~ cyl + am)
  tflow <- plug_recipe(tidyflow(mtcars, seed = 421351), rcp)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_grid(tflow, dials::grid_regular)  
  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )

  tflow <- plug_model(tflow, model)
  mod1_recipe <- fit(tflow)

  tflow <- drop_recipe(tflow)
  tflow <- plug_formula(tflow, mpg ~ cyl + am)
  mod1_formula <- fit(tflow)

  # rsplit can be compared because all.equal doesn't support
  # it. Instead, we convert to data frame to compare.

  # You were using expect_equal but expect equal
  # uses some sort of all.equal for tibble
  # and throws an error. This will be fixed
  # when this issue is resolved:
  # https://github.com/r-lib/testthat/issues/447
  expect_true(
    all.equal.list(rsplit2df(mod1_recipe),
                   rsplit2df(mod1_formula))
  )

})

test_that("drop_grid removes the action and the result", {
  lm_model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )

  tidyflow <- plug_recipe(tidyflow(mtcars),
                          ~ recipes::recipe(mpg ~ cyl + am, data = .x))
  
  tidyflow <- plug_resample(tidyflow, rsample::vfold_cv)
  tidyflow <- plug_grid(tidyflow, dials::grid_regular)
  tidyflow <- plug_model(tidyflow, lm_model)
  mod1 <- fit(tidyflow)
  tidyflow <- drop_grid(tidyflow)

  # Both are null on dropped tidyflow
  expect_error(pull_tflow_grid(tidyflow))
  expect_null(tidyflow$pre$actions$grid)

  # Both are available on fitted tidyflow
  expect_is(pull_tflow_fit_tuning(mod1), "tune_results")
  expect_is(mod1$pre$actions$grid[[1]], "function")
})

test_that("update a grid specification", {
  tidyflow <- tidyflow()
  tidyflow <- plug_grid(tidyflow, dials::grid_max_entropy)
  tidyflow <- replace_grid(tidyflow, dials::grid_latin_hypercube)

  expect_equal(tidyflow$pre$actions$grid$`dials::grid_latin_hypercube`,
               dials::grid_latin_hypercube)
})

test_that("update a recipe after model fit", {
  rec <- ~ recipes::recipe(mpg ~ cyl + am, .x)
  rec2 <- ~ recipes::recipe(mpg ~ disp + carb, .x)

  lm_model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )

  tidyflow <- tidyflow(mtcars)
  tidyflow <- plug_model(tidyflow, lm_model)
  tidyflow <- plug_resample(tidyflow, rsample::vfold_cv)
  tidyflow <- plug_grid(tidyflow, dials::grid_regular)    
  tidyflow <- plug_recipe(tidyflow, rec)

  tidyflow <- fit(tidyflow)

  # Should clear fitted model
  tidyflow <- replace_recipe(tidyflow, rec2)

  expect_equal(tidyflow$pre$actions$recipe$recipe,
               rlang::as_function(rec2)
               )

  expect_equal(pull_tflow_spec(tidyflow), lm_model)
  expect_equal(
    pull_tflow_mold(tidyflow),
    pull_tflow_rawdata(tidyflow)
  )
})

test_that("cannot add two grid specifications", {
  tidyflow <- tidyflow()
  tidyflow <- plug_grid(tidyflow, dials::grid_regular)

  expect_error(plug_grid(tidyflow, dials::grid_max_entropy),
               "A `grid` action has already been added to this tidyflow")
})

test_that("add/replace_grid check if `...` are named", {
  tidyflow <- tidyflow()

  expect_error(
    plug_grid(tidyflow, dials::grid_regular, 0.8),
    regexp = "Arguments in `...` for `plug_grid` should be uniquely named"
  )

  tidyflow <- plug_grid(tidyflow, dials::grid_regular)

  expect_error(
    replace_grid(tidyflow, dials::grid_max_entropy, 0.8),
    regexp = "Arguments in `...` for `plug_grid` should be uniquely named"
  )
  
})

test_that("Updating a grid after removing one, warns", {
  tidyflow <- plug_grid(tidyflow(), dials::grid_regular)

  expect_warning(
    replace_grid(drop_grid(tidyflow), dials::grid_regular),
    "The tidyflow does not have a grid specification."
  )

})

test_that("Updating a grid doesn't remove anything else", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl + am, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_grid(tflow, dials::grid_regular)

  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )
  
  tflow <- plug_model(tflow, model)

  # The grid
  tflow_grid <- replace_grid(tflow, dials::grid_random)
  expect_true(has_preprocessor_split(tflow))
  expect_true(has_preprocessor_resample(tflow))
  expect_true(has_preprocessor_grid(tflow))
  expect_true(has_spec(tflow))

})

test_that("Removing a grid doesn't remove anything else", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl + am, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_grid(tflow, dials::grid_regular)

  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )
  
  tflow <- plug_model(tflow, model)

  # The grid
  tflow_grid <- drop_grid(tflow)
  expect_true(has_preprocessor_split(tflow))
  expect_true(has_preprocessor_resample(tflow))
  expect_true(has_preprocessor_grid(tflow))
  expect_true(has_spec(tflow))

})

test_that("Name of grid function is always saved as name in the list", {
  # For plug_grid
  tidyflow <- plug_grid(tidyflow(), dials::grid_random)
  expect_true("dials::grid_random" %in% names(tidyflow$pre$actions$grid))

  # For replace_grid
  tidyflow <- plug_grid(tidyflow(), dials::grid_random)
  tidyflow <- replace_grid(tidyflow, dials::grid_max_entropy)
  expect_true("dials::grid_max_entropy" %in% names(tidyflow$pre$actions$grid))
})

test_that("If tune() is present, plug_grid must be present", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl + am, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_resample(tflow, rsample::vfold_cv)

  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )

  tflow <- plug_model(tflow, model)

  expect_error(
    fit(tflow),
    regexp = "The recipe or model has `tune()` parameters but no have grid specification. Did you want `plug_grid()`?", #nolintr
    fixed = TRUE
  )

  model <- parsnip::set_engine(parsnip::linear_reg(), "lm")
  rcp <-
    ~ recipes::step_ns(
      recipes::recipe(mpg ~ cyl + am, .x),
      cyl,
      deg_free = tune::tune()
    )
  tflow <- replace_model(tflow, model)
  tflow <- replace_recipe(tflow, rcp)

  expect_error(
    fit(tflow),
    regexp = "The recipe contains parameters with `tune()` but no grid specification has been made. Did you want `plug_grid`?", #nolintr
    fixed = TRUE
  )
})

test_that("Cannot run grid tuning without resamples", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl + am, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_grid(tflow, dials::grid_regular)

  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )

  tflow <- plug_model(tflow, model)

  expect_error(
    fit(tflow),
    regexp = "The tidyflow does not have a resample specification. Did you want `plug_resample()`?", #nolintr
    fixed = TRUE
  )
})

test_that("parameters on tidyflow returns same tuning params as tuning", {
  tflow <- tidyflow(mtcars, seed = 2315)
  tflow <- plug_recipe(tflow, ~ recipes::recipe(mpg ~ cyl + am, .x))
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_grid(tflow, dials::grid_regular)
  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )
  tflow <- plug_model(tflow, model)

  fit_mod <- fit(tflow)
  all_tuning <- do.call(rbind, pull_tflow_fit_tuning(fit_mod)$`.metrics`)
  # All params from parameters.tidyflow are used in the tuning
  expect_true(
    all(tune::parameters(tflow)$name %in% names(all_tuning))
  )

  rcp <-
    ~ recipes::step_ns(
      recipes::recipe(mpg ~ cyl + am, .x),
      cyl,
      deg_free = tune::tune()
    )

  tflow <- replace_recipe(tflow, rcp)
  fit_mod <- fit(tflow)
  all_tuning <- do.call(rbind, pull_tflow_fit_tuning(fit_mod)$`.metrics`)
  # All params from parameters.tidyflow are used in the tuning
  expect_true(all(tune::parameters(tflow)$name %in% names(all_tuning)))
})

test_that("Tuning is applied with all pre steps", {
  tflow <- tidyflow(mtcars, seed = 2315)
  rcp <-
    ~ recipes::step_ns(
      recipes::recipe(mpg ~ disp + am, .x),
      disp,
      deg_free = tune::tune()
    )
  tflow <- plug_split(tflow, rsample::initial_split)
  tflow <- plug_recipe(tflow, rcp)
  tflow <- plug_resample(tflow, rsample::vfold_cv)
  tflow <- plug_grid(tflow, dials::grid_regular)
  model <- parsnip::set_engine(
    parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
    "glmnet"
  )
  tflow <- plug_model(tflow, model)
  mod1_grid <- fit(tflow)
  tuning_vals <- pull_tflow_fit_tuning(mod1_grid)
  nrow_split <- function(x) {
    unique(
      vapply(x,
             function(.x) {
               nrow(rsample::analysis(.x)) + nrow(rsample::assessment(.x))
             },
             FUN.VALUE = numeric(1))
    )
  }

  # Tuning is done on the training data
  expect_equal(
    nrow_split(tuning_vals$splits),
    nrow(rsample::training(mod1_grid$pre$results$split))
  )

  # Recipe should NOT be applied when tuning grid. It is done
  # via tune_grid automatically. Let's compare that the resample
  # is the same data as the training data.
  training_resamples <-
    lapply(tuning_vals$splits, function(x) {
      tmp_df <- rbind(rsample::analysis(x), rsample::assessment(x))
      tmp_df[order(tmp_df$mpg, tmp_df$disp), ]
    })


  training_data <- pull_tflow_training(mod1_grid, prep = TRUE)
  training_data <- training_data[order(training_data$mpg, training_data$disp), ]

  all_rset_match <- all(
    vapply(training_resamples, function(x) all(training_data == x),
           FUN.VALUE = logical(1))
  )

  expect_true(all_rset_match)

  # Finally, let's compare our results with the manual approach
  # to tune_grid
  set.seed(2315)
  manual_mod1 <-
    tune::tune_grid(
      object = pull_tflow_spec(mod1_grid),
      preprocessor = mod1_grid$pre$results$recipe,
      resamples = pull_tflow_resample(mod1_grid),
      grid = mod1_grid$pre$results$grid
    )

  expect_true(
    all.equal.list(
      tblattr_2_df(tuning_vals),
      tblattr_2_df(manual_mod1)
    )
  )
})

# TODO
# Add test checking that if tune() if specified, the recipe mold should never
# be the last mold to extract. This can happen if it is allowed to run fit
# with a grid and recipe (w/ tun()) but without a resample. Test that it
# cannot be done and add explanation in text why this is important
## test_that("Cannot run grid tuning without resamples", {
##   tflow <- tidyflow(mtcars, seed = 2315)
##   rcp <-
##     ~ recipes::step_ns(
##       recipes::recipe(mpg ~ disp + am, .x),
##       disp,
##       deg_free = tune()
##     )
##   tflow <- plug_split(tflow, rsample::initial_split)
##   tflow <- plug_recipe(tflow, rcp)
##   tflow <- plug_resample(tflow, rsample::vfold_cv)
##   tflow <- plug_grid(tflow, dials::grid_regular)
##   model <- parsnip::set_engine(
##     parsnip::linear_reg(penalty = tune::tune(), mixture = tune::tune()),
##     "glmnet"
##   )
##   tflow <- plug_model(tflow, model)
##   mod1_grid <- fit(tflow)

##   expect_error(
##     fit(tflow),
##     regexp = "The tidyflow does not have a resamples specification. Did you want `plug_resample()`?", #nolintr
##     fixed = TRUE
##   )
## })

test_that("Repeating param names throws error", {
  mod <- plug_split(tidyflow(mtcars), rsample::initial_split)
  mod <- plug_resample(plug_formula(mod, mpg ~ .), rsample::vfold_cv)
  model <-
    parsnip::set_engine(
      parsnip::linear_reg(
        penalty = tune::tune(),
        mixture = tune::tune()),
      "glmnet"
    )
  
  expect_error(
    plug_grid(plug_model(mod, model),
              dials::grid_regular,
              penalty = penalty(),
              penalty = penalty()
              ),
    regexp = "Arguments in `...` for `plug_grid` should be uniquely named"
  )

  model <-
    parsnip::set_engine(
      parsnip::linear_reg(
        penalty = tune::tune("my_penalty"),
        mixture = tune::tune("my_mixture")),
      "glmnet"
    )

  expect_error(
    fit(
      plug_grid(plug_model(mod, model),
                dials::grid_regular,
                penalty = dials::penalty(),
                mixture = dials::mixture()
                )
    ),
    regexp = "At least one parameter does not match any id's in the set: 'penalty', 'mixture'"
  )

  expect_error(
    fit(
      plug_grid(plug_model(mod, model),
                dials::grid_regular,
                my_penalty = dials::penalty(),
                mixture = dials::mixture()
                )
    ),
    regexp = "At least one parameter does not match any id's in the set: 'mixture'"
  )
  
})


test_that("Tuning without specifying values in plug_grid works", {
  # Grid search with tuning parameters in model
  # No need to define the values of the tuning parameters
  # as they have defaults. For example, see dials::penalty()
  mod <- plug_split(tidyflow(mtcars), rsample::initial_split)
  mod <- plug_resample(plug_formula(mod, mpg ~ .), rsample::vfold_cv)
  model <-
    parsnip::set_engine(
      parsnip::linear_reg(
        penalty = tune::tune(),
        mixture = tune::tune()),
      "glmnet"
    )
  
  mod <- plug_grid(plug_model(mod, model), dials::grid_regular, levels = 2)
  res <- fit(mod)

  rows_tuning <-
    unique(
      vapply(pull_tflow_fit_tuning(res)$`.metrics`,
             nrow,
             FUN.VALUE = numeric(1)
             )
    )

  # This matches number of tuning parameters so it should always
  # be 8.
  expect_equal(rows_tuning, 8)

  # Let's just make sure that the parameters names are in the
  # tuning data frame.
  parameters_present <-
    all(
      vapply(pull_tflow_fit_tuning(res)$`.metrics`,
             function(x) all(c("penalty", "mixture") %in% names(x)),
             FUN.VALUE = logical(1)
             )
    )

  expect_true(parameters_present)
})

test_that("Specifying parameters in `...` for plug_grid replaces default values", {
  
  mod <- plug_split(tidyflow(mtcars), rsample::initial_split)
  mod <- plug_resample(plug_formula(mod, mpg ~ .), rsample::vfold_cv)
  model <-
    parsnip::set_engine(
      parsnip::linear_reg(
        penalty = tune::tune(),
        mixture = tune::tune()),
      "glmnet"
    )
  
  mod <- plug_grid(plug_model(mod, model),
                   dials::grid_regular,
                   penalty = dials::penalty(c(-1, 0), trans = NULL),
                   mixture = dials::mixture(c(0, 0.5)),
                   levels = 2
                   )
  res <- fit(mod)

  ## TODO: When pull_grid exists, put it here
  expect_true(between(res$pre$results$grid$penalty, -1, 0))
  expect_true(between(res$pre$results$grid$mixture, 0, 0.5))

  # Also works when both tuning params have different names
  model <-
    parsnip::set_engine(
      parsnip::linear_reg(
        penalty = tune::tune("my_penalty"),
        mixture = tune::tune("my_mixture")),
      "glmnet"
    )

  mod <- replace_grid(replace_model(mod, model),
                   dials::grid_regular,
                   my_penalty = dials::penalty(c(-1, 0), trans = NULL),
                   my_mixture = dials::mixture(c(0, 0.5)),
                   levels = 2
                   )
  res <- fit(mod)

  expect_true(between(res$pre$results$grid$my_penalty, -1, 0))
  expect_true(between(res$pre$results$grid$my_mixture, 0, 0.5))

  # Also works when one tuning param has different names and the other
  # the normal name
  model <-
    parsnip::set_engine(
      parsnip::linear_reg(
        penalty = tune::tune("my_penalty"),
        mixture = tune::tune()),
      "glmnet"
    )

  mod <- replace_grid(replace_model(mod, model),
                      dials::grid_regular,
                      my_penalty = dials::penalty(c(-5, 0), trans = NULL),
                      mixture = dials::mixture(c(0, 0.1)),
                      levels = 2
                      )
  res <- fit(mod)

  expect_true(between(res$pre$results$grid$my_penalty, -5, 0))
  expect_true(between(res$pre$results$grid$mixture, 0, 0.1))
})


test_that("Specifying tuning params in model and recipe works well in both", {
  
  mod <- plug_split(tidyflow(mtcars), rsample::initial_split)
  rec <- ~ recipes::step_ns(recipes::recipe(mpg ~ ., data = .), hp, deg_free = tune())
  mod <- plug_recipe(mod, rec)
  mod <- plug_resample(mod, rsample::vfold_cv)

  model <-
    parsnip::set_engine(
      parsnip::linear_reg(
        penalty = tune::tune(),
        mixture = tune::tune()),
      "glmnet"
    )
  
  mod <- plug_grid(plug_model(mod, model),
                   dials::grid_regular,
                   levels = 2
                   )
  res <- fit(mod)

  # Let's just make sure that the parameters names are in the
  # tuning data frame.
  parameters_and_recipe_present <-
    all(
      vapply(pull_tflow_fit_tuning(res)$`.metrics`,
             function(x) all(c("penalty", "mixture", "deg_free") %in% names(x)),
             FUN.VALUE = logical(1)
             )
    )
  
  expect_true(parameters_and_recipe_present)

  rows_tuning <-
    unique(
      vapply(pull_tflow_fit_tuning(res)$`.metrics`,
             nrow,
             FUN.VALUE = numeric(1)
             )
    )

  # This matches number of tuning parameters so it should always be 16.
  expect_equal(rows_tuning, 16)

  mod <- replace_grid(mod,
                      dials::grid_regular,
                      deg_free = dials::deg_free(c(1, 10)),
                      mixture = dials::mixture(c(0, 0.1)),
                      levels = 2
                      )
  res <- fit(mod)

  expect_true(between(res$pre$results$grid$deg_free, 1, 10))
  expect_true(between(res$pre$results$grid$mixture, 0, 0.1))
})

