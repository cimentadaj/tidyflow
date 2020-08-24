tflow <- plug_formula(plug_split(tidyflow(mtcars), rsample::initial_split), mpg ~ .)
tflow <- plug_grid(plug_resample(tflow, rsample::vfold_cv, v = 2), dials::grid_regular, levels = 1)
tflow <- plug_model(tflow, parsnip::set_engine(parsnip::linear_reg(), "lm"))

test_that("parameters without any tune returns empty", {
  baseline <- structure(list(name = character(0), id = character(0), source = character(0), 
                             component = character(0), component_id = character(0), object = list()), row.names = integer(0), class = c("parameters", 
                                                                                                                                        "tbl_df", "tbl", "data.frame"))

  # No tuning parameters
  expect_identical(baseline, parameters(tflow))
})

# But if we add tuning parameters, we can check which ones:
tflow <- plug_recipe(drop_formula(tflow),
                     ~ recipes::step_ns(recipes::recipe(mpg ~ ., data = .), hp,
                                        deg_free = tune::tune()))

test_that("Adding a tune in a recipe without fitting returns the tuning param", {

  baseline <- structure(list(name = "deg_free", id = "deg_free", source = "recipe", 
                             component = "step_ns", component_id = "", object = list(
                               structure(list(type = "integer", range = list(lower = 1L, 
                                                                             upper = 15L), inclusive = c(lower = TRUE, upper = TRUE
                                                                                                         ), trans = NULL, default = dials::unknown(), label = c(spline_degree = "Piecewise Polynomial Degree"), 
                                              finalize = NULL), class = c("quant_param", "param"
                                                                          )))), row.names = c(NA, -1L), class = c("parameters", 
                                                                                                                  "tbl_df", "tbl", "data.frame"))

  param_correct <- parameters(fit(tflow))
  param_correct$component_id <- ""
  expect_identical(baseline, param_correct)
})

model <- parsnip::set_engine(parsnip::linear_reg(penalty = tune::tune(),
                                                 mixture = tune::tune()),
                             "glmnet")
tflow <- replace_model(tflow, model)
tflow <- fit(tflow)

test_that("Adding tune in recipe + model without fitting returns the tuning params", {
  # parameters extracts both the tuning parameters from the recipe and
  # model. Since having the three parameters is too long for `dput`
  # I just check the properties of the parameters
  expect_equal(ncol(parameters(tflow)), 6)
  expect_equal(nrow(parameters(tflow)), 3)
  expect_named(parameters(tflow))
  expect_true(all(parameters(tflow)$name %in% c("penalty", "mixture", "deg_free")))
  expect_true(all(parameters(tflow)$source %in% c("model_spec", "model_spec", "recipe")))
})

test_that("Tune inside recipe + model after fitting returns the tuning params", {
  correct_param <- parameters(tflow)
  expect_equal(ncol(correct_param), 6)
  expect_equal(nrow(correct_param), 3)
  expect_named(correct_param)
  expect_true(all(correct_param$name %in% c("penalty", "mixture", "deg_free")))
  expect_true(all(correct_param$source %in% c("model_spec", "model_spec", "recipe")))
})
