test_that("can print empty tidyflow", {
  verify_output(
    test_path("out/test-print-tidyflow-empty.txt"),
    tidyflow()
  )
})

test_that("can print tidyflow with recipe", {
  rec <- ~ recipes::recipe(.x)

  verify_output(
    test_path("out/test-print-tidyflow-recipe.txt"),
    add_recipe(tidyflow(), rec)
  )
})

test_that("can print tidyflow with formula", {
  verify_output(
    test_path("out/test-print-tidyflow-formula.txt"),
    add_formula(tidyflow(), y ~ x)
  )
})

test_that("can print tidyflow with model", {
  model <- parsnip::linear_reg()

  verify_output(
    test_path("out/test-print-tidyflow-model.txt"),
    add_model(tidyflow(), model)
  )
})

test_that("can print tidyflow with model with engine specific args", {
  model <- parsnip::linear_reg(penalty = 0.01)
  model <- parsnip::set_engine(model, "glmnet", dfmax = 5)

  verify_output(
    test_path("out/test-print-tidyflow-model-args.txt"),
    add_model(tidyflow(), model)
  )
})

test_that("can print tidyflow with fit model", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  tidyflow <- tidyflow()
  tidyflow <- add_formula(tidyflow, mpg ~ cyl)
  tidyflow <- add_model(tidyflow, model)

  verify_output(
    test_path("out/test-print-tidyflow-fit.txt"),
    fit(tidyflow, mtcars)
  )
})

test_that("can print tidyflow with >10 recipe steps", {

  rcp_fun <- function(.x) {
    rec <- recipes::recipe(mpg ~ cyl, .x)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
  }

  verify_output(
    test_path("out/test-print-tidyflow-recipe-11-steps.txt"),
    add_recipe(tidyflow(), rcp_fun)
  )


  rcp_fun <- function(.x) {
    rec <- recipes::recipe(mpg ~ cyl, .x)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    rec <- recipes::step_log(rec, cyl)
    # One more step
    rec <- recipes::step_log(rec, cyl)
  }

  verify_output(
    test_path("out/test-print-tidyflow-recipe-12-steps.txt"),
    add_recipe(tidyflow(), rcp_fun)
  )
})
