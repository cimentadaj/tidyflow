check_tidyflow <- function(tidyflow) {
  expect_is(tidyflow, "tidyflow")

  if (has_raw_data(tidyflow)) {
    expect_is(tidyflow$data, "data.frame")
  } else {
    expect_is(tidyflow$data, "NULL")
  }
  
  expect_is(tidyflow$pre, "stage_pre")
  expect_is(tidyflow$fit, "stage_fit")
  expect_is(tidyflow$post, "stage_post")
  expect_equal(tidyflow$pre$actions, list())
  expect_equal(tidyflow$pre$mold, tidyflow$data)
  expect_equal(tidyflow$fit$actions, list())
  expect_equal(tidyflow$fit$fit, NULL)
  expect_equal(tidyflow$post$actions, list())
}

test_that("can create a basic tidyflow", {
  # Without data frame
  tidyflow <- tidyflow()
  check_tidyflow(tidyflow)

  # With data frame
  tidyflow <- tidyflow(mtcars)
  check_tidyflow(tidyflow)
})

test_that("tidyflow can only begin with a data frame", {
  expect_error(
    tidyflow(list()),
    regexp = "A tidyflow can only begin with a data frame; `data` must a data frame", #nolintr
    fixed = TRUE
  )
})

test_that("tidyflow must be the first argument when adding actions", {
  rec <- ~ recipes::recipe(mpg ~ cyl, .x)
  mod <- parsnip::linear_reg()

  expect_error(add_formula(1, mpg ~ cyl), "must be a tidyflow")
  expect_error(add_recipe(1, rec), "must be a tidyflow")
  expect_error(add_model(1, mod), "must be a tidyflow")
})

test_that("constructor validates input", {
  expect_error(new_tidyflow(pre = 1), "must be a `stage`")
  expect_error(new_tidyflow(fit = 1), "must be a `stage`")
  expect_error(new_tidyflow(post = 1), "must be a `stage`")

  expect_error(new_tidyflow(trained = 1), "must be a single logical value")
})
