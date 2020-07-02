test_that("can add a data to a tidyflow", {
  tidyflow <- tidyflow()
  tidyflow <- plug_data(tidyflow, mtcars)

  expect_is(tidyflow$data, "data.frame")
  expect_equal(tidyflow$data, tidyflow$pre$mold)
})

test_that("data is validated", {
  expect_error(plug_data(tidyflow(), 1), "`data` must be a data frame.")
})

test_that("remove tidyflow data", {
  tidyflow_no_data <- tidyflow()
  tidyflow_with_data <- plug_data(tidyflow_no_data, mtcars)
  tidyflow_removed_data <- drop_data(tidyflow_with_data)

  expect_equal(tidyflow_no_data$data, tidyflow_removed_data$data)
  expect_equal(tidyflow_no_data$pre$mold, tidyflow_removed_data$pre$mold)
})

test_that("update a dataframe", {
  tidyflow <- tidyflow()
  tidyflow <- plug_data(tidyflow, mtcars)
  tidyflow <- replace_data(tidyflow, iris)

  expect_equal(tidyflow$data, iris)
  expect_equal(tidyflow$data, tidyflow$pre$mold)
})

test_that("cannot add two data frames", {
  tidyflow <- tidyflow()
  tidyflow <- plug_data(tidyflow, mtcars)

  expect_error(plug_data(tidyflow, iris),
               "A data frame has already been added to this tidyflow")
})
