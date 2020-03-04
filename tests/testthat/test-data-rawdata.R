test_that("can add a data to a workflow", {
  workflow <- workflow()
  workflow <- add_data(workflow, mtcars)

  expect_is(workflow$data, "data.frame")
  expect_equal(workflow$data, workflow$pre$mold)
})

test_that("data is validated", {
  expect_error(add_data(workflow(), 1), "`data` must be a data frame.")
})

test_that("remove workflow data", {
  workflow_no_data <- workflow()
  workflow_with_data <- add_data(workflow_no_data, mtcars)
  workflow_removed_data <- remove_data(workflow_with_data)

  expect_equal(workflow_no_data$data, workflow_removed_data$data)
  expect_equal(workflow_no_data$pre$mold, workflow_removed_data$pre$mold)
})

test_that("update a dataframe", {
  workflow <- workflow()
  workflow <- add_data(workflow, mtcars)
  workflow <- update_data(workflow, iris)

  expect_equal(workflow$data, iris)
  expect_equal(workflow$data, workflow$pre$mold)
})

test_that("cannot add two data frames", {
  workflow <- workflow()
  workflow <- add_data(workflow, mtcars)

  expect_error(add_data(workflow, iris),
               "A data frame has already been added to this workflow")
})
