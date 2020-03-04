check_workflow <- function(workflow) {
  expect_is(workflow, "workflow")

  if (has_raw_data(workflow)) {
    expect_is(workflow$data, "data.frame")
  } else {
    expect_is(workflow$data, "NULL")
  }
  
  expect_is(workflow$pre, "stage_pre")
  expect_is(workflow$fit, "stage_fit")
  expect_is(workflow$post, "stage_post")
  expect_equal(workflow$pre$actions, list())
  expect_equal(workflow$pre$mold, workflow$data)
  expect_equal(workflow$fit$actions, list())
  expect_equal(workflow$fit$fit, NULL)
  expect_equal(workflow$post$actions, list())
}

test_that("can create a basic workflow", {
  # Without data frame
  workflow <- workflow()
  check_workflow(workflow)

  # With data frame
  workflow <- workflow(mtcars)
  check_workflow(workflow)
})

test_that("workflow can only begin with a data frame", {
  expect_error(
    workflow(list()),
    regexp = "A workflow can only begin with a data frame; `data` must a data frame", #nolintr
    fixed = TRUE
  )
})

test_that("workflow must be the first argument when adding actions", {
  rec <- ~ recipes::recipe(mpg ~ cyl, .x)
  mod <- parsnip::linear_reg()

  expect_error(add_formula(1, mpg ~ cyl), "must be a workflow")
  expect_error(add_recipe(1, rec), "must be a workflow")
  expect_error(add_model(1, mod), "must be a workflow")
})

test_that("constructor validates input", {
  expect_error(new_workflow(pre = 1), "must be a `stage`")
  expect_error(new_workflow(fit = 1), "must be a `stage`")
  expect_error(new_workflow(post = 1), "must be a `stage`")

  expect_error(new_workflow(trained = 1), "must be a single logical value")
})
