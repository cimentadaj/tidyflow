test_that("can create a basic tidyflow control object", {
  expect_is(control_tidyflow(), "control_tidyflow")
})

test_that("default parsnip control is created", {
  expect_equal(control_tidyflow()$control_parsnip, parsnip::control_parsnip())
})

test_that("parsnip control is validated", {
  expect_error(
    control_tidyflow(control_parsnip = 1),
    "must be a 'control_parsnip' object"
  )
})

test_that("default resamples control is created", {
  expect_equal(control_tidyflow()$control_resamples,
               tune::control_resamples()
               )
})

test_that("resamples/grid control is validated", {
  expect_error(
    control_tidyflow(control_resamples = 1),
    "must be a 'control_resamples' object"
  )

  expect_error(
    control_tidyflow(control_grid = 1),
    "must be a 'control_grid' object"
  )
})
