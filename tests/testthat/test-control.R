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
