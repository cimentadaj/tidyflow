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
               # coerce_control is temporary. See # TODO on coerce_control
               coerce_control(tune::control_resamples(), "control_resamples")
               )
})

# TODO
# When https://github.com/tidymodels/tune/issues/183 is fixed
# uncomment this test. Right now you're forcing all input
# to be coerced to the class.
# The issue is now fixed: waiting for tune to be released to CRAN to
# run this
## test_that("resamples control is validated", {
##   expect_error(
##     control_tidyflow(control_resamples = 1),
##     "must be a 'control_resamples' object"
##   )
## })
