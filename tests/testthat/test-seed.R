## TODO eliminate all pipes from here
rcp <-
  ~ .x %>%
    recipes::recipe(mpg ~ cyl) %>%
    recipes::step_log(cyl, base = 10)

test_that("Not specifying the seed returns different results using split", {

    mod <-
      mtcars %>%
      tidyflow() %>%
      plug_split(rsample::initial_split) %>%
      plug_recipe(rcp) %>%
      plug_model(parsnip::set_engine(parsnip::linear_reg(), "lm"))

      first_mod <- mod %>% fit()
      second_mod <- mod %>% fit()

      expect_false(identical(first_mod, second_mod))
})

test_that("Specifying the seed returns the same results using split", {

    mod <-
      mtcars %>%
      tidyflow(seed = 25131) %>%
      plug_split(rsample::initial_split) %>%
      plug_recipe(rcp) %>%
      plug_model(parsnip::set_engine(parsnip::linear_reg(), "lm"))

      first_mod <- mod %>% fit()
      second_mod <- mod %>% fit()

      # Setting the time elapsed to NULL, since there can be very minor
      # differences in time fitting the model for comparison.
      expect_equal(strip_elapsed(first_mod), strip_elapsed(second_mod))
})

test_that("Not specifying the seed returns different results using resample", {

    mod <-
      mtcars %>%
      tidyflow() %>%
      plug_resample(rsample::vfold_cv) %>%
      plug_recipe(rcp) %>%
      plug_model(parsnip::set_engine(parsnip::linear_reg(), "lm"))

      first_mod <- mod %>% fit()
      second_mod <- mod %>% fit()

      expect_false(identical(first_mod, second_mod))
})

test_that("Specifying the seed returns the same results using resample", {

  mod <-
    mtcars %>%
    tidyflow(seed = 25131) %>%
    plug_resample(rsample::vfold_cv) %>%
    plug_recipe(rcp) %>%
    plug_model(parsnip::set_engine(parsnip::linear_reg(), "lm"))

    ## all.equal doesn't work with list columns from rsplit
    ## `rsplit2df` simply accesses the rset_res and tuning_res
    ## and converts them to data frame so that expect_equal
    ## can pass.
    first_mod <- mod %>% fit() %>% rsplit2df()
    second_mod <- mod %>% fit() %>% rsplit2df()

    # You were using expect_equal but expect equal
    # uses some sort of all.equal for tibble
    # and throws an error. This will be fixed
    # when this issue is resolved:
    # https://github.com/r-lib/testthat/issues/447
    expect_true(
      all.equal.list(first_mod, second_mod)
    )

})
