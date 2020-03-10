#' Predict from a tidyflow
#'
#' @description
#' This is the `predict()` method for a fit tidyflow object.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @param object A tidyflow that has been fit by [fit.tidyflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on. Usually, this would be extracted from the tidyflow
#'   with \code{\link{pull_tflow_testing}} with \code{prep = TRUE} or
#'   \code{\link{pull_tflow_training}} with \code{prep = TRUE}.
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has.
#'
#' @name predict-tidyflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(rsample)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' tflow <- tidyflow(mtcars)
#' tflow <- plug_split(tflow, initial_split)
#' tflow <- plug_model(tflow, model)
#'
#' rcp <- ~ step_log(recipe(mpg ~ cyl + disp, .), disp)
#'
#' tflow <- plug_recipe(tflow, rcp)
#'
#' tflow <- fit(tflow)
#'
#' # This will automatically `bake()` the recipe on `new_data`,
#' # applying the log step to `disp`, and then fit the regression.
#' predict(tflow, new_data = pull_tflow_testing(tflow, prep = TRUE))
#'
predict.tidyflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  tflow <- object
  tuning <- try(pull_tflow_fit_tuning(tflow), silent = TRUE)

  # If there's a tuning object but no final model
  if (!inherits(tuning, "try-error") && !tflow$trained) {
    abort("You seem to have a model with tuning parameters but not a finalized model. Did you call complete_tidyflow()?")
  }

  # If no tuning object is present but the model is not trained
  if (inherits(tuning, "try-error") && !tflow$trained) {
    abort("Tidyflow has not yet been trained. Did you call fit()?")
  }

  blueprint <- pull_tflow_mold(tflow)$blueprint
  forged <- hardhat::forge(new_data, blueprint)
  new_data <- forged$predictors
  fit <- pull_tflow_fit(tflow)
  predict(fit, new_data, type = type, opts = opts, ...)
}
