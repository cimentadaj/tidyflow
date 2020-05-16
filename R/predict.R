#' Predict from a tidyflow
#'
#' @description
#' This is the `predict()` method for a fit tidyflow object. In addition,
#' when a split is specifid, `predict_training` and `predict_testing`
#' automatically predict and apply any preprocessing to the training
#' and testing data.
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
#' predict(tflow, new_data = pull_tflow_testing(tflow))
#'
#' # More automatic
#' predict_testing(tflow)
#'
#' predict_training(tflow)
#'
predict.tidyflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  x <- object
  tuning <- try(pull_tflow_fit_tuning(x), silent = TRUE)

  # If there's a tuning object but no final model
  if (!inherits(tuning, "try-error") && !x$trained) {
    abort("You seem to have a model with tuning parameters or a resample but not a finalized model. Did you call complete_tflow()?")
  }

  # If no tuning object is present but the model is not trained
  if (inherits(tuning, "try-error") && !x$trained) {
    abort("Tidyflow has not yet been trained. Did you call fit()?")
  }

  blueprint <- pull_tflow_mold(x)$blueprint
  forged <- hardhat::forge(new_data, blueprint)
  new_data <- forged$predictors
  fit <- pull_tflow_fit(x)
  predict(fit, new_data, type = type, opts = opts, ...)
}

#' @rdname predict-tidyflow
#' @export
predict_training <- function(object, type = NULL, opts = list(), ...) {
  if (!has_preprocessor_split(object)) {
    rlang::abort("`predict_training` can only work when a split preprocessor has been specifid. Did you want `plug_split`?")
  }

  tr_dt <- pull_tflow_training(object)
  res <-
    tibble::as_tibble(
      cbind(
        tr_dt,
        predict(object, new_data = tr_dt, type = type, opts = opts, ...)
      )
    )

  res
}

#' @rdname predict-tidyflow
#' @export
predict_testing <- function(object, type = NULL, opts = list(), ...) {
  if (!has_preprocessor_split(object)) {
    rlang::abort("`predict_testing` can only work when a split preprocessor has been specifid. Did you want `plug_split`?")
  }

  tst_dt <- pull_tflow_testing(object)

  res <-
    tibble::as_tibble(
      cbind(
        tst_dt,
        predict(object, new_data = tst_dt, type = type, opts = opts, ...)
      )
    )

  res
}
