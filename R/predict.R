#' Predict from a tidyflow
#'
#' @description
#' This is the `predict()` method for a fit tidyflow object. The nice thing
#' about predicting from a tidyflow is that it will:
#' 
#' - Preprocess `new_data` using the preprocessing method specified when the
#'   tidyflow was created and fit. This is accomplished using [
#'   hardhat::forge()], which will apply any formula preprocessing or call
#'   [recipes::bake()] if a recipe was supplied. For safety reasons,
#'   if a split was specified, the user should always use
#'   \code{\link{pull_tflow_testing}} to extract the raw testing data
#'   from the tidyflow and pass it to `new_data`.
#'
#' - Call [parsnip::predict.model_fit()] for you using the underlying fit
#'   parsnip model.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @param object A tidyflow that has been fit by [fit.tidyflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on. Usually, this would be extracted from the tidyflow
#'   with \code{\link{pull_tflow_testing}}
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has,
#' if `new_data` was specified, or a data frame of model predictions with
#' as many rows as the testing data extracted with the split specification.
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
predict.tidyflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  tflow <- object

  if (!tflow$trained) {
    abort("Tidyflow has not yet been trained. Do you need to call `fit()`?")
  }

  blueprint <- tflow$pre$mold$blueprint
  forged <- hardhat::forge(new_data, blueprint)
  new_data <- forged$predictors
  fit <- pull_tflow_fit(tflow)
  predict(fit, new_data, type = type, opts = opts, ...)
}
