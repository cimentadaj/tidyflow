#' Predict from a workflow
#'
#' @description
#' This is the `predict()` method for a fit workflow object. The nice thing
#' about predicting from a workflow is that it will:
#' 
#' - Preprocess `new_data` using the preprocessing method specified when the
#'   workflow was created and fit. This is accomplished using [
#'   hardhat::forge()], which will apply any formula preprocessing or call
#'   [recipes::bake()] if a recipe was supplied. For safety reasons,
#'   if a split was specified, the user should always use
#'   \code{\link{pull_workflow_testing}} to extract the raw testing data
#'   from the workflow and pass it to `new_data`.
#'
#' - Call [parsnip::predict.model_fit()] for you using the underlying fit
#'   parsnip model.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @param object A workflow that has been fit by [fit.workflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on. Usually, this would be extracted from the workflow
#'   with \code{\link{pull_workflow_testing}}
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has,
#' if `new_data` was specified, or a data frame of model predictions with
#' as many rows as the testing data extracted with the split specification.
#'
#' @name predict-workflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(rsample)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' wflow <- workflow(mtcars)
#' wflow <- add_split(wflow, initial_split)
#' wflow <- add_model(wflow, model)
#'
#' rcp <- ~ step_log(recipe(mpg ~ cyl + disp, .), disp)
#'
#' wflow <- add_recipe(wflow, rcp)
#'
#' wflow <- fit(wflow)
#'
#' # This will automatically `bake()` the recipe on `new_data`,
#' # applying the log step to `disp`, and then fit the regression.
#' predict(wflow, new_data = pull_workflow_testing(wflow))
#'
predict.workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  wflow <- object

  if (!wflow$trained) {
    abort("Workflow has not yet been trained. Do you need to call `fit()`?")
  }

  blueprint <- wflow$pre$mold$blueprint
  forged <- hardhat::forge(new_data, blueprint)
  new_data <- forged$predictors
  fit <- wflow$fit$fit
  predict(fit, new_data, type = type, opts = opts, ...)
}
