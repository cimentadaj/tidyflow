#' Fit the best model from a tuning grid
#' 
#' @param x A tidyflow
#' @param best_params A 1 row tibble with the best parameters to fit the final
#' model. Ideally, the result of \code{\link[tune]{select_best}}.
#' @param control A \code{\link{control_tidyflow}} object. The
#' \code{\link[parsnip]{control_parsnip}} control object inside
#' \code{\link{control_tidyflow}} is passed to
#' \code{\link[parsnip]{fit}} or \code{\link[parsnip]{fit}}.
#'
#' @details The finalized model is fitted on the training data if
#' \code{plug_split} was specified otherwise on the complete data.
#' 
#' @return The tidyflow `object` updated with the fitted best model. Can be
#' extracted with \code{\link{pull_tflow_fit}} and used to predict on the
#' training or test data with \code{\link{predict_training}} or
#' \code{\link{predict_testing}}
#'
#' @export
#' @examples
#' library(parsnip)
#' library(tune)
#' library(dials)
#' library(rsample)
#'
#' # Fit a regularized regression through a grid search.
#' reg_mod <- set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet")
#' tuned_res <-
#'  mtcars %>%
#'   tidyflow() %>% 
#'   plug_resample(vfold_cv, v = 2) %>% 
#'   plug_formula(mpg ~ .) %>% 
#'   plug_model(reg_mod) %>%
#'   plug_grid(grid_regular, levels = 1) %>%
#'   fit()
#'
#' # Extract the best tuning fit:
#' best_params <- select_best(pull_tflow_fit_tuning(tuned_res), "rmse")
#'
#' # Finalize the best model and refit on the whole dataset
#' final_model <-
#'   tuned_res %>%
#'   complete_tflow(best_params)
#'
#' # Extract final model with:
#' pull_tflow_fit(final_model)
#'
#' # Since there was no `plug_split`, the final model is fitted
#' # entirely on the data (no training/testing). If you try to predict
#' # on either one, it will not work:
#' \dontrun{
#' final_model %>%
#'   predict_training()
#' 
#' # Add a split step, fit again and then finalize the model
#' # to predict on the training set
#' tuned_split <-
#'   tuned_res %>%
#'   plug_split(initial_split) %>%
#'   fit()
#'
#' tuned_split %>%
#'  complete_tflow(best_params) %>%
#'  predict_training()
#' }
#' 
complete_tflow <- function (x, best_params, control = control_tidyflow()) {
  if (!inherits(x, "tidyflow")) {
    stop("`x` should be a tidyflow")
  }

  if (!(has_fit_tuning(x))) {
    abort("The tidyflow must be tuned to be able to complete the final model") #nolintr
  }

  if (inherits(pull_tflow_fit_tuning(x), "resample_results")) {
    abort("`complete_tflow` cannot finalize a model with a resampling result. To finalize a model you need a tuning result. Did you want `plug_grid`?") #nolintr
  }

  parsnip::check_final_param(best_params)
  mod <- tidyflow::pull_tflow_spec(x)
  mod <- tune::finalize_model(mod, best_params)
  x$fit$actions$model$spec <- mod
  
  if (has_preprocessor_recipe(x)) {
    rec <- tidyflow::pull_tflow_preprocessor(x)
    dt <- combine_outcome_preds(pull_tflow_mold(x))
    rec <- tune::finalize_recipe(rec(dt), best_params)
    x$pre$actions$recipe$recipe_res <- rec
    x$pre$mold <- hardhat::mold(rec, dt)
  }

  form <- x$fit$actions$formula
  mold <- x$pre$mold
  if (is.null(form)) {
    fit <- fit_from_xy(mod, mold, control$control_parsnip)
  } else {
    mold <- combine_outcome_preds(mold)
    fit <- fit_from_formula(mod, mold, control$control_parsnip, form)
  }

  x$fit$fit$fit <- fit
  x$trained <- TRUE
  x
}

