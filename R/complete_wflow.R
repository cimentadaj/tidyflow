#' Fit the best model from a tuning grid
#' 
#' @details
#' In the future, there will also be _postprocessing_ steps that can be added
#' after the model has been fit.
#' @param x A tidyflow
#' @param best_params A 1 row tibble with the best parameters to fit the final
#' model. Ideally, the result of \code{\link[tune]{select_best}}.
#' @param control A [control_tidyflow()] object
#' 
#' @return
#' 
#' The tidyflow `object`, updated with the fitted best model. Can be extracted
#' with \code{\link{pull_tflow_fit}}.
#'
#' @export
#' @examples
#' library(parsnip)
#' library(tune)
#' library(dials)
#' library(rsample)
#'
#' # Fit a regularized regression through a grid search.
#' # Do this by updating the already defined model:
#' new_mod <- set_engine(linear_reg(penalty = tune(), mixture = tune()),
#'                       "glmnet")
#' tuned_res <-
#'  mtcars %>%
#'   tidyflow() %>% 
#'   plug_resample(vfold_cv) %>% 
#'   plug_formula(mpg ~ .) %>% 
#'   plug_model(new_mod) %>%
#'   plug_grid(grid_regular, levels = 2) %>%
#'   fit()
#'
#' # Extract the tuning fit:
#' best_params <- select_best(pull_tflow_fit_tuning(tuned_res), "rmse")
#'
#' final_model <-
#'   tuned_res %>%
#'   complete_tflow(best_params)
#'
#' # Extract final model with:
#' pull_tflow_fit(final_model)
#' 
complete_tflow <- function (x, best_params, control = control_tidyflow()) {
  if (!inherits(x, "tidyflow")) {
    stop("`x` should be a tidyflow")
  }

  if (!(has_fit_tuning(x))) {
    abort("The tidyflow must be tuned to be able to complete the final model") #nolintr
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
  x
}
