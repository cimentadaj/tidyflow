#' Fit the best model from a tuning grid
#' 
#' @param x A tidyflow
#' @param metric The metric of reference from which to pick the best model
#' @param ... Extra arguments passed to
#' \code{\link[tune:show_best]{select_by_one_std_err}} or
#' \code{\link[tune:show_best]{select_by_pct_loss}}
#' @param best_params A 1 row tibble with the best parameters to fit the final
#' model. Should have the same format as the result of
#' \code{\link[tune:show_best]{select_best}},
#' \code{\link[tune:show_best]{select_by_one_std_err}} or
#' \code{\link[tune:show_best]{select_by_pct_loss}}. If \code{best_params} is specified,
#' the \code{method}, \code{metric} and \code{...} arguments are ignored.
#' @param method which method to use. The possible values are
#' \code{\link[tune:show_best]{select_best}}, \code{\link[tune:show_best]{select_by_one_std_err}} or
#' \code{\link[tune:show_best]{select_by_pct_loss}}. By default, it uses
#' \code{\link[tune:show_best]{select_best}}.
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
#' \dontrun{
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
#' # Finalize the best model and refit on the whole dataset
#' final_model <- complete_tflow(tuned_res, metric = "rmse")
#'
#' # complete_tflow uses tune::select_best as the default method. However,
#' # tune::select_by_one_std_err and
#' # tune::select_by_pct_loss can be used. These need to specify the metric and
#' # the tuning value from which to sort the selection. For example:
#' final_model_stderr <- complete_tflow(tuned_res,
#'                                      metric = "rmse",
#'                                      method = "select_by_one_std_err",
#'                                      penalty)
#'
#' # select_by_one_std_err finalizs the best model with the simplest tuning
#' # values within one standard deviation from most optimal
#' # combination. For more information on these methods, see
#' # ?select_best
#'
#' # You can also specify the best parameters, in case you want
#' # to override the automatic extraction of the best fit. If you
#' # specify `best_params` it will override all other arguments
#'
#' best_params <- select_best(pull_tflow_fit_tuning(tuned_res), metric = "rmse")
#' final_model_custom <- complete_tflow(tuned_res, best_params = best_params)
#'
#' # To see the final tuning values, extract the model spec
#' pull_tflow_spec(final_model)
#'
#' # To extract the final fitted model:
#' pull_tflow_fit(final_model)
#'
#' # Since there was no `plug_split`, the final model is fitted
#' # entirely on the data (no training/testing). If you try to predict
#' # on either one, it will not work:
#' final_model %>%
#'   predict_training()
#' 
#' # Add a split step, fit again and then finalize the model
#' # to predict on the training set
#' tuned_split <-
#'   tuned_res %>%
#'   replace_grid(grid_regular) %>% 
#'   plug_split(initial_split) %>%
#'   fit()
#'
#' tuned_split %>%
#'  complete_tflow(metric = "rmse") %>%
#'  predict_training()
#' }
#' 
complete_tflow <- function (x,
                            metric,
                            ...,
                            best_params = NULL,
                            method = c("select_best", "select_by_one_std_err", "select_by_pct_loss"),
                            control = control_tidyflow()) {
  
  if (!inherits(x, "tidyflow")) {
    stop("`x` should be a tidyflow")
  }

  if (!(has_fit_tuning(x))) {
    abort("The tidyflow must be tuned to be able to complete the final model") #nolintr
  }

  if (inherits(pull_tflow_fit_tuning(x), "resample_results")) {
    abort("`complete_tflow` cannot finalize a model with a resampling result. To finalize a model you need a tuning result. Did you want `plug_grid`?") #nolintr
  }

  select_fun <- match.arg(method)

  if (is.null(best_params)) {
    tune_grid <- pull_tflow_fit_tuning(x)
    raw_select_fun <- getExportedValue("tune", select_fun)
    best_params <- raw_select_fun(x = tune_grid, metric = metric, ...)
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
