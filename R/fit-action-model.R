#' Add a model to a tidyflow
#'
#' @description
#' - `plug_model()` adds a parsnip model to the tidyflow.
#'
#' - `drop_model()` removes the model specification as well as any fitted
#'   model object. Any extra formulas are also removed. Doesn't remove any steps
#'   from the `pre` stage.
#'
#' - `replace_model()` first removes the model then adds the new specification to
#'   the tidyflow.
#'
#' @details
#' `plug_model()` is a required step to construct a minimal tidyflow.
#'
#' @param x A tidyflow.
#'
#' @param spec A parsnip model specification.
#'
#' @param formula An optional formula override to specify the terms of the
#'   model. Typically, the terms are extracted from the formula or recipe
#'   preprocessing methods. However, some models (like survival and bayesian
#'   models) use the formula not to preprocess, but to specify the structure
#'   of the model. In those cases, a formula specifying the model structure
#'   must be passed unchanged into the model call itself. This argument is
#'   used for those purposes.
#'
#' @return
#' `x`, updated with either a new or removed model.
#'
#' @export
#' @examples
#' library(parsnip)
#'
#' # Define two competing model:
#' lm_model <- set_engine(linear_reg(), "lm")
#' regularized_model <- set_engine(lm_model, "glmnet")
#'
#' # Define a minimal tidyflow: data + formula + model
#' wf <-
#'   mtcars %>%
#'   tidyflow() %>%
#'   plug_formula(mpg ~ .) %>% 
#'   plug_model(lm_model)
#'
#' wf
#'
#' # We can drop the model at any time and the remaining steps
#' # are intact
#' drop_model(wf)
#'
#' # We can fit the model with `fit`:
#' fitted <- fit(wf)
#'
#' # Extract the model if needed:
#' fitted %>%
#'   pull_tflow_fit()
#'
#' # If we remove the model from the fitted `tidyflow`,
#' # the fit is dropped:
#' drop_model(fitted)
#'
#' # We could replace the model from the initial tidyflow with
#' # the regularized model with `replace_model`
#' 
#' ## TODO: when https://github.com/cimentadaj/tidyflow/issues/4 is fixed
#' ## replace wf with fitted here.
#' 
#' reg_fitted <-
#'   wf %>%
#'   replace_model(regularized_model) %>%
#'   fit()
#'
#' reg_fitted %>%
#'   pull_tflow_fit()
#'
plug_model <- function(x, spec, formula = NULL) {
  action <- new_action_model(spec, formula)
  plug_action(x, action, "model")
}

#' @rdname plug_model
#' @export
drop_model <- function(x) {
  validate_is_tidyflow(x)

  if (!has_spec(x)) {
    rlang::warn("The tidyflow has no model to remove.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(actions = x$pre$actions,
                        mold = x$pre$mold,
                        seed = x$pre$seed,
                        results = x$pre$results),
    fit = new_stage_fit(),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}


#' @rdname plug_model
#' @export
replace_model <- function(x, spec, formula = NULL) {
  x <- drop_model(x)
  plug_model(x, spec, formula)
}

# ------------------------------------------------------------------------------
fit.action_model <- function(object, x, control, ...) {
  control_parsnip <- control$control_parsnip
  spec <- object$spec
  formula <- object$formula
  resample_res <- x$pre$results$resample
  grid_res <- x$pre$results$grid$grid
  param_res <- x$pre$results$grid$params
  preproc <- x$pre$results$preprocessor



  if (has_tune(spec) && !has_preprocessor_grid(x)) {
    abort("The model contains parameters with `tune()` but no grid specification has been made. Did you want `plug_grid`?") #nolintr
  }

  # It means that they specified a resample and no grid
  if (!is.null(resample_res) && is.null(grid_res)) {
    control_resamples <- control$control_resamples
  
    x$fit$fit$tuning <-
      tune::fit_resamples(object = spec,
                          preprocessor = preproc,
                          resamples = resample_res,
                          control = control_resamples
                          )

    return(x)
    # It means that they specified a resample AND a grid, so tuning is wanted
  } else if (!is.null(resample_res) && !is.null(grid_res)) {
    control_grid <- control$control_grid

    x$fit$fit$tuning <-
      tune::tune_grid(object = spec,
                      preprocessor = preproc,
                      resamples = resample_res,
                      grid = grid_res,
                      control = control_grid,
                      param_info = param_res
                      )
    return(x)
  }

  mold <- x$pre$mold

  if (is.null(mold)) {
    abort("Internal error: No mold exists. `tidyflow` pre stage has not been run.")
  }

  if (is.null(formula)) {
    fit <- fit_from_xy(spec, mold, control_parsnip)
  } else {
    mold <- combine_outcome_preds(mold)
    fit <- fit_from_formula(spec, mold, control_parsnip, formula)
  }

  x$fit$fit$fit <- fit

  # Only the tidyflow is returned
  x
}

fit_from_xy <- function(spec, mold, control_parsnip) {
  fit_xy(spec, x = mold$predictors, y = mold$outcomes, control = control_parsnip)
}

fit_from_formula <- function(spec, mold, control_parsnip, formula) {
  fit(spec, formula = formula, data = mold, control = control_parsnip)
}

# ------------------------------------------------------------------------------

new_action_model <- function(spec, formula) {
  if (!is_model_spec(spec)) {
    abort("`spec` must be a `model_spec`.")
  }

  if (!is.null(formula) && !is_formula(formula)) {
    abort("`formula` must be a formula, or `NULL`.")
  }

  new_action_fit(spec = spec, formula = formula, subclass = "action_model")
}
