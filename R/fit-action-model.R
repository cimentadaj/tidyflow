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
#' lm_model <- linear_reg()
#' lm_model <- set_engine(lm_model, "lm")
#'
#' regularized_model <- set_engine(lm_model, "glmnet")
#'
#' wf <- tidyflow(mtcars)
#' wf <- plug_model(wf, lm_model)
#' wf
#'
#' wf <- plug_formula(wf, mpg ~ .)
#' wf
#'
#' drop_model(wf)
#'
#' fitted <- fit(wf)
#' fitted
#'
#' drop_model(fitted)
#'
#' drop_model(wf)
#'
#' replace_model(wf, regularized_model)
#' replace_model(fitted, regularized_model)
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
fit.action_model <- function(object, tflow, control, ...) {
  control_parsnip <- control$control_parsnip
  spec <- object$spec
  formula <- object$formula
  resample_res <- tflow$pre$results$resample
  grid_res <- tflow$pre$results$grid

  # It means that they specified a resample and no grid
  if (!is.null(resample_res) && is.null(grid_res)) {
    control_resamples <- control$control_resamples
    obj <- tflow$pre$results$recipe %||% tflow$pre$actions$formula$formula

    tflow$fit$fit$tuning <-
      tune::fit_resamples(object = spec,
                          preprocessor = obj,
                          resamples = resample_res,
                          control = control_resamples
                          )
    
    return(tflow)
    # It means that they specified a resample AND a grid, so tuning is wanted
  } else if (!is.null(resample_res) && !is.null(grid_res)) {
    control_grid <- control$control_grid
    obj <- tflow$pre$results$recipe %||% tflow$pre$actions$formula$formula

    tflow$fit$fit$tuning <-
      tune::tune_grid(object = spec,
                      preprocessor = obj,
                      resamples = resample_res,
                      grid = grid_res,
                      control = control_grid
                      )

    return(tflow)
  }

  mold <- tflow$pre$mold

  if (is.null(mold)) {
    abort("Internal error: No mold exists. `tidyflow` pre stage has not been run.")
  }

  if (is.null(formula)) {
    fit <- fit_from_xy(spec, mold, control_parsnip)
  } else {
    mold <- combine_outcome_preds(mold)
    fit <- fit_from_formula(spec, mold, control_parsnip, formula)
  }

  tflow$fit$fit <- fit

  # Only the tidyflow is returned
  tflow
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
