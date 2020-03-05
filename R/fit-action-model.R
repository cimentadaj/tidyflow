#' Add a model to a tidyflow
#'
#' @description
#' - `add_model()` adds a parsnip model to the tidyflow.
#'
#' - `remove_model()` removes the model specification as well as any fitted
#'   model object. Any extra formulas are also removed. Doesn't remove any steps
#'   from the `pre` stage.
#'
#' - `update_model()` first removes the model then adds the new specification to
#'   the tidyflow.
#'
#' @details
#' `add_model()` is a required step to construct a minimal tidyflow.
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
#' wf <- add_model(wf, lm_model)
#' wf
#'
#' wf <- add_formula(wf, mpg ~ .)
#' wf
#'
#' remove_model(wf)
#'
#' fitted <- fit(wf)
#' fitted
#'
#' remove_model(fitted)
#'
#' remove_model(wf)
#'
#' update_model(wf, regularized_model)
#' update_model(fitted, regularized_model)
#'
add_model <- function(x, spec, formula = NULL) {
  action <- new_action_model(spec, formula)
  add_action(x, action, "model")
}

#' @rdname add_model
#' @export
remove_model <- function(x) {
  validate_is_tidyflow(x)

  if (!has_spec(x)) {
    rlang::warn("The tidyflow has no model to remove.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(x$pre$actions, mold = x$pre$mold),
    fit = new_stage_fit(),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}


#' @rdname add_model
#' @export
update_model <- function(x, spec, formula = NULL) {
  x <- remove_model(x)
  add_model(x, spec, formula)
}

# ------------------------------------------------------------------------------
fit.action_model <- function(object, wflow, control, ...) {
  control_parsnip <- control$control_parsnip
  spec <- object$spec
  formula <- object$formula
  resample_res <- wflow$pre$actions$resample$rset_res

  # It means that they specified a resample
  if (!is.null(resample_res)) {
    obj <- wflow$pre$actions$recipe$recipe_res %||% wflow$pre$actions$formula

    resampled <-
      tune::fit_resamples(obj,
                          model = spec,
                          resamples = resample_res,
                          control = control,
                          ...
                          )
  }

  mold <- wflow$pre$mold

  if (is.null(mold)) {
    abort("Internal error: No mold exists. `tidyflow` pre stage has not been run.")
  }

  if (is.null(formula)) {
    fit <- fit_from_xy(spec, mold, control_parsnip)
  } else {
    mold <- combine_outcome_preds(mold)
    fit <- fit_from_formula(spec, mold, control_parsnip, formula)
  }

  wflow$fit$fit <- fit

  # Only the tidyflow is returned
  wflow
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
