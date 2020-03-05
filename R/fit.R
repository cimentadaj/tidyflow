#' Fit a tidyflow object
#'
#' @description
#' Fitting a tidyflow currently involves two main steps:
#'
#' - Preprocessing the data using a formula preprocessor, or by calling
#'   [recipes::prep()] on a recipe.
#'
#' - Fitting the underlying parsnip model using [parsnip::fit.model_spec()].
#'
#' @details
#' In the future, there will also be _postprocessing_ steps that can be added
#' after the model has been fit.
#'
#' @param object A tidyflow
#'
#' @param ... Not used
#'
#' @param control A [control_tidyflow()] object
#'
#' @return
#' The tidyflow `object`, updated with a fit parsnip model in the
#' `object$fit$fit` slot.
#'
#' @name fit-tidyflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' formula_tidyflow <-
#'  mtcars %>%
#'  tidyflow() %>%
#'  add_formula(mpg ~ cyl + log(disp)) %>%
#'  add_model(model)
#' 
#' fit(formula_tidyflow)
#'
#' recipe_tidyflow <-
#'  formula_tidyflow %>%
#'  drop_formula() %>% 
#'  add_recipe(~ recipe(mpg ~ cyl + disp, .x) %>% step_log(disp))
#'
#' fit(recipe_tidyflow)
#'

fit.tidyflow <- function(object, ..., control = control_tidyflow()) {
  tidyflow <- object

  if (!has_raw_data(tidyflow)) {
    abort("`data` must be specified to fit a tidyflow; Do you need `add_data`?")
  }

  ellipsis::check_dots_empty()
  validate_has_minimal_components(tidyflow)

  tidyflow <- .fit_pre(tidyflow)
  tidyflow <- .fit_model(tidyflow, control)

  # Eh? Predictions during the fit?
  # pred <- result$pred
  # result <- fit_post(tidyflow, pred)

  tidyflow$trained <- TRUE

  tidyflow
}

# ------------------------------------------------------------------------------

#' Internal tidyflow functions
#'
#' `.fit_pre()` and `.fit_model()` are internal tidyflow functions for
#' _partially_ fitting a tidyflow object. They are only exported for usage by
#' the tuning package, [tune](https://github.com/tidymodels/tune), and the
#' general user should never need to worry about them.
#'
#' @param tidyflow A tidyflow
#'
#'   For `.fit_pre()`, this should be a fresh tidyflow.
#'
#'   For `.fit_model()`, this should be a tidyflow that has already been trained
#'   through `.fit_pre()`.
#'
#' @param data A data frame of predictors and outcomes to use when fitting the
#'   tidyflow
#'
#' @param control A [control_tidyflow()] object
#'
#' @name tidyflows-internals
#' @keywords internal
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' base_tidyflow <- tidyflow(mtcars)
#' base_tidyflow <- add_model(base_tidyflow, model)
#'
#' formula_tidyflow <- add_formula(base_tidyflow, mpg ~ cyl + log(disp))
#'
#' partially_fit_tidyflow <- .fit_pre(formula_tidyflow)
#' fit_tidyflow <- .fit_model(partially_fit_tidyflow, control_tidyflow())
.fit_pre <- function(wflow) {
  n <- length(wflow[["pre"]]$actions)

  for (i in seq_len(n)) {
    action <- wflow[["pre"]]$actions[[i]]

    # Update the `wflow` as we iterate through pre steps
    wflow <- fit(action, wflow)
  }

  # But only return the wflow, it contains the final set of data in `mold`
  wflow
}

#' @rdname tidyflows-internals
#' @export
.fit_model <- function(tidyflow, control) {
  action_model <- tidyflow[["fit"]][["actions"]][["model"]]
  fit(action_model, wflow = tidyflow, control = control)
}

# ------------------------------------------------------------------------------

validate_has_minimal_components <- function(x) {
  has_preprocessor <- has_action(x$pre, "formula") || has_action(x$pre, "recipe")

  if (!has_preprocessor) {
    glubort(
      "The tidyflow must have a formula or recipe preprocessor. ",
      "Provide one with `add_formula()` or `add_recipe()`."
    )
  }

  has_model <- has_action(x$fit, "model")

  if (!has_model) {
    glubort(
      "The tidyflow must have a model. ",
      "Provide one with `add_model()`."
    )
  }

  invisible(x)
}
