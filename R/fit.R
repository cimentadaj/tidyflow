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
#' @param tflow A tidyflow object
#' @param ... Not used
#'
#' @param control A [control_tidyflow()] object
#'
#' @return
#' If a tuning grid has been specified with \code{\link{plug_grid}}, then the
#' result is a data frame with the tuning results. Can be extracted with
#' \code{\link{pull_tflow_fit_tuning}}. If no tuning grid has been specified,
#' the result is a parsnip model. It can be extracted with
#' \code{\link{pull_tflow_fit}}.
#'
#' @name fit-tidyflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(tune)
#' library(dials)
#' library(rsample)
#'
#' # Fit a simple linear model
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' formula_tidyflow <-
#'  mtcars %>%
#'  tidyflow() %>%
#'  plug_formula(mpg ~ cyl + log(disp)) %>%
#'  plug_model(model)
#' 
#' fit(formula_tidyflow)
#'
#' recipe_tidyflow <-
#'  formula_tidyflow %>%
#'  drop_formula() %>% 
#'  plug_recipe(~ recipe(mpg ~ cyl + disp, .x) %>% step_log(disp))
#'
#' fit(recipe_tidyflow)
#'
#' # Fit a regularized regression through a grid search.
#' # Do this by updating the already defined model:
#' new_mod <- set_engine(linear_reg(penalty = tune(), mixture = tune()),
#'                       "glmnet")
#' tuned_res <-
#'  formula_tidyflow %>%
#'   plug_resample(vfold_cv, v = 2) %>% 
#'   replace_formula(mpg ~ .) %>% 
#'   replace_model(new_mod) %>%
#'   plug_grid(grid_regular, levels = 2) %>%
#'   fit()
#'
#' # Extract the tuning fit:
#' pull_tflow_fit_tuning(tuned_res)
#'
#' # TODO: Extract best params and refit once complete_wflow is ready
#'  
#'
fit.tidyflow <- function(tflow, ..., control = control_tidyflow()) {

  if (!has_raw_data(tflow)) {
    abort("`data` must be specified to fit a tidyflow; Do you need `plug_data`?")
  }

  ellipsis::check_dots_empty()
  validate_has_minimal_components(tflow)

  tflow <- .fit_pre(tflow)
  tflow <- .fit_model(tflow, control)

  # Eh? Predictions during the fit?
  # pred <- result$pred
  # result <- fit_post(tflow, pred)

  # Only if it has be fit (NOT TUNED!)
  tflow$trained <- if (has_fit(tflow)) TRUE else FALSE

  tflow
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
#' base_tidyflow <- plug_model(base_tidyflow, model)
#'
#' formula_tidyflow <- plug_formula(base_tidyflow, mpg ~ cyl + log(disp))
#'
#' partially_fit_tidyflow <- .fit_pre(formula_tidyflow)
#' fit_tidyflow <- .fit_model(partially_fit_tidyflow, control_tidyflow())
.fit_pre <- function(x) {
  n <- length(x[["pre"]]$actions)

  for (i in seq_len(n)) {
    # If no seed has been specified, `set.seed` supports NULL as random
    set.seed(x$pre$seed)
    action <- x[["pre"]]$actions[[i]]
    # Update the `x` as we iterate through pre steps
    x <- fit(action, x)
  }

  # But only return the x, it contains the final set of data in `mold`
  x
}

#' @rdname tidyflows-internals
#' @export
.fit_model <- function(x, control) {
  # If no seed has been specified, `set.seed` supports NULL as random
  set.seed(x$pre$seed)

  object <- x[["fit"]][["actions"]][["model"]]
  fit(object, x = x, control = control)
}

# ------------------------------------------------------------------------------

validate_has_minimal_components <- function(x) {
  has_preprocessor <- has_action(x$pre, "formula") || has_action(x$pre, "recipe")

  if (!has_preprocessor) {
    glubort(
      "The tidyflow must have a formula or recipe preprocessor. ",
      "Provide one with `plug_formula()` or `plug_recipe()`."
    )
  }

  has_model <- has_action(x$fit, "model")

  if (!has_model) {
    glubort(
      "The tidyflow must have a model. ",
      "Provide one with `plug_model()`."
    )
  }

  invisible(x)
}
