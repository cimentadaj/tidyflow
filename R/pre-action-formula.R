#' Add formula terms to a tidyflow
#'
#' @description
#' - `plug_formula()` specifies the terms of the model through the usage of a
#'   formula.
#'
#' - `drop_formula()` removes the formula as well as any downstream objects
#'   that might get created after the formula is used for preprocessing, such as
#'   terms. Additionally, if the model has already been fit, then the fit is
#'   removed.
#'
#' - `replace_formula()` first removes the formula, then replaces the previous
#'   formula with the new one. Any model that has already been fit based on this
#'   formula will need to be refit.
#'
#' @details
#' To fit a tidyflow, one of `plug_formula()` or `plug_recipe()` _must_ be
#' specified, but not both.
#'
#' @param x A tidyflow
#'
#' @param formula A formula specifying the terms of the model. It is advised to
#'   not do preprocessing in the formula, and instead use a recipe if that is
#'   required.
#'
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'   If `NULL`, [hardhat::default_formula_blueprint()] is used.
#'
#' @return
#' `x`, updated with either a new or removed formula preprocessor.
#'
#' @export
#' @examples
#' tidyflow <- tidyflow()
#' tidyflow <- plug_formula(tidyflow, mpg ~ cyl)
#' tidyflow
#'
#' drop_formula(tidyflow)
#'
#' replace_formula(tidyflow, mpg ~ disp)
plug_formula <- function(x, formula, ..., blueprint = NULL) {
  ellipsis::check_dots_empty()
  action <- new_action_formula(formula, blueprint)
  plug_action(x, action, "formula")
}

#' @rdname plug_formula
#' @export
drop_formula <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_formula(x)) {
    rlang::warn("The tidyflow has no formula preprocessor to remove.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(actions = purge_action_formula(x),
                        mold = x$data,
                        seed = x$pre$seed,
                        results = purge_results_formula(x)),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname plug_formula
#' @export
replace_formula <- function(x, formula, ..., blueprint = NULL) {
  ellipsis::check_dots_empty()
  x <- drop_formula(x)
  plug_formula(x, formula, blueprint = blueprint)
}

# ------------------------------------------------------------------------------

fit.action_formula <- function(object, tidyflow) {
  formula <- object$formula
  blueprint <- object$blueprint
  
  # TODO - Strip out the formula environment at some time?
  tidyflow$pre$mold <- hardhat::mold(formula,
                                     tidyflow$pre$mold,
                                     blueprint = blueprint)

  tidyflow$pre$results$formula <- tidyflow$pre$mold

  # All pre steps return the `tidyflow`
  tidyflow
}

# ------------------------------------------------------------------------------

check_conflicts.action_formula <- function(action, x) {
  pre <- x$pre

  if (has_action(pre, "recipe")) {
    abort("A formula cannot be added when a recipe already exists.")
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_formula <- function(formula, blueprint) {
  if (!is_formula(formula)) {
    abort("`formula` must be a formula.")
  }

  if (is.null(blueprint)) {
    blueprint <- hardhat::default_formula_blueprint()
  } else if (!is_formula_blueprint(blueprint)) {
    abort("`blueprint` must be a hardhat 'formula_blueprint'.")
  }

  new_action_pre(
    formula = formula,
    blueprint = blueprint,
    subclass = "action_formula"
  )
}

is_formula_blueprint <- function(x) {
  inherits(x, "formula_blueprint")
}
