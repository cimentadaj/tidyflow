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
#' By default \code{tidyflow} leaves \code{workflows} to figure out which
#' type of factor/character transformation to happen (either leave factor as is,
#' transform to N-1 dummies or use a one-hot encoding approach of N dummy columns).
#' These transformations depend on the specific model supplied in \code{plug_model}.
#' See \code{\link[workflows]{add_formula}} for more details on how transformations
#' are handled.
#'
#' However, \code{plug_formula} allows to override the type of transformation
#' using the blueprint. For example, by passing
#' \code{default_formula_blueprint(intercept = TRUE, indicators = "none")} to
#' the blueprint argument of \code{plug_formula} you can
#' enforce that all factors/characters area left without transforming. You can also use
#' \code{default_formula_blueprint(intercept = TRUE, indicators = "traditional")} and
#' \code{default_formula_blueprint(intercept = TRUE, indicators = "one_hot")} to
#' transform all factors/characters to N-1 dummies or N dummies respectively.
#'
#' For example, to transform all factors/characters to one-hot encoding, you can pass
#' the blueprint to \code{plug_formula}:
#'
#' ```r
#' bp <- default_formula_blueprint(intercept = TRUE, indicators = "one_hot")
#' iris %>%
#'   tidyflow(seed = 21315)
#'   plug_formula(Sepal.Length ~ Species, blueprint = bp) %>%
#'   plug_model(set_engine(parsnip::linear_reg(), "lm")) %>%
#'   fit()
#' ```
#'
#' For custom transformations between types (for example, applying one-hot on
#' factors and not on characters), the user can provide a \code{\link[recipes]{recipe}}
#' with a \code{\link[recipes]{step_dummy}} step to \code{\link{plug_recipe}}. See
#' \href{https://recipes.tidymodels.org/articles/Dummies.html}{this vignette}
#' for more details
#'
#' @param x A tidyflow
#' @param formula A formula specifying the terms of the model. It is advised to
#'   not do preprocessing in the formula, and instead use a recipe if that is
#'   required.
#'
#' @param ... Not used.
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'   If `NULL`, [hardhat::default_formula_blueprint()] is used.
#'
#' @return
#' The tidyflow `x`, updated with either a new or removed formula preprocessor.
#'
#' @export
#' @examples
#'
#' # Just for the pipe: %>%
#' library(tibble)
#'
#' tflow <-
#'   mtcars %>%
#'   tidyflow(seed = 652341) %>% 
#'   plug_formula(mpg ~ .)
#'
#' tflow
#'
#' drop_formula(tflow)
#'
#' replace_formula(tflow, mpg ~ disp)
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

fit.action_formula <- function(object, x) {
  formula <- object$formula
  blueprint <- object$blueprint
  x$pre$results$preprocessor <- formula
  x$pre$results$blueprint <- blueprint

  # All pre steps return the `tidyflow`
  x
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
