#' Extract elements from a tidyflow
#'
#' @description
#' These functions provide tidymodels-consistent `extract_*` methods for
#' tidyflow objects. They are alternatives to the `pull_tflow_*` functions
#' and follow the tidymodels ecosystem naming conventions.
#'
#' - `extract_fit_parsnip()` returns the parsnip model fit object.
#'
#' - `extract_spec_parsnip()` returns the parsnip model specification.
#'
#' - `extract_preprocessor()` returns the preprocessor (formula or recipe).
#'
#' - `extract_mold()` returns the mold from the internal workflow object.
#'
#' - `extract_recipe()` returns the prepped recipe (after fitting).
#'
#' - `extract_parameter_set_dials()
#'
#' @param x A tidyflow object.
#' @param ... Not currently used.
#'
#' @return The extracted element from the tidyflow.
#'
#' @name tidyflow-extract
#' @examples
#' library(parsnip)
#' library(rsample)
#'
#' model <- set_engine(linear_reg(), "lm")
#'
#' tflow <-
#'   mtcars %>%
#'   tidyflow() %>%
#'   plug_split(initial_split) %>%
#'   plug_formula(mpg ~ cyl + disp) %>%
#'   plug_model(model)
#'
#' fitted_tflow <- fit(tflow)
#'
#' # Extract the model specification (before fitting)
#' extract_spec_parsnip(fitted_tflow)
#'
#' # Extract the fitted model
#' extract_fit_parsnip(fitted_tflow)
#'
#' # Extract the preprocessor (formula or recipe)
#' extract_preprocessor(fitted_tflow)
#'
#' # Extract the mold from the workflow
#' extract_mold(fitted_tflow)
#'
NULL

#' @rdname tidyflow-extract
#' @export
extract_fit_parsnip.tidyflow <- function(x, ...) {
  pull_tflow_fit(x)
}

#' @rdname tidyflow-extract
#' @export
extract_spec_parsnip.tidyflow <- function(x, ...) {
  pull_tflow_spec(x)
}

#' @rdname tidyflow-extract
#' @export
extract_preprocessor.tidyflow <- function(x, ...) {
  pull_tflow_preprocessor(x)
}

#' @rdname tidyflow-extract
#' @export
extract_mold.tidyflow <- function(x, ...) {
 validate_is_tidyflow(x)

  if (!has_fit(x)) {
    cli::cli_abort(c(
      "Cannot extract mold from tidyflow.",
      "x" = "The tidyflow has not been fitted yet.",
      "i" = "Use {.fn fit} to fit the tidyflow first."
    ))
  }

  workflows::extract_mold(x$fit$fit$wflow)
}

#' @rdname tidyflow-extract
#' @export
extract_recipe.tidyflow <- function(x, ..., estimated = TRUE) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_recipe(x)) {
    cli::cli_abort(c(
      "Cannot extract recipe from tidyflow.",
      "x" = "The tidyflow does not have a recipe preprocessor.",
      "i" = "Use {.fn plug_recipe} to add a recipe."
    ))
  }

  if (estimated) {
    pull_tflow_prepped_recipe(x)
  } else {
    x$pre$actions$recipe$recipe
  }
}

#' @rdname tidyflow-extract
#' @export
extract_parameter_set_dials.tidyflow <- function(x, ...) {
  parameters(x)
}
