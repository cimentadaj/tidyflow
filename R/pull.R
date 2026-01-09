#' Extract elements of a tidyflow
#'
#' @description
#' These functions extract various elements from a tidyflow object. If they do
#' not exist yet, an error is thrown. **Most of these steps can only be executed
#' after the \code{tidyflow} has been fitted**.
#'
#' For tidymodels-consistent naming, see also \code{\link{tidyflow-extract}} for
#' the `extract_*` family of methods (e.g., \code{extract_fit_parsnip()},
#' \code{extract_spec_parsnip()}, \code{extract_preprocessor()}).
#'
#' - \code{pull_tflow_rawdata()} returns the complete raw/untrained data.
#'
#' - \code{pull_tflow_split()} returns the split object from the function specified
#'    in \code{\link{plug_split}}.
#'
#' - \code{pull_tflow_training()} returns the training data from the split. Only
#'    works when a split has been specified with \code{\link{plug_split}}.
#'    If \code{prep = TRUE}, the preprocessing (either recipe or formula)
#'    is applied to the data.
#'
#' - \code{pull_tflow_testing()} returns the testing data from the split. Only
#'    works when a split has been specified with \code{\link{plug_split}}
#'    If \code{prep = TRUE}, the preprocessing (either recipe or formula)
#'    is applied to the data.
#'
#' - \code{pull_tflow_preprocessor()} returns either the formula or recipe used
#'   for preprocessing. Note that if the recipe has a \code{tune()} argument,
#'   it won't be finalized.
#'
#' - \code{pull_tflow_resample()} returns the resample object from the function
#'    specified in \code{\link{plug_resample}}. The resample object does not
#'    have the preprocessor applied (either formula or recipe).
#'
#' - \code{pull_tflow_grid()} returns the grid data frame from which the tuning
#'    parameter was made.
#'
#' - \code{pull_tflow_prepped_recipe()} returns the prepped recipe.
#'
#' - \code{pull_tflow_spec()} returns the parsnip model specification.
#'
#' - \code{pull_tflow_fit()} returns the parsnip model fit.
#'
#' - \code{pull_tflow_fit_tuning()} returns the resample result from model tuning.
#'
#' @param x A tidyflow
#'
#' @param prep A logical stating whether the training/testing data should be
#' returned with the preprocessing step applied (either the formula or the recipe
#' preprocessing). By default it is set to \code{FALSE}.
#' 
#' @return
#' The extracted value from the tidyflow, `x`, as described in the description
#' section.
#'
#' @name tidyflow-extractors
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(rsample)
#'
#' model <- set_engine(linear_reg(), "lm")
#'
#' recipe <- ~ recipe(.x, mpg ~ cyl + disp) %>% step_log(disp)
#'
#' tflow <-
#'  mtcars %>%
#'  tidyflow() %>%
#'  plug_split(initial_split) %>%
#'  plug_model(model)
#'
#' recipe_tflow <- plug_recipe(tflow, recipe)
#' formula_tflow <- plug_formula(tflow, mpg ~ cyl + log(disp))
#'
#' fit_recipe_tflow <- fit(recipe_tflow)
#' fit_formula_tflow <- fit(formula_tflow)
#'
#' pull_tflow_rawdata(fit_recipe_tflow)
#' pull_tflow_rawdata(fit_formula_tflow)
#' 
#' # The preprocessor is either the recipe function or a formula
#' pull_tflow_preprocessor(fit(recipe_tflow))
#' pull_tflow_preprocessor(fit(formula_tflow))
#'
#' # The `spec` is the parsnip spec before it has been fit.
#' # The `fit` is the fit parsnip model.
#' pull_tflow_spec(fit_formula_tflow)
#' pull_tflow_fit(fit_formula_tflow)
#'
#' # The raw training and testing
#' pull_tflow_training(fit_recipe_tflow)
#' pull_tflow_testing(fit_recipe_tflow)
#'
#' # Or with the preprocessor (recipe/formula) applied
#' pull_tflow_training(fit_recipe_tflow, prep = TRUE)
#' pull_tflow_testing(fit_recipe_tflow, prep = TRUE)
#' 
#' # A useful shortcut is to extract the prepped recipe from the tidyflow
#' pull_tflow_prepped_recipe(fit_recipe_tflow)
#'
#'
NULL

#' @rdname tidyflow-extractors
#' @export
pull_tflow_rawdata <- function(x) {
  validate_is_tidyflow(x)

  if (has_raw_data(x)) {
    return(x$data)
  }

  cli::cli_abort(c(
    "Cannot extract data from tidyflow.",
    "x" = "No data has been added to this tidyflow.",
    "i" = "Use {.fn plug_data} or {.fn tidyflow} with a data frame."
  ))
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_split <- function(x) {
  if (!(has_fit(x) || has_fit_tuning(x))) {
    cli::cli_abort(c(
      "Cannot extract split from tidyflow.",
      "x" = "The tidyflow has not been fitted yet.",
      "i" = "Use {.fn fit} to fit the tidyflow first."
    ))
  }

  if (!has_preprocessor_split(x)) {
    cli::cli_abort(c(
      "Cannot extract split from tidyflow.",
      "x" = "No split has been specified.",
      "i" = "Use {.fn plug_split} to add a data split."
    ))
  }

  x$pre$results$split
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_training <- function(x, prep = FALSE) {
  validate_is_tidyflow(x)
  # No need to check if there's a split or it has been fit
  # pull_tflow_split does.
  training_data <- rsample::training(pull_tflow_split(x))
  preproc <- x$pre$actions$recipe$recipe_res %||% x$pre$results$preprocessor

  if (prep) {
    if (has_tune(preproc) && !x$trained) {
      cli::cli_abort(c(
        "Cannot prep training data with unfinalized tuning parameters.",
        "x" = "The recipe has {.fn tune} parameters but the model is not finalized.",
        "i" = "Use {.fn complete_tflow} to finalize the model first."
      ))
    } else {
      training_data <- hardhat::mold(preproc, training_data)
      training_data <- combine_outcome_preds(training_data)
    }
  }

  training_data

}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_testing <- function(x, prep = FALSE) {
  validate_is_tidyflow(x)
  # No need to check if there's a split or it has been fit
  # pull_tflow_split does.
  test_data <- rsample::testing(pull_tflow_split(x))
  preproc <- x$pre$actions$recipe$recipe_res %||% x$pre$results$preprocessor

  if (prep) {
    if (has_tune(preproc) && !x$trained) {
      cli::cli_abort(c(
        "Cannot prep testing data with unfinalized tuning parameters.",
        "x" = "The recipe has {.fn tune} parameters but the model is not finalized.",
        "i" = "Use {.fn complete_tflow} to finalize the model first."
      ))
    } else {
      test_data <- hardhat::mold(preproc, test_data)
      test_data <- combine_outcome_preds(test_data)
    }
  }

  test_data
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_resample <- function(x) {
  if (!(has_fit(x) || has_fit_tuning(x))) {
    cli::cli_abort(c(
      "Cannot extract resample from tidyflow.",
      "x" = "The tidyflow has not been fitted yet.",
      "i" = "Use {.fn fit} to fit the tidyflow first."
    ))
  }

  if (!has_preprocessor_resample(x)) {
    cli::cli_abort(c(
      "Cannot extract resample from tidyflow.",
      "x" = "No resample has been specified.",
      "i" = "Use {.fn plug_resample} to add resampling."
    ))
  }

  x$pre$results$resample
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_grid <- function(x) {
  if (!has_fit_tuning(x)) {
    cli::cli_abort(c(
      "Cannot extract grid from tidyflow.",
      "x" = "The tidyflow has not been fitted with tuning yet.",
      "i" = "Use {.fn fit} to fit the tidyflow first."
    ))
  }

  if (!has_preprocessor_grid(x)) {
    cli::cli_abort(c(
      "Cannot extract grid from tidyflow.",
      "x" = "No grid has been specified.",
      "i" = "Use {.fn plug_grid} to add a tuning grid."
    ))
  }

  x$pre$results$grid$grid
}


#' @rdname tidyflow-extractors
#' @export
pull_tflow_preprocessor <- function(x) {
  validate_is_tidyflow(x)

  if (has_preprocessor_rcp_formula(x)) {
    return(x$pre$results$preprocessor)
  }

  cli::cli_abort(c(
    "Cannot extract preprocessor from tidyflow.",
    "x" = "No preprocessor (formula or recipe) has been specified.",
    "i" = "Use {.fn plug_formula} or {.fn plug_recipe} to add a preprocessor."
  ))
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_prepped_recipe <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_recipe(x)) {
    cli::cli_abort(c(
      "Cannot extract prepped recipe from tidyflow.",
      "x" = "No recipe has been specified.",
      "i" = "Use {.fn plug_recipe} to add a recipe."
    ))
  }

  res <- pull_tflow_preprocessor(x)

  is_rcp_or_formula <- inherits(res, "recipe") || inherits(res, "formula")
  if (!is_rcp_or_formula) {
    cli::cli_abort(c(
      "Cannot extract prepped recipe from tidyflow.",
      "x" = "The recipe has not been executed yet.",
      "i" = "Use {.fn fit} to fit the tidyflow first."
    ))
  }

  recipes::prep(res)
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_spec <- function(x) {
  validate_is_tidyflow(x)

  if (has_spec(x)) {
    return(x$fit$actions$model$spec)
  }

  cli::cli_abort(c(
    "Cannot extract model spec from tidyflow.",
    "x" = "No model has been specified.",
    "i" = "Use {.fn plug_model} to add a model specification."
  ))
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_fit <- function(x) {
  validate_is_tidyflow(x)

  if (has_fit(x)) {
    return(x$fit$fit$fit)
  }

  if (has_fit_tuning(x)) {
    cli::cli_abort(c(
      "Cannot extract model fit from tidyflow.",
      "x" = "The tidyflow has tuning results but no finalized model.",
      "i" = "Use {.fn complete_tflow} to finalize the best model."
    ))
  }

  cli::cli_abort(c(
    "Cannot extract model fit from tidyflow.",
    "x" = "The tidyflow has not been fitted yet.",
    "i" = "Use {.fn fit} to fit the tidyflow first."
  ))
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_fit_tuning <- function(x) {
  validate_is_tidyflow(x)

  if (has_fit_tuning(x)) {
    return(x$fit$fit$tuning)
  }

  if (has_fit(x)) {
    cli::cli_abort(c(
      "Cannot extract tuning results from tidyflow.",
      "x" = "The tidyflow has a model fit but no tuning results.",
      "i" = "Did you mean {.fn pull_tflow_fit}?"
    ))
  }

  cli::cli_abort(c(
    "Cannot extract tuning results from tidyflow.",
    "x" = "The tidyflow has not been fitted yet.",
    "i" = "Use {.fn fit} to fit the tidyflow first."
  ))
}

recipe_or_formula <- function(x) {
  preproc <- x$pre$results$preprocessor
  if (inherits(preproc, "formula")) ~1 else preproc
}
