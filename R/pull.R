#' Extract elements of a tidyflow
#'
#' @description
#' These functions extract various elements from a tidyflow object. If they do
#' not exist yet, an error is thrown. **Most of these steps can only be executed
#' after the \code{tidyflow} has been fitted**.
#'
#' - \code{pull_tflow_rawdata()} returns the complete raw/untrained data.
#'
#' - \code{pull_tflow_split()} returns the split object from the function specified
#'    in \code{\link{plug_split}}.
#'
#' - \code{pull_tflow_training()} turns the training data from the split. Only
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

  abort("The tidyflow does not have data.")
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_split <- function(x) {
  if (!(has_fit(x) || has_fit_tuning(x))) {
    abort("Tidyflow has not yet been trained. Do you need to call `fit()`?")
  }

  if (!has_preprocessor_split(x)) {
    abort("The tidyflow must have a split preprocessor.")
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
      abort("You seem to have a recipe with tuning parameters but not a finalized model. Did you call complete_tflow()?")
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
      abort("You seem to have a recipe with tuning parameters but not a finalized model. Did you call complete_tflow()?")
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
    abort("Tidyflow has not yet been trained. Do you need to call `fit()`?")
  }

  if (!has_preprocessor_resample(x)) {
    abort("The tidyflow must have a resample preprocessor.")
  }

  x$pre$results$resample
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_grid <- function(x) {
  if (!has_fit_tuning(x)) {
    abort("Tidyflow has not yet been trained. Do you need to call `fit()`?")
  }

  if (!has_preprocessor_grid(x)) {
    abort("The tidyflow must have a grid preprocessor.")
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

  abort("The tidyflow does not have a preprocessor.")
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_prepped_recipe <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_recipe(x)) {
    abort("The tidyflow must have a recipe preprocessor.")
  }

  res <- pull_tflow_preprocessor(x)

  is_rcp_or_formula <- inherits(res, "recipe") || inherits(res, "formula")
  if (!is_rcp_or_formula) {
    abort("The recipe/formula hasn't been executed. Have you called `fit` yet?")
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

  abort("The tidyflow does not have a model spec.")
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_fit <- function(x) {
  validate_is_tidyflow(x)

  if (has_fit(x)) {
    return(x$fit$fit$fit)
  }

  if (has_fit_tuning(x)) {
    abort("The tidyflow does not have a model fit but a tuning resample. Did you want to finalize the model with `complete_tflow`?") #nolintr
  }

  abort("The tidyflow does not have a model fit. Have you called `fit()` yet?")
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_fit_tuning <- function(x) {
  validate_is_tidyflow(x)

  if (has_fit_tuning(x)) {
    return(x$fit$fit$tuning)
  }

  if (has_fit(x)) {
    abort("The tidyflow does not have a tuning resample but a model fit. Did you want `pull_tflow_fit`?") #nolintr
  }

  abort("The tidyflow does not have a tuning fit. Have you called `fit()` yet?")
}

recipe_or_formula <- function(x) {
  preproc <- x$pre$results$preprocessor
  if (inherits(preproc, "formula")) ~1 else preproc
}
