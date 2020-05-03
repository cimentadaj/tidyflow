#' Extract elements of a tidyflow
#'
#' @description
#' These functions extract various elements from a tidyflow object. If they do
#' not exist yet, an error is thrown.
#'
#' - `pull_tflow_rawdata()` returns the complete raw/untrained data.
#'
#' - `pull_tflow_split()` returns the split object from the function specified
#'    in \code{\link{plug_split}}.
#'
#' - `pull_tflow_training()` returns the training data from the split. Only
#'    works when a split has been specified with \code{\link{plug_split}}.
#'    If \code{prep = TRUE}, the preprocessing (either recipe or formula)
#'    is applied to the data.
#'
#' - `pull_tflow_testing()` returns the testing data from the split. Only
#'    works when a split has been specified with \code{\link{plug_split}}
#'    If \code{prep = TRUE}, the preprocessing (either recipe or formula)
#'    is applied to the data.
#' 
#' - `pull_tflow_preprocessor()` returns either the formula or recipe used
#'   for preprocessing. Note that in the case of the recipe it returns the
#'   function that should be applied to the data to apply the recipe.
#'   \code{\link{pull_tflow_prepped_recipe}} returns the recipe already applied
#'   to the training data.
#'
#' - `pull_tflow_resample()` returns the resample object from the function
#'    specified in \code{\link{plug_resample}}. This resample object already
#'    has the preprocessor applied (either formula or recipe).
#'
#' - `pull_tflow_grid()` returns the grid data frame from which the tuning
#'    parameter was made.
#' 
#' - `pull_tflow_prepped_recipe()` returns the prepped recipe. It is
#'   extracted from the mold object returned from `pull_tflow_mold()`.
#'
#' - `pull_tflow_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe or the formula terms object.
#'
#' - `pull_tflow_spec()` returns the parsnip model specification.
#'
#' - `pull_tflow_fit()` returns the parsnip model fit.
#'
#' - `pull_tflow_fit_tuning()` returns the resample result from model tuning.
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
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' recipe <- ~ recipe(.x, mpg ~ cyl + disp) %>% step_log(disp)
#'
#' base_tflow <- tidyflow()
#' base_tflow <- plug_data(base_tflow, mtcars)
#' base_tflow <- plug_split(base_tflow, initial_split)
#' base_tflow <- plug_model(base_tflow, model)
#'
#' recipe_tflow <- plug_recipe(base_tflow, recipe)
#' formula_tflow <- plug_formula(base_tflow, mpg ~ cyl + log(disp))
#'
#' fit_recipe_tflow <- fit(recipe_tflow)
#' fit_formula_tflow <- fit(formula_tflow)
#'
#' pull_tflow_rawdata(fit_recipe_tflow)
#' pull_tflow_rawdata(fit_formula_tflow)
#' 
#' # The preprocessor is either the recipe function or a formula
#' pull_tflow_preprocessor(recipe_tflow)
#' pull_tflow_preprocessor(formula_tflow)
#'
#' # The `spec` is the parsnip spec before it has been fit.
#' # The `fit` is the fit parsnip model.
#' pull_tflow_spec(fit_formula_tflow)
#' pull_tflow_fit(fit_formula_tflow)
#'
#' # The mold is returned from `hardhat::mold()`, and contains the
#' # predictors, outcomes, and information about the preprocessing
#' # for use on new data at `predict()` time.
#' pull_tflow_mold(fit_recipe_tflow)
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
#' # That is identical to
#' identical(
#'   pull_tflow_mold(fit_recipe_tflow)$blueprint$recipe,
#'   pull_tflow_prepped_recipe(fit_recipe_tflow)
#' )
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

  if (prep) {
    training_data <- hardhat::forge(training_data, pull_tflow_mold(x)$blueprint,
                                    outcomes = TRUE)
    training_data <- combine_outcome_preds(training_data)
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

  if (prep) {
    test_data <- hardhat::forge(test_data, pull_tflow_mold(x)$blueprint,
                                outcomes = TRUE)
    test_data <- combine_outcome_preds(test_data)
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

  x$pre$results$grid
}


#' @rdname tidyflow-extractors
#' @export
pull_tflow_preprocessor <- function(x) {
  validate_is_tidyflow(x)

  if (has_preprocessor_formula(x)) {
    return(x$pre$actions$formula$formula)
  }

  if (has_preprocessor_recipe(x)) {
    return(x$pre$actions$recipe$recipe)
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

  mold <- pull_tflow_mold(x)

  mold$blueprint$recipe
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_mold <- function(x) {
  validate_is_tidyflow(x)

  if (has_mold(x)) {
    return(x$pre$mold)
  }

  abort("The tidyflow does not have a mold. Have you called `fit()` yet?")
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

  abort("The tidyflow does not have a model fit. Have you called `fit()` yet?")
}

#' @rdname tidyflow-extractors
#' @export
pull_tflow_fit_tuning <- function(x) {
  validate_is_tidyflow(x)

  if (has_fit_tuning(x)) {
    return(x$fit$fit$tuning)
  }

  abort("The tidyflow does not have a tuning fit. Have you called `fit()` yet?")
}

recipe_or_formula <- function(x) {
  rcp <- x$pre$results$recipe
  if (!is.null(rcp)) rcp else ~1
}
