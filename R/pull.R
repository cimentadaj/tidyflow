#' Extract elements of a tidyflow
#'
#' @description
#' These functions extract various elements from a tidyflow object. If they do
#' not exist yet, an error is thrown.
#'
#' - `pull_tidyflow_rawdata()` returns the complete raw/untrained data.
#' 
#' - `pull_tidyflow_preprocessor()` returns either the formula or recipe used
#'   for preprocessing.
#'
#' - `pull_tidyflow_spec()` returns the parsnip model specification.
#'
#' - `pull_tflow_fit()` returns the parsnip model fit.
#'
#' - `pull_tidyflow_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe or the formula terms object.
#'
#' - `pull_tidyflow_prepped_recipe()` returns the prepped recipe. It is
#'   extracted from the mold object returned from `pull_tidyflow_mold()`.
#'
#' - `pull_tidyflow_testing()` returns the raw testing data (without applying
#'   the preprocessing steps recipe or formula). Since the split
#'   training/testing is done when the user `fit()'s the model, the testing
#'   data can only be extracted after a model fit. The prepping of the testing
#'   data is done when specifying `new_data` in `predict.tidyflow`
#'   automatically.
#'
#' @param x A tidyflow
#'
#'
#' @return
#' The extracted value from the tidyflow, `x`, as described in the description
#' section.
#'
#' @name tidyflow-extractors
#' @examples
#' library(parsnip)
#' library(recipes)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' recipe <- ~ recipe(mpg ~ cyl + disp, .x) %>% step_log(disp)
#'
#' base_tidyflow <- tidyflow()
#' base_tidyflow <- plug_data(base_tidyflow, mtcars)
#' base_tidyflow <- plug_model(base_tidyflow, model)
#'
#' recipe_tidyflow <- plug_recipe(base_tidyflow, recipe)
#' formula_tidyflow <- plug_formula(base_tidyflow, mpg ~ cyl + log(disp))
#'
#' fit_recipe_tidyflow <- fit(recipe_tidyflow)
#' fit_formula_tidyflow <- fit(formula_tidyflow)
#'
#' pull_tidyflow_rawdata(fit_recipe_tidyflow)
#' pull_tidyflow_rawdata(fit_formula_tidyflow)
#' 
#' # The preprocessor is either the recipe function or a formula
#' pull_tidyflow_preprocessor(recipe_tidyflow)
#' pull_tidyflow_preprocessor(formula_tidyflow)
#'
#' # The `spec` is the parsnip spec before it has been fit.
#' # The `fit` is the fit parsnip model.
#' pull_tidyflow_spec(fit_formula_tidyflow)
#' pull_tflow_fit(fit_formula_tidyflow)
#'
#' # The mold is returned from `hardhat::mold()`, and contains the
#' # predictors, outcomes, and information about the preprocessing
#' # for use on new data at `predict()` time.
#' pull_tidyflow_mold(fit_recipe_tidyflow)
#'
#' # A useful shortcut is to extract the prepped recipe from the tidyflow
#' pull_tidyflow_prepped_recipe(fit_recipe_tidyflow)
#'
#' # That is identical to
#' identical(
#'   pull_tidyflow_mold(fit_recipe_tidyflow)$blueprint$recipe,
#'   pull_tidyflow_prepped_recipe(fit_recipe_tidyflow)
#' )
#'
NULL

#' @rdname tidyflow-extractors
#' @export
pull_tidyflow_rawdata <- function(x) {
  validate_is_tidyflow(x)

  if (has_raw_data(x)) {
    return(x$data)
  }

  abort("The tidyflow does not have data.")
}


#' @rdname tidyflow-extractors
#' @export
pull_tidyflow_preprocessor <- function(x) {
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
pull_tidyflow_spec <- function(x) {
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
    return(x$fit$fit)
  }

  abort("The tidyflow does not have a model fit. Have you called `fit()` yet?")
}

#' @rdname tidyflow-extractors
#' @export
pull_tidyflow_mold <- function(x) {
  validate_is_tidyflow(x)

  if (has_mold(x)) {
    return(x$pre$mold)
  }

  abort("The tidyflow does not have a mold. Have you called `fit()` yet?")
}

#' @rdname tidyflow-extractors
#' @export
pull_tidyflow_prepped_recipe <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_recipe(x)) {
    abort("The tidyflow must have a recipe preprocessor.")
  }

  mold <- pull_tidyflow_mold(x)

  mold$blueprint$recipe
}

#' @rdname tidyflow-extractors
#' @export
pull_tidyflow_testing <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_split(x)) {
    abort("The tidyflow must have a split preprocessor.")
  }

  if (!x$trained) {
    abort("Tidyflow has not yet been trained. Do you need to call `fit()`?")
  }

  test_data <- x$pre$actions$split$testing
  test_data
}
