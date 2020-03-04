#' Extract elements of a workflow
#'
#' @description
#' These functions extract various elements from a workflow object. If they do
#' not exist yet, an error is thrown.
#'
#' - `pull_workflow_rawdata()` returns the complete raw/untrained data.
#' 
#' - `pull_workflow_preprocessor()` returns either the formula or recipe used
#'   for preprocessing.
#'
#' - `pull_workflow_spec()` returns the parsnip model specification.
#'
#' - `pull_tflow_fit()` returns the parsnip model fit.
#'
#' - `pull_workflow_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe or the formula terms object.
#'
#' - `pull_workflow_prepped_recipe()` returns the prepped recipe. It is
#'   extracted from the mold object returned from `pull_workflow_mold()`.
#'
#' - `pull_workflow_testing()` returns the raw testing data (without applying
#'   the preprocessing steps recipe or formula). Since the split
#'   training/testing is done when the user `fit()'s the model, the testing
#'   data can only be extracted after a model fit. The prepping of the testing
#'   data is done when specifying `new_data` in `predict.workflow`
#'   automatically.
#'
#' @param x A workflow
#'
#'
#' @return
#' The extracted value from the workflow, `x`, as described in the description
#' section.
#'
#' @name workflow-extractors
#' @examples
#' library(parsnip)
#' library(recipes)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' recipe <- ~ recipe(mpg ~ cyl + disp, .x) %>% step_log(disp)
#'
#' base_workflow <- workflow()
#' base_workflow <- add_data(base_workflow, mtcars)
#' base_workflow <- add_model(base_workflow, model)
#'
#' recipe_workflow <- add_recipe(base_workflow, recipe)
#' formula_workflow <- add_formula(base_workflow, mpg ~ cyl + log(disp))
#'
#' fit_recipe_workflow <- fit(recipe_workflow)
#' fit_formula_workflow <- fit(formula_workflow)
#'
#' pull_workflow_rawdata(fit_recipe_workflow)
#' pull_workflow_rawdata(fit_formula_workflow)
#' 
#' # The preprocessor is either the recipe function or a formula
#' pull_workflow_preprocessor(recipe_workflow)
#' pull_workflow_preprocessor(formula_workflow)
#'
#' # The `spec` is the parsnip spec before it has been fit.
#' # The `fit` is the fit parsnip model.
#' pull_workflow_spec(fit_formula_workflow)
#' pull_tflow_fit(fit_formula_workflow)
#'
#' # The mold is returned from `hardhat::mold()`, and contains the
#' # predictors, outcomes, and information about the preprocessing
#' # for use on new data at `predict()` time.
#' pull_workflow_mold(fit_recipe_workflow)
#'
#' # A useful shortcut is to extract the prepped recipe from the workflow
#' pull_workflow_prepped_recipe(fit_recipe_workflow)
#'
#' # That is identical to
#' identical(
#'   pull_workflow_mold(fit_recipe_workflow)$blueprint$recipe,
#'   pull_workflow_prepped_recipe(fit_recipe_workflow)
#' )
#'
NULL

#' @rdname workflow-extractors
#' @export
pull_workflow_rawdata <- function(x) {
  validate_is_workflow(x)

  if (has_raw_data(x)) {
    return(x$data)
  }

  abort("The workflow does not have data.")
}


#' @rdname workflow-extractors
#' @export
pull_workflow_preprocessor <- function(x) {
  validate_is_workflow(x)

  if (has_preprocessor_formula(x)) {
    return(x$pre$actions$formula$formula)
  }

  if (has_preprocessor_recipe(x)) {
    return(x$pre$actions$recipe$recipe)
  }

  abort("The workflow does not have a preprocessor.")
}

#' @rdname workflow-extractors
#' @export
pull_workflow_spec <- function(x) {
  validate_is_workflow(x)

  if (has_spec(x)) {
    return(x$fit$actions$model$spec)
  }

  abort("The workflow does not have a model spec.")
}

#' @rdname workflow-extractors
#' @export
pull_tflow_fit <- function(x) {
  validate_is_workflow(x)

  if (has_fit(x)) {
    return(x$fit$fit)
  }

  abort("The workflow does not have a model fit. Have you called `fit()` yet?")
}

#' @rdname workflow-extractors
#' @export
pull_workflow_mold <- function(x) {
  validate_is_workflow(x)

  if (has_mold(x)) {
    return(x$pre$mold)
  }

  abort("The workflow does not have a mold. Have you called `fit()` yet?")
}

#' @rdname workflow-extractors
#' @export
pull_workflow_prepped_recipe <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_recipe(x)) {
    abort("The workflow must have a recipe preprocessor.")
  }

  mold <- pull_workflow_mold(x)

  mold$blueprint$recipe
}

#' @rdname workflow-extractors
#' @export
pull_workflow_testing <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_split(x)) {
    abort("The workflow must have a split preprocessor.")
  }

  if (!x$trained) {
    abort("Workflow has not yet been trained. Do you need to call `fit()`?")
  }

  test_data <- x$pre$actions$split$testing
  test_data
}
