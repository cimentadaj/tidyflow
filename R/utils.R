is_uniquely_named <- function(x) {
  if (length(x) > 0) {
    is_named(x) && !anyDuplicated(names(x))
  } else {
    TRUE
  }
}

glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

is_model_fit <- function(x) {
  inherits(x, "model_fit")
}

is_model_spec <- function(x) {
  inherits(x, "model_spec")
}

validate_recipes_available <- function() {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    abort(
      "The `recipes` package must be available to add a recipe."
    )
  }
  invisible()
}

# ------------------------------------------------------------------------------

validate_is_tidyflow <- function(x, arg = "`x`") {
  if (!is_tidyflow(x)) {
    glubort("{arg} must be a tidyflow, not a {class(x)[[1]]}.")
  }

  invisible(x)
}

# ------------------------------------------------------------------------------
has_preprocessor_split <- function(x) {
  "split" %in% names(x$pre$actions)
}

has_preprocessor_resample <- function(x) {
  "resample" %in% names(x$pre$actions)
}

has_preprocessor_recipe <- function(x) {
  "recipe" %in% names(x$pre$actions)
}

has_preprocessor_formula <- function(x) {
  "formula" %in% names(x$pre$actions)
}

has_preprocessor_fit <- function(x) {
  !is.null(x$fit$fit)
}


has_mold <- function(x) {
  !is.null(x$pre$mold)
}

has_spec <- function(x) {
  "model" %in% names(x$fit$actions)
}

has_fit <- function(x) {
  !is.null(x$fit$fit$fit)
}

has_fit_tuning <- function(x) {
  !is.null(x$fit$fit$tuning)
}

has_raw_data <- function(x) {
  !is.null(x$data)
}


# ------------------------------------------------------------------------------

# Since all actions within pre/fit/post are named, we want to be able to remove
# some actions from a stage. For example, if in the pre stage there is a recipe and
# the split specification, we want the power to remove specific actions
# from that stage. Each purge_action_* works for a stage and name to remove the
# action.
purge_ <- function(x, name) {
  all_names <- names(x)
  selected_names <- setdiff(all_names, name)
  x <- x[selected_names]
  # In the case where the list of actions is empty
  # it will still be named. Make sure its completely empty
  # This is useful for comparing with an empty list of actions in the tests
  if (rlang::is_empty(x)) x <- unname(x)
  x
}

purge_action_split <- function(x) {
  purge_(x$pre$actions, "split")
}

purge_action_resample <- function(x) {
  purge_(x$pre$actions, "resample")
}

purge_action_formula <- function(x) {
  purge_(x$pre$actions, "formula")
}

purge_action_recipe <- function(x) {
  purge_(x$pre$actions, "recipe")
}

purge_results_split <- function(x) {
  purge_(x$pre$results, "split")
}

purge_results_resample <- function(x) {
  purge_(x$pre$results, "resample")
}

purge_results_formula <- function(x) {
  purge_(x$pre$results, "formula")
}

purge_results_recipe <- function(x) {
  purge_(x$pre$results, "recipe")
}

combine_outcome_preds <- function(mold) {
  cbind(mold$outcomes, mold$predictors)
}

# To compare equality of models, elapsed time is sometimes
# the only different thing
strip_elapsed <- function(x) {
  x$fit$fit$elapsed <- NULL
  x
}

# To compare equality with all.equal in tests. all.equal
# doesn't support list columns in tibbles. Here we just
# turn to dataframe all rsplit objects
rsplit2df <- function(x) {
  x$pre$results$resample <- as.data.frame(x$pre$results$resample)
  x$fit$fit$tuning <- as.data.frame(x$fit$fit$tuning)

  x
}
