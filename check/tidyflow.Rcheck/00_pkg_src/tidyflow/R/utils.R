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

has_preprocessor_grid <- function(x) {
  "grid" %in% names(x$pre$actions)
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

has_tune <- function(x) {
  UseMethod("has_tune")
}

has_tune.recipe <- function(x) {
  steps <- x$steps

  detect_tune1 <- function(step) any(vapply(step, function(x) is.language(x), logical(1)))

  tune_calls <- vapply(steps, detect_tune1, logical(1))

  # Even if tune is defined as tune("a new name"), by converting
  # the call object to character, the first slot is always the
  # function call. Any args specified will be slot 2, 3, 4, ...
  detect_tune2 <- function(step) {
    step <- Filter(function(x) length(x) != 0, step)
    any(vapply(step, function(x) as.character(x)[[1]] == "tune", logical(1)))
  }

  res <- vapply(steps[tune_calls], detect_tune2, logical(1))

  # Any argument has a tune() call?
  any(res)
}

has_tune.formula <- function(x) {
  FALSE
}

has_tune.model_spec <- function(x) {
  possible_tune_args <- lapply(x$args, rlang::eval_tidy)

  available_args <- vapply(possible_tune_args,
                           Negate(is.null),
                           FUN.VALUE = logical(1))


  possible_tune_args <- possible_tune_args[available_args]

  any_tune_args <- any(
    vapply(possible_tune_args,
           function(x) as.character(x)[[1]] == "tune",
           FUN.VALUE = logical(1))
  )

  any_tune_args
}

has_tune.tidyflow <- function(x) {
  # A formula never has tune params, so I hack it to return ~1
  has_tune(recipe_or_formula(x)) | has_tune(pull_tflow_spec(x))
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

purge_action_grid <- function(x) {
  purge_(x$pre$actions, "grid")
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

purge_results_grid <- function(x) {
  purge_(x$pre$results, "grid")
}

purge_results_formula <- function(x) {
  purge_(x$pre$results, "formula")
}

purge_results_recipe <- function(x) {
  purge_(x$pre$results, "recipe")
}

combine_outcome_preds <- function(mold) {
  mold_names <- c("predictors", "outcomes", "blueprint", "extras")

  if (any(mold_names %in% names(mold))) {
    mold <- cbind(mold$outcomes, mold$predictors)
  }

  mold
}

# To compare equality of models, elapsed time is sometimes
# the only different thing
strip_elapsed <- function(x) {
  x$fit$fit$fit$elapsed <- NULL
  x
}

# To compare equality with all.equal in tests. all.equal
# doesn't support list columns in tibbles. Here we just
# turn to dataframe all rsplit objects
rsplit2df <- function(x) {
  x$pre$results$resample <- as.data.frame(x$pre$results$resample)
  x$fit$fit$tuning <- as.data.frame(x$fit$fit$tuning)

  # The attribute parameters from the fit tuning result
  # is a tibble. tibble has removed the method all.equal.tbl
  # https://github.com/tidyverse/dplyr/issues/2751
  # Here I inspect the attribute parameters and turn it into
  # a data frame. This way, all.equal.list can compare the data
  # frames as list columns.
  x <- tblattr_2_df(x$fit$fit$tuning)

  x
}

tblattr_2_df <- function(x) {
  attr(x, "parameters") <- as.data.frame(attr(x, "parameters"))
  x
}

compact <- function(.x) {
  Filter(length, .x)
}
