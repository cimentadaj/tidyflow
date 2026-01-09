#' Add a tuning method specification to a tidyflow
#'
#' @description
#' - `plug_tune()` specifies the tuning method used for hyperparameter
#'   optimization. It supports both grid search and iterative methods like
#'   Bayesian optimization. If not specified, the default is `tune_grid`.
#'
#' - `drop_tune()` removes the tune specification from the tidyflow.
#'
#' - `replace_tune()` first removes the tune method, then adds a new one.
#'
#' @details
#' The tune specification is optional. If not specified, `plug_grid()` will
#' use `tune::tune_grid()` by default. Using `plug_tune()` allows you to
#' choose alternative tuning methods:
#'
#' \itemize{
#'   \item `tune_grid`: Exhaustive grid search (default)
#'   \item `tune_bayes`: Bayesian optimization using Gaussian processes
#'   \item `tune_sim_anneal`: Simulated annealing optimization
#'   \item `tune_race_anova`: Racing with ANOVA models
#'   \item `tune_race_win_loss`: Racing with win/loss statistics
#' }
#'
#' @param x A tidyflow
#' @param method The tuning method function. One of `tune_grid`, `tune_bayes`,
#'   `tune_sim_anneal`, `tune_race_anova`, or `tune_race_win_loss`.
#' @param ... Additional arguments passed to the tuning function. Common
#'   arguments include `iter` for iterative methods, `initial` for Bayesian
#'   optimization, etc.
#'
#' @return The tidyflow `x`, updated with either a new or removed tune specification.
#'
#' @export
#' @examples
#' \dontrun{
#' library(parsnip)
#' library(rsample)
#' library(tune)
#' library(dials)
#'
#' # Use Bayesian optimization instead of grid search
#' mod <-
#'   mtcars %>%
#'   tidyflow() %>%
#'   plug_split(initial_split) %>%
#'   plug_formula(mpg ~ .) %>%
#'   plug_resample(vfold_cv, v = 5) %>%
#'   plug_model(set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet")) %>%
#'   plug_grid(grid_regular, levels = 2) %>%
#'   plug_tune(tune_bayes, iter = 10)
#'
#' res <- fit(mod)
#' }
#'
plug_tune <- function(x, method = tune::tune_grid, ...) {
  validate_is_tidyflow(x)

  .dots <- enquos(...)
  method <- enquo(method)
  method_name <- quo_text(method)
  method_fn <- eval_tidy(method)

  action <- new_action_tune(method_fn, .dots, method_name)
  plug_action(x, action, "tune")
}

#' @rdname plug_tune
#' @export
drop_tune <- function(x) {
  validate_is_tidyflow(x)

  if (!has_tune_method(x)) {
    rlang::warn("The tidyflow does not have a tune specification.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(actions = purge_action_tune(x),
                        mold = x$data,
                        seed = x$pre$seed,
                        results = purge_results_tune(x)),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname plug_tune
#' @export
replace_tune <- function(x, method = tune::tune_grid, ...) {
  x <- drop_tune(x)
  method <- enquo(method)
  plug_tune(x, !!method, ...)
}

# ------------------------------------------------------------------------------
# Internal functions

new_action_tune <- function(method, .dots, method_name) {
  if (!is.function(method)) {
    cli::cli_abort(c(
      "Invalid tune method.",
      "x" = "{.arg method} must be a tuning function.",
      "i" = "Use {.fn tune_grid}, {.fn tune_bayes}, etc."
    ))
  }

  new_action_pre(
    !!method_name := method,
    args = .dots,
    subclass = "action_tune"
  )
}

fit.action_tune <- function(object, x) {
  # Store the tune method and args in results for use during model fitting
  x$pre$results$tune <- list(
    method = object[[1]],
    args = lapply(object$args, eval_tidy),
    method_name = names(object)[1]
  )

  x
}

# Helper functions
has_tune_method <- function(x) {
  "tune" %in% names(x$pre$actions)
}

purge_action_tune <- function(x) {
  purge_(x$pre$actions, "tune")
}

purge_results_tune <- function(x) {
  purge_(x$pre$results, "tune")
}
