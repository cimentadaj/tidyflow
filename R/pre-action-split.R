#' Add a split specification to a tidyflow
#'
#' @description
#' - `add_split()` specifies the type of splitting used in the analysis. It
#'   accepts a function \code{.f} that will be applied to the data. Only
#'   functions which return an \code{rsplit} object will be allowed. See
#'   package \code{\link[rsample]{rsample}} and the details section. If a model
#'   has been fit before adding the split, it will need to be refit.
#'
#' - `remove_split()` removes the split specification from the tidyflow. Note
#'   that it keeps other preprocessing steps such as the recipe.
#'
#' - `update_split()` first removes the split, then adds a new split 
#'   specification. Any model that has already been fit based on this
#'   split will need to be refit.
#'
#' @details
#' The split specification is an optional step in the tidyflow. You can add a
#' dataframe, prepare a recipe and fit the model without splitting into
#' training/testing.
#'
#' When applied to the data, the function \code{.f} must return an object
#' of class \code{rsplit}. These are functions which come from the
#' \code{\link[rsample]{rsample}} package.
#'
#' @param x A tidyflow
#'
#' @param .f A function to be applied to the dataset in the tidyflow. Must
#' return an object of class \code{rsplit}. See package
#' \code{\link[rsample]{rsample}}.
#'
#' @param ... arguments passed to \code{.f}. These arguments must be named.
#' The processing of \code{...} respects the quotation rules from \code{.f}.
#' In other words, if the function allows variables as strings \strong{and}
#' as names, the user can specify both. See the example sections.
#'
#' @return
#' `x`, updated with either a new or removed split specification.
#'
#' @export
#' @examples
#' library(rsample)
#'
#' wf <- tidyflow()
#' wf <- add_data(wf, mtcars)
#' 
#' # Strata as string
#' wf <- add_split(wf, initial_split, prop = 0.8, strata = "cyl")
#'
#' wf
#' 
#' # Strata as unquoted name
#' wf <- update_split(wf, initial_split, prop = 0.8, strata = cyl)
#'
#' wf
#'
#' remove_split(wf)
#'
#' # New split function
#' update_split(wf, initial_time_split)
#'
add_split <- function(x, .f, ...) {

  ## In case you fit the model and **then** add a split.
  ## This fun removes everything.
  x <- update_fit(x)

  ## if (has_preprocessor_resample(x)) {
  ##   abort("A tidyflow must never have a resample before splitting the data")
  ## }

  .dots <- enquos(...)

  if (!is_uniquely_named(.dots)) {
    abort("Arguments in `...` for `.f` should be named")
  }

  # Capture name of function to put as name in action list
  # Easier for printing the split specification
  .f <- enquo(.f)
  name_f <- quo_text(.f)
  action <- new_action_split(eval_tidy(.f), .dots, name_f)
  add_action(x, action, "split")
}

#' @rdname add_split
#' @export
remove_split <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_split(x)) {
    rlang::warn("The tidyflow does not have a split specification.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(actions = purge_action_split(x), mold = x$data),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_split
#' @export
update_split <- function(x, .f, ...) {
  x <- remove_split(x)
  .f <- enquo(.f)
  add_split(x, !!.f, ...)
}

# ------------------------------------------------------------------------------
fit.action_split <- function(object, tidyflow) {

  ## object[[2]] are the arguments as quosures
  args <- lapply(object[[2]], eval_tidy)

  split_res <- rlang::exec(
    # function body
    object[[1]],
    # function args
    tidyflow$pre$mold,
    !!!args
  )

  if (!inherits(split_res, "rsplit")) {
    abort("The split function should return an object of class `rsplit`.")
  }

  tidyflow$pre$mold <- rsample::training(split_res)
  tidyflow$pre$actions$split$testing <- rsample::testing(split_res)
  
  # All pre steps return the `wflow`
  tidyflow
}

# Exclude blueprint; it doesn't apply to data
new_action_split <- function(.f, .dots, name_f) {
  if (!is.function(.f)) {
    abort("`.f` must be a function for splitting the dataset")
  }

  new_action_pre(
    # Capture function name, function body and args for later to apply on
    # data. The name of f is just for printing the tidyflow
    !!name_f := .f,
    args = .dots,
    subclass = "action_split"
  )
}

update_fit <- function(x) {
  if (x$trained) {
    x <-
      new_tidyflow(
        data = x$data,
        pre = new_stage_pre(actions = x$pre$actions, mold = x$data),
        fit = new_stage_fit(actions = x$fit$actions),
        post = new_stage_post(actions = x$post$actions),
        trained = FALSE
      )
  }

  x
}
