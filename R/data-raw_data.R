#' Add a data to a workflow
#'
#' @description
#' - `add_data()` specifies the raw data used in the analysis. This data is used
#'   for the preprocessing and fitting the model.
#'
#' - `remove_data()` removes the data from the workflow.
#'
#' - `update_data()` first removes the data, then replaces the previous
#'   data with the new one. Any model that has already been fit based on this
#'   data will need to be refit.
#'
#' @details
#' To fit a workflow, `data` _must_ be specified.
#'
#' @param x A workflow
#'
#' @param data A data frame or tibble.
#'
#' @param ... Not used.
#'
#'
#' @return
#' `x`, updated with either a new or removed data frame.
#'
#' @export
#' @examples
#'
#' wf <- workflow()
#' wf <- add_data(wf, mtcars)
#' wf
#'
#' remove_data(wf)
#'
#' update_data(wf, iris)
#'
add_data <- function(x, data, ...) {
  ellipsis::check_dots_empty()

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  if (has_raw_data(x)) {
    rlang::abort("A data frame has already been added to this workflow")
  }

  # At the beginning, mold is the **mutable** data, which will be updated
  # with every action and data the pristine state.
  x$data <- data
  x$pre$mold <- data
  x
}

#' @rdname add_data
#' @export
remove_data <- function(x) {
  validate_is_workflow(x)

  if (!has_raw_data(x)) {
    rlang::warn("The workflow has no data to remove.")
  }

  new_workflow(
    # data is not a stage: I think of it as the pristine state
    # of the main character in the analysis. Everything
    # after data is indeed a stage which contains actions
    data = NULL,
    pre = new_stage_pre(actions = x$pre$actions, mold = NULL),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_data
#' @export
update_data <- function(x, data, ...) {
  ellipsis::check_dots_empty()
  x <- remove_data(x)
  add_data(x, data)
}
