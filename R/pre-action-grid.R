#' Add a split specification to a tidyflow
#'
#' @description
#' - `plug_grid()` specifies the type of grid used in the model tuning. It
#'   accepts a function \code{.f} that will be fed the tuning parameters defined
#'   in the model and the recip. Only functions which return a \code{param_grid}
#'   object will be allowed. See package \code{\link[dials]{dials}} and
#'   the details section. If a model has been fit before adding the grid,
#'   it will need to be refit.
#'
#' - `drop_grid()` removes the grid specification from the tidyflow. Note
#'   that it keeps other preprocessing steps such as the recipe and model.
#'
#' - `replace_grid()` first removes the grid, then adds a new grid
#'   specification. Any model that has already been fit based on this
#'   split will need to be refit.
#'
#' @details
#' The grid specification is an optional step in the tidyflow. You can add a
#' dataframe, prepare a recipe and fit the model without adding a grid
#' specification.
#'
#' The tuning parameters defined in the model and recipe are extracted
#' and passed to \code{.f}. This should return an object of class
#' \code{param_grid}. These functions come from the \code{\link[dials]{dials}}
#' package.
#'
#' @param x A tidyflow
#'
#' @param .f A function which will be passed the tuned arguments from the model
#' and recipe. Must return an object of class \code{param_grid}. See package
#' \code{\link[dials]{dials}}.
#'
#' @param ... arguments passed to \code{.f}. The processing of \code{...}
#' respects the quotation rules from \code{.f}. In other words, if the function
#' allows variables as strings \strong{and} as names, the user can specify both.
#' See the example sections.
#'
#' @return
#' `x`, updated with either a new or removed grid specification.
#'
#' @export
#' @examples
#' library(rsample)
#'
#' wf <- tidyflow()
#' wf <- plug_data(wf, mtcars)
#' 
#' # Strata as string
#' wf <- plug_grid(wf, initial_split, prop = 0.8, strata = "cyl")
#'
#' wf
#' 
#' # Strata as unquoted name
#' wf <- replace_grid(wf, initial_split, prop = 0.8, strata = cyl)
#'
#' wf
#'
#' drop_grid(wf)
#'
#' # New split function
#' replace_grid(wf, initial_time_split)
#'
plug_grid <- function(x, .f, ...) {
  .dots <- enquos(...)

  ## if (!is_uniquely_named(.dots)) {
  ##   abort("Arguments in `...` for `.f` should be named")
  ## }

  # Capture name of function to put as name in action list
  # Easier for printing the split specification
  .f <- enquo(.f)
  name_f <- quo_text(.f)
  action <- new_action_grid(eval_tidy(.f), .dots, name_f)
  plug_action(x, action, "grid")
}

#' @rdname plug_grid
#' @export
drop_grid <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_grid(x)) {
    rlang::warn("The tidyflow does not have a grid specification.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(actions = purge_action_grid(x),
                        mold = x$data,
                        seed = x$pre$seed,
                        results = purge_results_grid(x)),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname plug_grid
#' @export
replace_grid <- function(x, .f, ...) {
  x <- drop_grid(x)
  .f <- enquo(.f)
  plug_grid(x, !!.f, ...)
}

# ------------------------------------------------------------------------------
fit.action_grid <- function(object, tflow) {

  if (!has_spec(tflow)) {
      abort("The tidyflow does not have a model specification to extract the tuning parameters") #nolintr
  }

  # Including recipe + model
  all_params <- parameters(tflow)

  ## object[[2]] are the arguments as quosures
  args <- lapply(object[[2]], eval_tidy)

  grid_res <- rlang::exec(
    # function body
    object[[1]],
    all_params
  )

  if (!inherits(grid_res, "param_grid")) {
    abort("The grid function should return an object of class `param_grid`.")
  }

  tflow$pre$results$grid <- grid_res
  
  # All pre steps return the `tflow`
  tflow
}

# Exclude blueprint; it doesn't apply to data
new_action_grid <- function(.f, .dots, name_f) {
  if (!is.function(.f)) {
    abort("`.f` must be a function for creating a grid of tuning parameters")
  }

  new_action_pre(
    # Capture function name, function body and args for later to apply on
    # data. The name of f is just for printing the tidyflow
    !!name_f := .f,
    args = .dots,
    subclass = "action_grid"
  )
}

# TODO:
# test that when fitting a tidyflow, it fails if no model has been specified
# test that when extracting parameters from lm, params is empty and throws a
# warning
# test that params get extracted when there are tune() in both recipe and model
# test that when extracting params from a model with no tuning, it returns empty
# test that when no model is present, params returns empty params
# test that when no recipe is present params returns empty params
# test that when model is present but not recipe, model params are returned
# test that when recipe is present but not model, recipe params are returned
