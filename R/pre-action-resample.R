#' Add a resample specification to a tidyflow
#'
#' @description
#' - `add_resample()` specifies the type of resample used in the analysis. It
#'   accepts a function \code{.f} that will be applied to the data. Only
#'   functions which return an \code{rset} object will be allowed. See
#'   package \code{\link[rsample]{rsample}} and the details section.
#'
#' - `drop_resample()` removes the resample specification from the tidyflow.
#'   Note that it keeps other preprocessing steps such as the recipe.
#'
#' - `replace_resample()` first removes the resample, then adds a new resample
#'   specification. Any model that has already been fit based on this
#'   split will need to be refit.
#'
#' @details
#' The resample specification is an optional step in the tidyflow. You can add a
#' dataframe, prepare a recipe and fit the model without splitting into
#' training/testing.
#'
#' When applied to the data, the function \code{.f} must return an object
#' of class \code{rset}. These are functions which come from the
#' \code{\link[rsample]{rsample}} package.
#'
#' @param x A tidyflow
#'
#' @param .f A function to be applied to the dataset in the tidyflow. Must
#' return an object of class \code{rset}. See package
#' \code{\link[rsample]{rsample}}.
#'
#' @param ... arguments passed to \code{.f}. These arguments must be named.
#' The processing of \code{...} respects the quotation rules from \code{.f}.
#' In other words, if the function allows variables as strings \strong{and}
#' as names, the user can specify both. See the example sections.
#'
#' @return
#' `x`, updated with either a new or removed resample specification.
#'
#' @export
#' @examples
#' library(rsample)
#'
#' wf <- tidyflow()
#' wf <- add_data(wf, mtcars)
#' 
#' # Strata as string
#' wf <- add_resample(wf, vfold_cv, v = 5, strata = "cyl")
#'
#' wf
#' 
#' # Strata as unquoted name
#' wf <- replace_resample(wf, initial_split, v = 5, strata = cyl)
#'
#' wf
#'
#' drop_resample(wf)
#'
#' # New split function
#' replace_resample(wf, bootstraps)
#'
add_resample <- function(x, .f, ...) {
  .dots <- enquos(...)

  if (!is_uniquely_named(.dots)) {
    abort("Arguments in `...` for `.f` should be named")
  }

  # Capture name of function to put as name in action list
  # Easier for printing the split specification
  .f <- enquo(.f)
  name_f <- quo_text(.f)
  action <- new_action_resample(eval_tidy(.f), .dots, name_f)
  add_action(x, action, "resample")
}

#' @rdname add_resample
#' @export
drop_resample <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_resample(x)) {
    rlang::warn("The tidyflow does not have a resample specification.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(actions = purge_action_resample(x)),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_resample
#' @export
replace_resample <- function(x, .f, ...) {
  x <- drop_resample(x)
  .f <- enquo(.f)
  add_resample(x, !!.f, ...)
}

# ------------------------------------------------------------------------------

fit.action_resample <- function(object, wflow) {

  # Since a tidyflow will alows need to have a formula or recipe
  # the result of mold when it reaches a resample, will always be
  # a mold structure. Let's convert that to a data frame
  mold <- combine_outcome_preds(wflow$pre$mold)

  ## object[[2]] are the arguments as quosures
  args <- lapply(object[[2]], eval_tidy)

  resample_res <- rlang::exec(
    # function body
    object[[1]],
    # function args
    mold,
    !!!args
  )

  if (!inherits(resample_res, "rset")) {
    abort("The resample function should return an object of class `rset`.")
  }

  wflow$pre$actions$resample$rset_res <- resample_res
  
  # All pre steps return the `wflow`
  wflow
}

# Exclude blueprint; it doesn't apply to data
new_action_resample <- function(.f, .dots, name_f) {
  if (!is.function(.f)) {
    abort("`.f` must be a function for resampling the dataset")
  }

  new_action_pre(
    # Capture function name, function body and args for later to apply on
    # data. The name of f is just for printing the tidyflow
    !!name_f := .f,
    args = .dots,
    subclass = "action_resample"
  )
}
