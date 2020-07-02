#' Control object for a tidyflow
#'
#' `control_tidyflow()` holds the control parameters for a tidyflow.
#' It allows you to specify control options for `parsnip`, `resamples`
#' and `grid`.
#'
#' @param control_parsnip A parsnip control object. If `NULL`, a default control
#'   argument is constructed from \code{\link[parsnip]{control_parsnip}}.
#'
#' @param control_resamples A resamples control object. If `NULL`, a default control
#'   argument is constructed from \code{\link[tune:control_grid]{control_resamples}}.
#'
#' @param control_grid A grid control object. If `NULL`, a default control
#'   argument is constructed from \code{\link[tune]{control_grid}}.
#' 
#' @return
#' A `control_tidyflow` object for tweaking the tidyflow fitting/tuning process.
#'
#' @export
#' @examples
#' \dontrun{
#' library(parsnip)
#' library(rsample)
#' library(tune)
#' library(tidyflow)
#' 
#' # Build tidyflow
#' tflow <-
#'   mtcars %>%
#'   tidyflow() %>%
#'   plug_split(initial_split) %>%
#'   plug_formula(mpg ~ .) %>%
#'   plug_resample(vfold_cv, v = 2) %>% 
#'   plug_model(set_engine(linear_reg(), "lm"))
#'
#' # For each resample object, we want the predictions
#' ct <- control_tidyflow(control_resample = control_resamples(save_pred = TRUE,
#'                                                             verbose = TRUE))
#' 
#' # Specify the control object
#' fit_m <- fit(tflow, control = ct)
#' fit_m
#' 
#' # Extract the predictions
#' fit_m %>%
#'   pull_tflow_fit_tuning() %>%
#'   .[[".predictions"]]
#'
#' # `control_resamples` is only used when there is a resample but not
#' # grid. When there is a resample and a grid, `control_grid` should be
#' # used.
#' ct <- control_tidyflow(control_grid = control_grid(verbose = TRUE,
#'                                                    save_pred = TRUE))
#' 
#' # Since there is no grid specification, this is ignored.
#' # No messages should be printed nor a new .predictions
#' # columns in the result
#' fit_m <- fit(tflow, control = ct)
#' fit_m
#'
#' # control_parsnip controls the options of the model
#' # For example, verbosity controls the messags of the model
#' ct <- control_tidyflow(control_parsnip = control_parsnip(verbosity = 2))
#'
#' # Run a regularized regression with only one independent variable.
#' # This is not possible, it will raise an error and we will see it
#' # because of verbosity
#' res <-
#'   tflow %>%
#'   replace_model(set_engine(linear_reg(penalty = 0, mixture = 1), "glmnet")) %>%
#'   replace_formula(mpg ~ cyl) %>% 
#'   fit(control = ct)
#' }
control_tidyflow <- function(control_parsnip = NULL,
                             control_resamples = NULL,
                             control_grid = NULL) {
  
  control_parsnip <- check_control_parsnip(control_parsnip)
  control_resamples <- check_control_resamples(control_resamples)
  control_grid <- check_control_grid(control_grid)

  data <- list(
    control_parsnip = control_parsnip,
    control_resamples = control_resamples,
    control_grid = control_grid
  )

  structure(data, class = "control_tidyflow")
}

#' @export
print.control_tidyflow <- function(x, ...) {
  cat("<control_tidyflow>")
  invisible()
}

check_control_parsnip <- function(x) {
  if (is.null(x)) {
    x <- parsnip::control_parsnip()
  }

  if (!inherits(x, "control_parsnip")) {
    abort("`control_parsnip` must be a 'control_parsnip' object.")
  }

  x
}

check_control_resamples <- function(x) {
  if (is.null(x)) {
    x <- tune::control_resamples()
  }

  x <- coerce_control(x, "control_resamples")

  if (!inherits(x, "control_resamples")) {
    abort("`control_resamples` must be a 'control_resamples' object.")
  }

  x
}

check_control_grid <- function(x) {
  if (is.null(x)) {
    x <- tune::control_grid()
  }

  x <- coerce_control(x, "control_grid")

  if (!inherits(x, "control_grid")) {
    abort("`control_grid` must be a 'control_grid' object.")
  }

  x
}


# TODO
# Seems tune::control_* functions don't have yet a custom
# control_* class. Until then, coerce them to the specific classes
# such that it fits tidyflow.
# https://github.com/tidymodels/tune/issues/183
coerce_control <- function(x, cls) {
  if (!inherits(x, cls)) {
    class(x) <- c(cls, class(x))
  }
  x
}
