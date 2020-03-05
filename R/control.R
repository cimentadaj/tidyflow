#' Control object for a tidyflow
#'
#' `control_tidyflow()` holds the control parameters for a tidyflow.
#'
#' @param control_parsnip A parsnip control object. If `NULL`, a default control
#'   argument is constructed from [parsnip::control_parsnip()].
#'
#' @return
#' A `control_tidyflow` object for tweaking the tidyflow fitting process.
#'
#' @export
#' @examples
#' control_tidyflow()
control_tidyflow <- function(control_parsnip = NULL) {
  control_parsnip <- check_control_parsnip(control_parsnip)

  data <- list(
    control_parsnip = control_parsnip
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
