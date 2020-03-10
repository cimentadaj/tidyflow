#' Extract the parameters of a tidyflow
#'
#' @param x A data frame or tibble used to begin the tidyflow. This is
#' optional as the data can be specified with [plug_data()]. 
#' 
#' @param ... Not used.
#' @return A tibble of class parameters
#'
#' @examples
#' ## TODO
#' 5 + 5
#' 
#' @export
parameters.tidyflow <- function(x, ...) {

  model <- try(tidyflow::pull_tflow_spec(x), silent = TRUE)
  if (inherits(model, "try-error")) {
    param_data <- dials::parameters(list())
  } else {
    param_data <- tune::parameters(model)
  }

  if (has_preprocessor_recipe(x)) {
    if (has_raw_data(x)) {
      recipe <- pull_tflow_preprocessor(x)(pull_tflow_mold(x))
      recipe_param_data <- tune::parameters(recipe)
      param_data <- rbind(param_data, recipe_param_data)
    } else {
      rlang::abort("The raw data must be specified to extract the parameters of the recipe. Did you want `plug_data`?") #nolintr
    }
  }

  dials::parameters_constr(
    param_data$name,
    param_data$id,
    param_data$source,
    param_data$component,
    param_data$component_id,
    param_data$object
  )
}
