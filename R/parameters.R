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
## parameters.tidyflow <- function(x, ...) {
##   model <- tidyflow::pull_tidyflow_spec(x)
##   param_data <- dials::parameters(model)
##   if (has_preprocessor_recipe(x)) {
##     if (has_raw_data(x)) {
##       recipe <- pull_tidyflow_preprocessor(x)(x$pre$mold)
##       recipe_param_data <- dials::parameters(recipe)
##       param_data <- dplyr::bind_rows(param_data, recipe_param_data)
##     } else {
##       rlang::abort("The raw data must be specified to extract the parameters of the recipe. Did you want `plug_data`?") #nolintr
##     }
##   }

##   dials::parameters_constr(
##     param_data$name,
##     param_data$id,
##     param_data$source,
##     param_data$component,
##     param_data$component_id,
##     param_data$object
##   )
## }
