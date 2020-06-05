#' Extract the parameters of a tidyflow
#'
#' @param x A tidyflow
#' @param ... Not used.
#' @return A tibble of class parameters
#'
#' @details
#' \code{parameters} extracts the \code{\link[tune]{tune}} parameters
#' from both the model and recipe. In principle, the user never should need to
#' extract them. Behind the scenes, \code{tidyflow} extracts them and generates
#' random values based on the defaults by \code{link[dials]{dials}}. However,
#' the user might want to extract the parameters to figure out which parameters
#' were correctly supplied to the \code{tidyflow}.
#'
#' This function can be used on the \code{tidyflow} both before fitting the
#' model and after the model fit.
#'
#' @name parameters-tidyflow
#' @export
#' 
#' @examples
#' library(rsample)
#' library(tune)
#' library(dials)
#' library(recipes)
#' library(parsnip)
#' 
#' tflow <-
#'   mtcars %>%
#'   tidyflow() %>%
#'   plug_split(initial_split) %>%
#'   plug_formula(mpg ~ .) %>% 
#'   plug_resample(vfold_cv) %>%
#'   plug_grid(grid_regular) %>%
#'   plug_model(set_engine(linear_reg(), "lm"))
#' 
#' # No tuning parameters
#' tidyflow::parameters(tflow)
#' 
#' # But if we add tuning parameters, we can check which ones:
#' tflow %>%
#'   drop_formula() %>% 
#'   plug_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
#'   tidyflow::parameters()
#' 
#' # parameters extracts both the tuning parameters from the recipe and
#' # model:
#' tflow <-
#'   tflow %>%
#'   drop_formula() %>% 
#'   plug_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
#'   replace_model(set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet"))
#' 
#' tidyflow::parameters(tflow)
#' 
#' # This can serve well to refresh your memory on which tuning
#' # parameters are present and then override the custom values
#' # in `plug_grid`:
#' \dontrun{
#'   res <-
#'     tflow %>%
#'     replace_grid(grid_regular, penalty = penalty(c(-1, 0))) %>%
#'     fit()
#'   res
#' }
#'
#'
parameters <- function(x, ...) {
  UseMethod("parameters")
}

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
      molded_data <- combine_outcome_preds(pull_tflow_mold(x))
      recipe <- pull_tflow_preprocessor(x)(molded_data)
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
