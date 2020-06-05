#' Add a recipe to a tidyflow
#'
#' @description
#' - `plug_recipe()` specifies the type of recipe used in the analysis. It
#'   accepts a function \code{.f} that will be applied to the data. Only
#'   functions which return a \code{recipe} object will be allowed. See
#'   package \code{\link[recipes]{recipes}} for how to create a recipe.
#'
#' - `drop_recipe()` removes the recipe function from the tidyflow. Note
#'   that it keeps other preprocessing steps such as the split and resample.
#'
#' - `replace_recipe()` first removes the recipe function, then adds the new
#'   recipe function. Any model that has already been fit based on this
#'   recipe will need to be refit.
#'
#' @param x A tidyflow
#' 
#' @param .f A function or a formula with a recipe inside. See the details
#' section.
#'
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'   If `NULL`, [hardhat::default_recipe_blueprint()] is used.
#'
#' @details
#'
#' To fit a tidyflow, one of `plug_formula()` or `plug_recipe()` _must_ be
#' specified, but not both.
#'
#' \code{.f} can be either a function or a formula. In either case, both
#' things should have only one argument and return the recipe applied to
#' the only argument, which is assumed to be the data.
#' 
#' \itemize{
#'   \item If a function is supplied, it is assumed that there is one argument
#'   and that argument is for the data. The output should be the recipe
#'   applied to the main argument. The *function* is used as is.
#'
#'   \item If a *formula*, e.g. \code{~ recipe(mpg ~ cyl, data = .)}, it is
#'   converted to a function. It is also assumed that the first argument in the
#'   recipe function is passed to the data. Other arguments will be ignored.
#'   If a *formula*, the argument name can be either `.` or `.x`. See the
#'   examples section for more details.
#' }
#'
#' Since the recipe step in a \code{tidyflow} is not the ideal step for
#' exploration, we suggest that the user constructs the recipe outside
#' the \code{tidyflow} and applies it to the data beforehand, just to make sure
#' it works. After making sure the recipe can be fitted without errors, the user
#' can provide the function or formula for the recipe. Defining a recipe without
#' testing on the data can lead to errors on \code{recipe} that are best fixed
#' in an interactive fashion.
#'
#' @return
#' The tidyflow `x`, updated with either a new or removed recipe function.
#'
#' @export
#' @examples
#' library(recipes)
#' library(parsnip)
#'
#' # Passing a function to `plug_recipe`
#' recipe_fun <- function(.x) {
#'   recipe(mpg ~ ., data = .x) %>%
#'    step_center(all_predictors()) %>%
#'    step_scale(all_predictors())
#' }
#'
#' # Let's make sure that it works with the data first
#' recipe_fun(mtcars)
#'
#' # Specify the function to be applied to the data in `plug_recipe`
#' tflow <-
#'  mtcars %>%
#'  tidyflow() %>%
#'  plug_recipe(recipe_fun) %>%
#'  plug_model(set_engine(linear_reg(), "lm"))
#' 
#' # Fit the model
#' fit(tflow)
#'
#' # Specify a formula of a recipe. Remove the old one and specify one on the
#' # fly:
#' tflow %>%
#'  replace_recipe(~ recipe(mpg ~ cyl, data = .) %>% step_log(cyl, base = 10)) %>%
#'  fit()
#'
#' # Note how the function argument can be either `.` or `.x`
#' tflow %>%
#'  replace_recipe(~ {
#'   .x %>% 
#'    recipe(mpg ~ cyl + am) %>%
#'     step_log(cyl, base = 10) %>%
#'     step_mutate(am = factor(am)) %>%
#'     step_dummy(am)
#'  }) %>%
#'  fit()
#' 
plug_recipe <- function(x, .f, ..., blueprint = NULL) {
  ellipsis::check_dots_empty()
  validate_recipes_available()
  action <- new_action_recipe(.f, blueprint)
  plug_action(x, action, "recipe")
}

#' @rdname plug_recipe
#' @export
drop_recipe <- function(x) {
  validate_is_tidyflow(x)

  if (!has_preprocessor_recipe(x)) {
    rlang::warn("The tidyflow has no recipe preprocessor to remove.")
  }

  new_tidyflow(
    data = x$data,
    pre = new_stage_pre(actions = purge_action_recipe(x),
                        mold = x$data,
                        seed = x$pre$seed,
                        results = purge_results_recipe(x)),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname plug_recipe
#' @export
replace_recipe <- function(x, .f, ..., blueprint = NULL) {
  ellipsis::check_dots_empty()
  x <- drop_recipe(x)
  plug_recipe(x, .f, blueprint = blueprint)
}

# ------------------------------------------------------------------------------
fit.action_recipe <- function(object, x) {
  recipe_fun <- object$recipe
  blueprint <- object$blueprint
  molded_data <- combine_outcome_preds(x$pre$mold)
  rcp_data <- recipe_fun(molded_data)
  
  if (!is_recipe(rcp_data)) {
    abort("The recipe function `.f` should return an object of class `recipe`")
  }

  if (has_tune(rcp_data) && !has_preprocessor_grid(x)) {
    abort("The recipe contains parameters with `tune()` but no grid specification has been made. Did you want `plug_grid`?") #nolintr
  }
  
  # Keep recipe around
  x$pre$results$recipe <- rcp_data

  # Only if the recipe or model has a tune, we return the unprepped data to mold
  # This is because you the prepping is done via tune_grid. This
  # is all taken care of in fit.action_model.
  
  if (has_tune(rcp_data) || has_tune(pull_tflow_spec(x))) {
    var_df <- rcp_data$var_info
    y_var <- var_df[var_df$role == "outcome", "variable", drop = TRUE]
    x_vars <- var_df[var_df$role != "outcome", "variable", drop = TRUE]
    x$pre$mold <- hardhat::mold(molded_data[x_vars], molded_data[y_var])
  } else {
    x$pre$mold <- hardhat::mold(rcp_data,
                                molded_data,
                                blueprint = blueprint)
  }


  # All pre steps return the `tidyflow`
  x
}

# ------------------------------------------------------------------------------

check_conflicts.action_recipe <- function(action, x) {
  pre <- x$pre

  if (has_action(pre, "formula")) {
    abort("A recipe cannot be added when a formula already exists.")
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_recipe <- function(.f, blueprint) {
  .f <- try(rlang::as_function(.f), silent = TRUE)

  if (inherits(.f, "try-error")) {
    abort("`.f` must be a function with a recipe for applying to the dataset")
  }

  if (is.null(blueprint)) {
    blueprint <- hardhat::default_recipe_blueprint()
  } else if (!is_recipe_blueprint(blueprint)) {
    abort("`blueprint` must be a hardhat 'recipe_blueprint'.")
  }

  new_action_pre(
    recipe = .f,
    blueprint = blueprint,
    subclass = "action_recipe"
  )
}

is_recipe <- function(x) {
  inherits(x, "recipe")
}

is_recipe_blueprint <- function(x) {
  inherits(x, "recipe_blueprint")
}
