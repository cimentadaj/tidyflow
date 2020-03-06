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
#' @details
#' To fit a tidyflow, one of `plug_formula()` or `plug_recipe()` _must_ be
#' specified, but not both.
#'
#' @param x A tidyflow
#' 
#' @param .f A function or a formula
#'
#' If a *function*, it is used as is.
#'
#' If a *formula*, e.g. ‘~ recipe(mpg ~ cyl, data = .x)’, it is converted to a
#' function. The only the first argument in the recipe function is passed
#' to the data. Other arguments will be ignored. If a *formula*, the argument
#' name can be either `.` or `.x`. See the examples section for more details.
#' 
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'   If `NULL`, [hardhat::default_recipe_blueprint()] is used.
#'
#' @return
#' `x`, updated with either a new or removed recipe function.
#'
#' @export
#' @examples
#' library(recipes)
#' library(parsnip)
#'
#' recipe_fun <- function(.x) {
#'   recipe(mpg ~ ., data = .x) %>%
#'    step_center(all_predictors()) %>%
#'    step_scale(all_predictors())
#' }
#'
#' # Specify an already created recipe function
#' tflow <-
#'  mtcars %>%
#'  tidyflow() %>%
#'  plug_recipe(recipe_fun) %>%
#'  plug_model(set_engine(linear_reg(), "lm"))
#' 
#' # Fit the model
#' tflow %>%
#'  fit()
#'
#' # Remove the old recipe, specify one on the fly and fit again
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

  ## For when `...` is supported
  ## .dots <- enquos(...)
  ## if (!is_uniquely_named(.dots)) {
  ##   abort("Arguments in `...` for `.f` should be named")
  ## }

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

fit.action_recipe <- function(object, tflow) {
  recipe_fun <- object$recipe
  blueprint <- object$blueprint
  molded_data <- tflow$pre$mold
  rcp_data <- recipe_fun(molded_data)

  if (!is_recipe(rcp_data)) {
    abort("The recipe function `.f` should return an object of class `recipe`")
  }

  # Keep recipe around
  tflow$pre$results$recipe <- rcp_data
  tflow$pre$mold <- hardhat::mold(rcp_data,
                                  molded_data,
                                  blueprint = blueprint)

  # All pre steps return the `tidyflow`
  tflow
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
