#' Add a grid specification to a tidyflow
#'
#' @description
#' - `plug_grid()` specifies the type of grid used in the model tuning. It
#'   accepts a function \code{.f} that will be fed the tuning parameters defined
#'   in the model and the recipe. Only functions which return a \code{param_grid}
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
#' The grid specification is an optional step in the tidyflow. You can add
#' the data, prepare a recipe and fit the model without adding a grid
#' specification.
#'
#' The tuning parameters defined in the model and recipe are extracted
#' and passed to \code{.f}. If \code{expand = TRUE}, the result of \code{.f}
#' should return be an object of class \code{param_grid}. The functions used
#' to generate the grid values should come from the \code{\link[dials]{dials}}
#' package. However, if \code{expand = FALSE}, only `expand.grid` is supported in
#' \code{.f} and each parameter should not be a parameter object (for example
#' \code{\link[dials]{mixture}}). Rather, they should be the raw values used
#' to expand, such as \code{mixture = c(0, 0.5, 1)}. For more details see
#' the description of \code{expand} argument and the example section.
#'
#' If a tuning parameter in the model/recipe is assigned a name (that is,
#' \code{tune("new_name")}) and the user is interested in specifying
#' the tuning values for that parameter using \code{plug_grid} or
#' \code{replace_grid}, then the parameter name should have the name not
#' the original name. See the example section for a concrete example.
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
#' See the example section.
#'
#' @param expand A logical stating whether to treat `.f` as an expanding
#' function. This behavior changes how `plug_grid` works in important ways.
#' In particular, `.f` should only be \code{`expand.grid`} and any arguments in
#' `...` should be all the tuning parameters defined in the tidyflow to be
#' expanded. This does not support parameter objects like
#' \code{\link[dials]{mixture}} but rather the raw values to be expanded
#' by \code{expand.grid}. For example, instead of \code{penalty = dials::mixture()}
#' it should be \code{mixture = c(0, 0.5, 1)}. See the example section.
#'
#' @return
#' `x`, updated with either a new or removed grid specification.
#'
#' @export
#' @examples
#'
#' \dontrun{
#' library(parsnip)
#' library(rsample)
#' library(tune)
#' library(dials)
#' library(recipes)
#' # Grid search with tuning parameters in model
#' # No need to define the values of the tuning parameters
#' # as they have defaults. For example, see dials::penalty()
#' mod <-
#'   mtcars %>%
#'   tidyflow() %>%
#'   plug_split(initial_split) %>%
#'   plug_formula(mpg ~ .) %>% 
#'   plug_resample(vfold_cv) %>%
#'   plug_model(set_engine(linear_reg(penalty = tune(), mixture = tune()), "glmnet")) %>% 
#'   plug_grid(grid_regular)
#' 
#' res <- fit(mod)
#' 
#' res %>%
#'   pull_tflow_fit_tuning() %>%
#'   show_best("rsq")
#' 
#' # If you want to specify tuning values, you can do so with
#' # `plug_grid` or `replace_grid` but they must have the same
#' # name as the tuning parameter
#' res2 <-
#'   mod %>%
#'   replace_grid(grid_regular, penalty = penalty(c(-1, 0)), levels = 2) %>%
#'   fit()
#' 
#' res2 %>%
#'   pull_tflow_fit_tuning() %>%
#'   show_best("rsq")
#' 
#' # If tune assigns a name, then `plug_grid` or `replace_grid` must
#' # use that name to replace it
#' model <-
#'   set_engine(
#'     linear_reg(penalty = tune("my_penalty"), mixture = tune("my_mixture")),
#'     "glmnet"
#'   )
#' 
#' # You must use `my_penalty`
#' res3 <-
#'   mod %>%
#'   replace_model(model) %>%   
#'   replace_grid(grid_regular, my_penalty = penalty(c(-1, 0)), levels = 2) %>%
#'   fit()
#' 
#' res3 %>%
#'   pull_tflow_fit_tuning() %>%
#'   show_best("rsq")
#'
#' # If you want to create all possible combination of grid values,
#' # you must use only `expand.grid` and `expand = TRUE`.
#' res4 <-
#'  mod %>%
#'  replace_grid(expand.grid,
#'               penalty = seq(0.01, 0.02, 0.005),
#'               mixture = c(0, 0.5, 1),
#'               expand = TRUE) %>%
#'  fit()
#'
#' # See how they values are not random, but rather
#' # all combination of the supplied values
#' res4 %>%
#'  pull_tflow_fit_tuning() %>%
#'  collect_metrics()
#' 
#' # You can also tune values from a recipe directly
#' res5 <-
#'   res3 %>%
#'   drop_formula() %>% 
#'   plug_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
#'   fit()
#' 
#' res5 %>%
#'   pull_tflow_fit_tuning() %>%
#'   show_best("rsq")
#' }
#' 
plug_grid <- function(x, .f, ..., expand = FALSE) {
  .dots <- c(as.list(enquos(...)), expand = expand)

  if (!is_uniquely_named(.dots)) {
    fun_name <- as.character(match.call())[1]
    abort(
      paste0("Arguments in `...` for `", fun_name, "` should be uniquely named")
    )
  }

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
replace_grid <- function(x, .f, ..., expand = FALSE) {
  x <- drop_grid(x)
  .f <- enquo(.f)
  plug_grid(x, !!.f, ..., expand = expand)
}

# ------------------------------------------------------------------------------
fit.action_grid <- function(object, tflow) {

  if (!has_preprocessor_resample(tflow)) {
    abort("The tidyflow has a grid specification but no resample specification. Did you want `plug_resample()`?") #nolintr
  }

  if (!has_spec(tflow)) {
    abort("The tidyflow does not have a model specification to extract the tuning parameters. Did you want `plug_model()`?") #nolintr
  }

  if (!has_tune(tflow)) {
    abort("The tidyflow has a grid specification but no tuning placeholders. Did you mean to specify `tune()` in your model or recipe?") #nolintr
  }

  # Including recipe + model
  all_params <- tune::parameters(tflow)

  # For some reason, running set seed before tune::parameters
  # does not return the same random grid as running the
  # grid out side. I tested this a few times and putting the
  # seed here makes sure that the same grid is always returned
  # consistent with grids OUTSIDE tflow.
  set.seed(tflow$pre$seed)

  ## object[[2]] are the arguments as quosures
  args <- lapply(object[[2]], eval_tidy)

  # When specifying parameters in `...` that need to override
  # the ones defined in `tune()` inside the model, I only
  # keep the names of parameters defined in `...` not
  # and update the all_params
  name_args <- names(formals(object[[1]]))
  parameters_names <- setdiff(names(args), c(name_args, "expand"))

  if (args$expand) {

    if (names(object)[1] != "expand.grid") {
      rlang::abort("When `expand = TRUE`, `plug_grid` only accepts the function `expand.grid` in `.f` for expanding the arguments") #nolintr
    }

    ind_exp <- which(names(args) == "expand")
    args <- args[-ind_exp]

    param_specific <- vapply(args,
                             inherits,
                             "param",
                             FUN.VALUE = logical(1)
                             )

    if (any(param_specific)) {
      rlang::abort("When `expand = TRUE`, `plug_grid` arguments should not be parameter objects such as deg_free() or mixture(). They should be vectors to be expanded such as deg_free = 1:10 or mixture = 0:1") #nolintr
    }

    # Focing all params to have an NA in args2 is a hack
    # for update to try to update the specified params
    # into all_params. This way, at least if the user
    # specified the name of parameters wrong or is missing
    # particular parameters, update takes care of raising the error.
    # Once that's checked, I just run the function specified
    # with the specified parameters.
    args2 <- args
    args2 <- lapply(args2, function(x) NA)

    rlang::exec(
      stats::update,
      all_params,
      !!!args2[parameters_names]
    )

    grid_res <- rlang::exec(
      # function body
      object[[1]],
      !!!args
    )

    if (!inherits(grid_res, "data.frame")) {
      abort("The grid function should return an object of class `data.frame` if expand is `TRUE`.") #nolintr
    }
    
  } else {

    if (names(object)[1] == "expand.grid") {
      rlang::abort("`expand.grid` is only permitted when `expand = TRUE`. Did you want `expand = TRUE`?")
    }

    if (length(parameters_names) != 0) {
      all_params <- rlang::exec(
        stats::update,
        all_params,
        !!!args[parameters_names]
      )
    }

    grid_res <- rlang::exec(
      # function body
      object[[1]],
      all_params,
      !!!args
    )

    if (!inherits(grid_res, "param_grid")) {
      abort("The grid function should return an object of class `param_grid`.")
    }

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
# add support for expand_grid, such that you can combine vectors and param objects
# for example tidyr::expand_grid(deg_free = 1:10, grid_regular(penalty()))
