#' Fit a tidyflow object
#'
#' @param x A tidyflow object
#' @param control A \code{\link{control_tidyflow}} object with the options
#' for the model. See \code{\link{control_tidyflow}} for all options and
#' examples.
#' @param ... Not used right now
#'
#' @return
#' `tidyflow` can return two type of results: a model object or a resamples.
#' Depending on the specification of the \code{tidyflow} you can get either.
#'
#' Here are all possible results:
#' \itemize{
#'   \item Any \code{tidyflow} **without** a \code{\link{plug_resample}} or
#'   \code{\link{plug_grid}} specificiation, will always return a model. Since
#'   there is no grid search or resampling happening, `fit` will always return
#'   a tidyflow with a model. If needed, this model can be extracted with
#'   \code{\link{pull_tflow_fit}}.
#'
#'   \item Any \code{tidyflow} **with** either a \code{\link{plug_resample}} or
#'   a combination of \code{\link{plug_resample}} and \code{\link{plug_grid}}
#'   will always return a \code{tidyflow} with a resample object. The resample
#'   object can be extracted with \code{\link{pull_tflow_fit_tuning}}. If a
#'   tuning grid is supplied with \code{\link{plug_grid}}, one can finalize
#'   the best model with \code{\link{complete_tflow}}. See
#'   \code{\link{complete_tflow}} for examples on how to use it.
#' }
#'
#' @details
#' Fitting a tidyflow currently involves several steps:
#'
#' - Check that there is at least a minimum tidyflow: data, the
#'   formula/recipe and the model
#'
#' - Execute each step in the \code{tidyflow} in this order:
#' \itemize{
#'    \item Apply the split passed by \code{\link[rsample]{initial_split}} and
#'    extract only the training data.
#' 
#'    \item Apply the formula or the recipe to the training data. Whenever the
#'    user specifies a resample or a grid, the recipe is not applied. Instead,
#'    the recipe is passed to either \code{\link[tune]{fit_resamples}} or
#'    \code{\link[tune]{tune_grid}} and let these functions apply it.
#' 
#'    \item Apply the resample function to the training data. As described in
#'    the item above, the recipe is not applied to the training data previous
#'    to this step.
#'
#'    \item Apply the grid function to the parameters defined in the
#'    \code{parsnip} model and the \code{recipe}. Alternatively, extract
#'    the arguments defined in \code{\link{plug_grid}}. This generates a
#'    grid of values to explore.
#'
#'    \item Run the model/grid search/resample depending on the specification.
#' }
#'
#' - This is the sacred order used for execution in \code{tidyflow}. One can
#'   specify some of these steps and exclude others. \code{tidyflow} will
#'   generate errors accordingly if something is missing or needed. For example,
#'   one can create the combination of data + formula + resample + model and then
#'   fit. In a follow up, one can add a grid and the execution order will always
#'   be the one described above skipping any steps that are not specified.
#' 
#'
#' @name fit-tidyflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(tune)
#' library(dials)
#' library(rsample)
#'
#' # Fit a simple linear model
#' model <- set_engine(linear_reg(), "lm")
#' 
#' formula_tidyflow <-
#'  mtcars %>%
#'  tidyflow() %>%
#'  plug_formula(mpg ~ cyl + log(disp)) %>%
#'  plug_model(model)
#'
#' # The result is a model since we didn't specify any resample/grid
#' res <- fit(formula_tidyflow)
#'
#' # You can extract the model fit if neede
#' res %>%
#'  pull_tflow_fit()
#'
#' # Alternatively, we can add a split specification and
#' # predict on the training data automatically:
#' formula_tidyflow <-
#'  formula_tidyflow %>%
#'  plug_split(initial_split)
#'
#' res2 <- fit(formula_tidyflow)
#'
#' res2 %>%
#'  predict_training()
#'
#' # This has the advantage that `predict_training` or `predict_testing` will
#' # apply the recipe/formula automatically for you:
#'
#' recipe_tidyflow <-
#'  formula_tidyflow %>%
#'  drop_formula() %>% 
#'  plug_recipe(~ recipe(mpg ~ ., .x) %>% step_log(disp))
#'
#' res3 <- fit(recipe_tidyflow)
#' res3 %>%
#'  predict_testing()
#'
#' # We can accumulate steps and add a cross-validation and tuning grid.
#' # Fit a regularized regression through a grid search.
#' # Do this by updating the already defined model:
#' new_mod <- set_engine(linear_reg(penalty = tune(), mixture = tune()),
#'                       "glmnet")
#' tuned_res <-
#'   recipe_tidyflow %>%
#'   plug_resample(vfold_cv, v = 2) %>% 
#'   replace_model(new_mod) %>%
#'   plug_grid(grid_regular, levels = 2) %>%
#'   fit()
#'
#' # Since we specified a resample/grid, the result is now a `tidyflow`
#' # with a resample object
#' tuned_res
#'
#' # If needed, we can extract that resample:
#' tuned_res %>%
#'  pull_tflow_fit_tuning() %>%
#'  autoplot()
#'
#' # When the model tuning is finished, `complete_tflow` can
#' # finalize the model with the best model. It can pick
#' # the best model for you.
#'
#' tuned_res %>%
#'  complete_tflow(metric = "rmse") %>%
#'  predict_training()
#'
#' # `complete_tflow` is powerful as it already applied the recipe
#' # and retrained the model on the entire training data with
#' # the best tuning parameter from the tuning grid.
#'
#' # The power of this model building is that you can replace any step
#' # and rerun the fit:
#' bootstrap_res <-
#'  tuned_res %>%
#'  replace_resample(bootstraps, times = 2) %>%
#'  fit()
#'
#' bootstrap_res %>%
#'  complete_tflow(metric = "rsq") %>%
#'  predict_training()
#' 
fit.tidyflow <- function(x, control = control_tidyflow(), ...) {

  if (!has_raw_data(x)) {
    abort("`data` must be specified to fit a tidyflow; Do you need `plug_data`?")
  }

  ellipsis::check_dots_empty()
  validate_has_minimal_components(x)

  x <- .fit_pre(x)
  x <- .fit_model(x, control)

  # Eh? Predictions during the fit?
  # pred <- result$pred
  # result <- fit_post(x, pred)

  # Only if it has be fit (NOT TUNED!)
  x$trained <- if (has_fit(x)) TRUE else FALSE

  x
}

# ------------------------------------------------------------------------------

#' Internal tidyflow functions
#'
#' `.fit_pre()` and `.fit_model()` are internal tidyflow functions for
#' _partially_ fitting a tidyflow object. They are only exported for usage by
#' the tuning package, [tune](https://github.com/tidymodels/tune), and the
#' general user should never need to worry about them.
#'
#' @param tidyflow A tidyflow
#'
#'   For `.fit_pre()`, this should be a fresh tidyflow.
#'
#'   For `.fit_model()`, this should be a tidyflow that has already been trained
#'   through `.fit_pre()`.
#'
#' @param data A data frame of predictors and outcomes to use when fitting the
#'   tidyflow
#'
#' @param control A [control_tidyflow()] object
#'
#' @name tidyflows-internals
#' @keywords internal
.fit_pre <- function(x) {
  n <- length(x[["pre"]]$actions)

  for (i in seq_len(n)) {
    # If no seed has been specified, `set.seed` supports NULL as random
    set.seed(x$pre$seed)
    action <- x[["pre"]]$actions[[i]]
    # Update the `x` as we iterate through pre steps
    x <- fit(action, x)
  }

  # But only return the x, it contains the final set of data in `mold`
  x
}

#' @rdname tidyflows-internals
.fit_model <- function(x, control) {
  # If no seed has been specified, `set.seed` supports NULL as random
  set.seed(x$pre$seed)

  object <- x[["fit"]][["actions"]][["model"]]
  fit(object, x = x, control = control)
}

# ------------------------------------------------------------------------------

validate_has_minimal_components <- function(x) {
  has_preprocessor <- has_action(x$pre, "formula") || has_action(x$pre, "recipe")

  if (!has_preprocessor) {
    glubort(
      "The tidyflow must have a formula or recipe preprocessor. ",
      "Provide one with `plug_formula()` or `plug_recipe()`."
    )
  }

  has_model <- has_action(x$fit, "model")

  if (!has_model) {
    glubort(
      "The tidyflow must have a model. ",
      "Provide one with `plug_model()`."
    )
  }

  invisible(x)
}
