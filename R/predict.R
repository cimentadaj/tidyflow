#' Predict from a tidyflow
#'
#' @description
#' `predict()` method for a fitted tidyflow object. This method
#' can be applied to new data and if a recipe is defined in the
#' \code{tidyflow}, the steps are applied to the new data.
#' Alternatively, when a split is specified, \code{predict_training} and
#' \code{predict_testing} automatically predict and apply any preprocessing to
#' the training and testing data.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @param x A tidyflow that has been fitted by [fit.tidyflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on. Usually, this would be extracted from the tidyflow
#'   with \code{\link{pull_tflow_testing}} or
#'   \code{\link{pull_tflow_training}}. Note that \code{predict.tidyflow}
#'   already applies the recipe or formula automatically. It is not advised to
#'   preprocess the \code{newdata} before passing it to \code{predict.tidyflow}.
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has.
#'
#' @name predict-tidyflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(rsample)
#' library(tune)
#'
#' model <- set_engine(linear_reg(), "lm")
#'
#' tflow <-
#'  mtcars %>%
#'  tidyflow() %>%
#'  plug_split(initial_split) %>%
#'  plug_model(model) %>%
#'  plug_recipe(~ recipe(mpg ~ cyl + disp, .) %>% step_log(disp))
#'
#' tflow <- fit(tflow)
#'
#' # This will automatically `bake()` the recipe on `new_data`,
#' # applying the log step to `disp`, and then fit the regression.
#' predict(tflow, new_data = pull_tflow_testing(tflow))
#'
#' # When a split has been specified through `plug_split`,
#' # predict_training/predict_testing automatically extract
#' # everything and applies the recip/formula:
#' predict_testing(tflow)
#' predict_training(tflow)
#'
#' # When a grid search has been performed, the user needs to
#' # finalize the model through complete_tflow and then
#' # predict/predict_training/predict_testing will work.
#' res <-
#'  tflow %>%
#'  # Adds a grid search for the polynomials of qsec
#'  replace_recipe(~ recipe(mpg ~ ., data = .) %>% step_ns(hp, deg_free = tune())) %>%
#'  plug_resample(vfold_cv, v = 2) %>% 
#'  plug_grid(grid_regular) %>%
#'  fit()
#'
#' # We can complete the tidyflow by fitting the best model
#' # based on the RMSE metric and then predict:
#' res %>%
#'  complete_tflow(metric = "rmse") %>%
#'  predict_training()
#'
#' # In short, to be able to predict, you need to have either a single model
#' # or a finalized tuning grid with `complete_tflow`.
#' 
predict.tidyflow <- function(x, new_data, type = NULL, opts = list(), ...) {
  tuning <- try(pull_tflow_fit_tuning(x), silent = TRUE)

  # If there's a tuning object but no final model
  if (!inherits(tuning, "try-error") && !x$trained) {
    abort("You seem to have a model with tuning parameters or a resample but not a finalized model. Did you call complete_tflow()?")
  }

  # If no tuning object is present but the model is not trained
  if (inherits(tuning, "try-error") && !x$trained) {
    abort("Tidyflow has not yet been trained. Did you call fit()?")
  }

  blueprint <- pull_tflow_mold(x)$blueprint
  forged <- hardhat::forge(new_data, blueprint)
  new_data <- forged$predictors
  fit <- pull_tflow_fit(x)
  predict(fit, new_data, type = type, opts = opts, ...)
}

#' @rdname predict-tidyflow
#' @export
predict_training <- function(object, type = NULL, opts = list(), ...) {
  if (!has_preprocessor_split(object)) {
    rlang::abort("`predict_training` can only work when a split preprocessor has been specifid. Did you want `plug_split`?")
  }

  # No need for prep = TRUE, since predict.tidyflow already applies the bake
  tr_dt <- pull_tflow_training(object)
  res <-
    tibble::as_tibble(
      cbind(
        tr_dt,
        predict(object, new_data = tr_dt, type = type, opts = opts, ...)
      )
    )

  res
}

#' @rdname predict-tidyflow
#' @export
predict_testing <- function(object, type = NULL, opts = list(), ...) {
  if (!has_preprocessor_split(object)) {
    rlang::abort("`predict_testing` can only work when a split preprocessor has been specifid. Did you want `plug_split`?")
  }

  # No need for prep = TRUE, since predict.tidyflow already applies the bake
  tst_dt <- pull_tflow_testing(object)

  res <-
    tibble::as_tibble(
      cbind(
        tst_dt,
        predict(object, new_data = tst_dt, type = type, opts = opts, ...)
      )
    )

  res
}
