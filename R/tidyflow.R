#' Create a tidyflow
#'
#' A `tidyflow` is a container object that aggregates information required to
#' fit and predict from a model. This information might be the main dataset,
#' specified through \code{plug_data()}, a recipe used in
#' preprocessing, specified through \code{plug_recipe()}, or the model specification
#' to fit, specified through \code{plug_model()}. However, it supports more
#' complex workflows, such as \code{plug_split()}, \code{plug_resample()},
#' \code{plug_grid()}.
#'
#' @param data A data frame or tibble used to begin the tidyflow. This is
#' optional as the data can be specified with [plug_data()]. 
#' 
#' @param seed A seed to be used for reproducibility across the complete
#' workflow. This seed is used for every step, to ensure the same result
#' when doing random splitting, resampling and model fitting.
#' 
#' @return
#' A new `tidyflow` object.
#'
#' @examples
#' library(recipes)
#' library(rsample)
#' library(dials)
#' library(parsnip)
#' library(tune)
#'
#' wflow <-
#'  mtcars %>%
#'  tidyflow(seed = 23113) %>%
#'  plug_recipe(~ recipe(mpg ~ cyl, .x) %>% step_log(cyl))
#'
#' # tidyflow gives a prinout of the current specification
#' # in the order of execution:
#' wflow
#'
#' # The minimum tidyflow is: data + recipe/formula + model
#' wflow <-
#'  wflow %>%
#'  plug_model(set_engine(linear_reg(), "lm"))
#'
#' # The output shows that we have the data, the recipe and the model
#' wflow
#'
#' # We can fit that model and we get a brief print out of the model:
#' fit(wflow)
#'
#' # We can add further steps and the print out will tell you the
#' # workflow specification:
#' wflow <-
#'  wflow %>%
#'  plug_split(initial_split) %>%
#'  plug_resample(vfold_cv, v = 2) %>%
#'  plug_grid(grid_regular)
#'
#' # The print out shows that we have a split/resample/grid
#' # now set correcly.
#' wflow
#'
#' @export
tidyflow <- function(data = NULL, seed = NULL) {
  if (!is.null(data) && !is.data.frame(data)) {
    abort("A tidyflow can only begin with a data frame; `data` must a data frame") #nolintr
  }

  new_tidyflow(data = data,
               pre = new_stage_pre(mold = data, seed = seed))
}

# ------------------------------------------------------------------------------

new_tidyflow <- function(data = NULL,
                         pre = new_stage_pre(),
                         fit = new_stage_fit(),
                         post = new_stage_post(),
                         trained = FALSE) {
  if (!is_stage(pre)) {
    abort("`pre` must be a `stage`.")
  }

  if (!is_stage(fit)) {
    abort("`fit` must be a `stage`.")
  }

  if (!is_stage(post)) {
    abort("`post` must be a `stage`.")
  }

  if (!is_scalar_logical(trained)) {
    abort("`trained` must be a single logical value.")
  }

  data <- list(
    data = data,
    pre = pre,
    fit = fit,
    post = post,
    trained = trained
  )

  structure(data, class = "tidyflow")
}

is_tidyflow <- function(x) {
  inherits(x, "tidyflow")
}

# ------------------------------------------------------------------------------

#' @export
print.tidyflow <- function(x, ...) {
  print_header(x)
  invisible(x)
}

print_header <- function(x) {

  # Print header
  if (x$trained) {
    trained <- " [trained]"
  } else {
    trained <- if (has_fit_tuning(x)) " [tuned]" else ""
  }

  header <- glue::glue("Tidyflow{trained}")
  header <- cli::rule(header, line = 2)

  cat_line(header)
  # End print header

  print_data(x)
  print_split(x)
  print_formula_recipe(x)
  print_resample(x)
  print_grid(x)
  print_spec(x)

  if ((has_fit(x)) | has_fit_tuning(x)) {
    cat_line(cli::rule("Results", line = 2))
    print_model_resample(x)
  }

  invisible(x)
}

print_data <- function(x) {
  data_msg <- cli::style_italic("Data:")

  if (!has_raw_data(x)) {
    data_msg <- glue::glue("{data_msg} None")
  } else {
    dt <- pull_tflow_rawdata(x)
    data_missing <- round(sum(is.na(dt)) / nrow(dt), 1)
    # I include a `is.nan` in case the data frame
    # is empty and 0 / 0 equals NaN
    data_content <- glue::glue(
      "{number_formatter(nrow(dt))} rows x {number_formatter(ncol(dt))} columns"
    )

    data_msg <- glue::glue("{data_msg} {data_content}")
  }

  cat_line(data_msg)
}

print_split <- function(x) {
  split_msg <- cli::style_italic("Split:")

  if (!has_preprocessor_split(x)) {
    split_msg <- glue::glue("{split_msg} None")
  } else {
    split_fn_name <- names(x$pre$actions$split)[1]
    arg <- unlist(x$pre$actions$split$args)

    if (rlang::is_empty(arg)) {
      arg_msg <- "default args"
    } else {
      arg_msg <- paste0(names(arg), " = ", arg, collapse = ", ")
    }

    split_msg <- glue::glue("{split_msg} {split_fn_name} w/ {arg_msg}")
  }

  cat_line(split_msg)
}

print_formula_recipe <- function(x) {
  if (!(has_preprocessor_formula(x) || has_preprocessor_recipe(x))) {
    preprocessor_msg <- cli::style_italic("Recipe/Formula: None")
    cat_line(preprocessor_msg)
  } else {
    if (has_preprocessor_formula(x)) {
      print_formula(x)
    } else if (has_preprocessor_recipe(x)) {
      print_recipe(x)
    }
  }
}

print_formula <- function(x) {
  formula_msg <- cli::style_italic("Formula: ")
  formula <- pull_tflow_preprocessor(x)
  formula <- rlang::expr_text(formula)
  cat_line(glue::glue(formula_msg, formula))
}

print_recipe <- function(x) {
  cat_line("Recipe: available")
}

print_resample <- function(x) {
  resample_msg <- cli::style_italic("Resample:")

  if (!has_preprocessor_resample(x)) {
    resample_msg <- glue::glue("{resample_msg} None")
  } else {
    resample_fn_name <- names(x$pre$actions$resample)[1]
    arg <- unlist(x$pre$actions$resample$args)

    if (rlang::is_empty(arg)) {
      arg_msg <- "default args"
    } else {
      arg_msg <- paste0(names(arg), " = ", arg, collapse = ", ")
    }

    resample_msg <- glue::glue("{resample_msg} {resample_fn_name} w/ {arg_msg}")
  }

  cat_line(resample_msg)
}

print_grid <- function(x) {
  grid_msg <- cli::style_italic("Grid:")

  if (!has_preprocessor_grid(x)) {
    grid_msg <- glue::glue("{grid_msg} None")
  } else {
    grid_fn_name <- names(x$pre$actions$grid)[1]
    arg <- unlist(x$pre$actions$grid$args)

    if (rlang::is_empty(arg)) {
      # TODO: add tuning values from model/recipe if available
      arg_msg <- "default args"
    } else {
      arg_named <- names(arg) != ""
      if (any(arg_named)) {
        named_args <- paste0(names(arg[arg_named]), " = ", arg[arg_named], collapse = ", ")
        # If no unnamed args and any named arg, then the result will be
        # ", levels = 5" for example. Here convert empty "" to character()
        # so that when no unnamed arg, no extra comma is added.
        unnamed_args <- paste0(arg[!arg_named], collapse = ", ")
        unnamed_args <- if (unnamed_args == "") character() else unnamed_args
        arg_msg <- paste0(c(unnamed_args, named_args), collapse = ", ")
      } else {
        arg_msg <- paste0(arg, collapse = ", ")
      }
    }

    grid_msg <- glue::glue("{grid_msg} {grid_fn_name} w/ {arg_msg}")
  }

  cat_line(grid_msg)
} 

print_spec <- function(x) {
  if (has_spec(x)) {
    spec <- pull_tflow_spec(x)

    cat_line(cli::style_italic("Model:"))
    print(spec)
  } else {
    cat_line(cli::style_italic("Model: None"))
  }

  invisible(x)
}

print_model_resample <- function(x) {
  has_fit <- has_fit(x)
  has_tuning <- has_fit_tuning(x)

  # Space between Tidyflow/Preprocessor section and Model section
  cat_line("")
  if (has_tuning) print_tuning(x)
  cat_line("")
  if (has_fit) print_fit(x)
  invisible(x)
}

print_fit <- function(x) {
  parsnip_fit <- pull_tflow_fit(x)
  fit <- parsnip_fit$fit

  output <- utils::capture.output(fit)
  n_output <- length(output)

  if (n_output < 6L) print(fit)

  n_extra_output <- n_output - 5L
  output <- output[1:5]

  extra_output_msg <- glue::glue("and {n_extra_output} more lines.")

  cat_line("Fitted model:")
  cat_line(output)
  cat_line("")
  cat_line("...")
  cat_line(extra_output_msg)

  invisible(x)
}

print_tuning <- function(x) {
  tuning_fit <- pull_tflow_fit_tuning(x)

  n_output <- nrow(tuning_fit)

  cat_line("Tuning results: ")
  cat_line("")

  if (n_output < 6L) {
    print(tuning_fit)
    return(invisible(x))
  }

  n_extra_output <- n_output - 5L
  output <- tuning_fit[1:5, ]

  print(output)
  cat_line("")
  cat_line(glue::glue("... and {n_extra_output} more lines."))
  cat_line("")

  invisible(x)
}


cat_line <- function(...) {
  cat(paste0(..., collapse = "\n"), "\n", sep = "")
}

number_formatter <- function(tx) { 
  div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                      c(0, 1e3, 1e6, 1e9, 1e12) )
  intm <- round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2)
  paste0(intm, c("","K","M","B","T")[div] )
}
