#' Create a tidyflow
#'
#' A `tidyflow` is a container object that aggregates information required to
#' fit and predict from a model. This information might be the main dataset,
#' specified through [plug_data()], a recipe used in
#' preprocessing, specified through [plug_recipe()], or the model specification
#' to fit, specified through [plug_model()].
#'
#' @param data A data frame or tibble used to begin the tidyflow. This is
#' optional as the data can be specified with [plug_data()]. 
#' 
#' @param seed A seed to be used for reproducibility across the complete
#' workflow. This seed is used for every step, to ensure the same result
#' when doing random splitting, resampling and model fitting.
#' @return
#' A new `tidyflow` object.
#'
#' @examples
#' library(recipes)
#'
#' rec <- ~ recipe(mpg ~ cyl, .x) %>% step_log(cyl)
#' wrk <- tidyflow(mtcars)
#' wrk <- plug_recipe(wrk, rec)
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
  print_preprocessor(x)
  print_model(x)
  # cat_line("")
  # print_postprocessor(x)
  invisible(x)
}

print_header <- function(x) {
  if (x$trained) {
    trained <- " [trained]"
  } else {
    trained <- if (has_fit_tuning(x)) " [tuned]" else ""
  }

  header <- glue::glue("Tidyflow{trained}")
  header <- cli::rule(header, line = 2)

  cat_line(header)

  data_msg <- cli::style_italic("Data:")

  if (!has_raw_data(x)) {
    data_msg <- glue::glue("{data_msg} None")
  } else {
    dt <- pull_tidyflow_rawdata(x)
    data_missing <- sum(is.na(dt)) / nrow(dt)
    # I include a `is.nan` in case the data frame
    # is empty and 0 / 0 equals NaN
    data_content <- glue::glue(
      "{nrow(dt)} rows x {ncol(dt)} columns; ",
      "{if (is.nan(data_missing)) 0 else data_missing}% missing values"
    )

    data_msg <- glue::glue("{data_msg} {data_content}")
  }

  cat_line(data_msg)

  preprocessor_msg <- cli::style_italic("Preprocessor:")

  if (has_preprocessor_formula(x)) {
    preprocessor <- "Formula"
  } else if (has_preprocessor_recipe(x)) {
    preprocessor <- "Recipe"
  } else {
    preprocessor <- "None"
  }

  preprocessor_msg <- glue::glue("{preprocessor_msg} {preprocessor}")
  cat_line(preprocessor_msg)

  spec_msg <- cli::style_italic("Model:")

  if (has_spec(x)) {
    spec <- class(pull_tidyflow_spec(x))[[1]]
    spec <- glue::glue("{spec}()")
  } else {
    spec <- "None"
  }

  spec_msg <- glue::glue("{spec_msg} {spec}")
  cat_line(spec_msg)

  invisible(x)
}

print_preprocessor <- function(x) {
  has_preprocessor_formula <- has_preprocessor_formula(x)
  has_preprocessor_recipe <- has_preprocessor_recipe(x)
  has_preprocessor_split <- has_preprocessor_split(x)
  has_preprocessor_resample <- has_preprocessor_resample(x)

  preprocessing <- c(
    has_preprocessor_formula,
    has_preprocessor_recipe,
    has_preprocessor_split,
    has_preprocessor_resample
  )

  # If all have **no** preprocessing, don't print anything
  if (all(!preprocessing)) {
    return(invisible(x))
  }

  # Space between Tidyflow section and Data section
  cat_line("")

  header <- cli::rule("Preprocessor")
  cat_line(header)

  if (has_preprocessor_split) {
    print_preprocessor_split(x)
  }

  if (has_preprocessor_resample) {
    print_preprocessor_resample(x)
  }

  if (has_preprocessor_formula) {
    print_preprocessor_formula(x)
  }

  ## if (has_preprocessor_recipe) {
  ##   print_preprocessor_recipe(x)
  ## }

  invisible(x)
}

print_preprocessor_split <- function(x) {
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

print_preprocessor_resample <- function(x) {
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

print_preprocessor_formula <- function(x) {
  formula_msg <- cli::style_italic("Formula: ")
  formula <- pull_tidyflow_preprocessor(x)
  formula <- rlang::expr_text(formula)

  cat_line(glue::glue(formula_msg, formula))

  invisible(x)
}

print_preprocessor_recipe <- function(x) {
  recipe <- pull_tidyflow_preprocessor(x)
  steps <- recipe$steps
  n_steps <- length(steps)

  if (n_steps == 1L) {
    step <- "Step"
  } else {
    step <- "Steps"
  }

  n_steps_msg <- glue::glue("{n_steps} Recipe {step}")
  cat_line(n_steps_msg)

  if (n_steps == 0L) {
    return(invisible(x))
  }

  cat_line("")

  step_names <- map_chr(steps, pull_step_name)

  if (n_steps <= 10L) {
    cli::cat_bullet(step_names)
    return(invisible(x))
  }

  extra_steps <- n_steps - 10L
  step_names <- step_names[1:10]

  if (extra_steps == 1L) {
    step <- "step"
  } else {
    step <- "steps"
  }

  extra_dots <- "..."
  extra_msg <- glue::glue("and {extra_steps} more {step}.")

  step_names <- c(step_names, extra_dots, extra_msg)

  cli::cat_bullet(step_names)
  invisible(x)
}

pull_step_name <- function(x) {
  step <- class(x)[[1]]
  glue::glue("{step}()")
}

print_model <- function(x) {
  has_spec <- has_spec(x)

  if (!has_spec) {
    return(invisible(x))
  }

  has_fit <- has_fit(x)
  has_tuning <- has_fit_tuning(x)

  # Space between Tidyflow/Preprocessor section and Model section
  cat_line("")

  header <- cli::rule("Model")
  cat_line(header)

  print_spec(x)

  if (has_tuning) {
    print_tuning(x)
    invisible(x)
  }

  if (has_fit) {
    print_fit(x)
    return(invisible(x))
  }

  invisible(x)
}

print_spec <- function(x) {
  spec <- pull_tidyflow_spec(x)

  print(spec)

  invisible(x)
}

print_fit <- function(x) {
  parsnip_fit <- pull_tflow_fit(x)
  fit <- parsnip_fit$fit

  output <- utils::capture.output(fit)
  n_output <- length(output)

  if (n_output < 50L) {
    print(fit)
    return(invisible(x))
  }

  n_extra_output <- n_output - 50L
  output <- output[1:50]

  extra_output_msg <- glue::glue("and {n_extra_output} more lines.")

  cat_line(output)
  cat_line("")
  cat_line("...")
  cat_line(extra_output_msg)

  invisible(x)
}

print_tuning <- function(x) {
  tuning_fit <- pull_tflow_tuning(x)

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
