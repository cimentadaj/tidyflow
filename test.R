library(rsample)
library(recipes)
library(parsnip)
devtools::load_all()

res <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>% 
  plug_formula(mpg ~ .) %>% 
  plug_model(set_engine(linear_reg(), "lm"))

res

set.seed(23131)
res %>%
  fit()

mod <-
  mtcars %>%
  tidyflow() %>%
  plug_split(initial_split) %>%
  plug_recipe(~ recipe(mpg ~ cyl, data = .) %>% step_log(cyl, base = 10)) %>%
  plug_resample(vfold_cv) %>%
  plug_model(set_engine(linear_reg(), "lm"))

set.seed(23141)
res_vf <-
  mod %>%
  fit()

params <- pull_tflow_tuning(res_vf) %>% show_best(metric = "rsq")

res_vfold %>%
  select_best(metric = "rmse", maximize = FALSE)

mod %>%
  replace_recipe(~ {
    recipe(mpg ~ gear + cyl, data = .) %>%
      step_mutate(gear = as.factor(gear)) %>% 
      step_dummy(gear)
  }) %>%
  fit()

recipe_fun


complete_tflow <- function (x, parameters) {
    if (!inherits(x, "tidyflow")) {
        stop("`x` should be a tidyflow")
    }

    parsnip::check_final_param(parameters)
    mod <- tidyflow::pull_tidyflow_spec(x)
    mod <- finalize_model(mod, parameters)
    x$fit$actions$model$spec <- mod
    
    if (has_preprocessor_recipe(x)) {
        rec <- workflows::pull_workflow_preprocessor(x)
        rec <- finalize_recipe(rec, parameters)
        x <- set_workflow_recipe(x, rec)
    }
    x
}


## You left off here:
## Trying to figure out why finalize_model throws an error
## when providing the parameters

reprex::reprex({
  library(parsnip)
  library(tune)
  data("example_ames_knn")

  knn_model <-
    nearest_neighbor(
      mode = "regression",
      neighbors = tune("K"),
      weight_func = tune(),
      dist_power = tune()
    ) %>%
    set_engine("kknn")
  
  lowest_rmse <- select_best(ames_grid_search, metric = "rmse", maximize = FALSE)
  lowest_rmse
  
  knn_model
  finalize_model(knn_model, lowest_rmse)
})


pset <- parameters(x)
if (tibble::is_tibble(parameters)) {
  parameters <- as.list(parameters)
}

parameters <- parameters[names(parameters) %in% pset$id]
# You figure out it was here because filtering the variable which
# were given different tuning parameters raises a weird error. Once
# you figure it out, you wanted to make a PR to tune.
discordant <- dplyr::filter(pset, id != name & id %in% names(parameters))
if (nrow(discordant) > 0) {
  for (i in 1:nrow(discordant)) {
    names(parameters)[names(parameters) == discordant$id[i]] <- discordant$name[i]
  }
}
rlang::exec(update, object = x, !!!parameters)

## Once you finish that, all of this was done in order to adapt
## finalize_workflow to finalize_tidyflow to update the final model

## You will have two functions: one which updates the tidyflow with
## the final parameters and another which fits the last model.
## As a shortcut, you'll have one which does both things be default.
