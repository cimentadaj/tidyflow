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
mod %>%
  fit()



mod %>%
  replace_recipe(~ {
    recipe(mpg ~ gear + cyl, data = .) %>%
      step_mutate(gear = as.factor(gear)) %>% 
      step_dummy(gear)
  }) %>%
  fit()

recipe_fun
