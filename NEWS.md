# tidyflow (development version)

## Breaking changes
* Minimum R version requirement increased from 3.2 to 4.1 to align with
  tidymodels ecosystem requirements.

* Updated minimum version requirements for tidymodels dependencies:

  - workflows: 0.1.3 → 1.0.0
  - parsnip: 0.1.3 → 1.0.0
  - tune: 0.1.0 → 1.0.0
  - dials: 0.0.6 → 1.0.0
  - hardhat: 0.1.2 → 1.0.0
  - generics: 0.0.2 → 0.1.0

## New features

* Added tidymodels-consistent `extract_*` methods for tidyflow objects (#api):
  - `extract_fit_parsnip()`: Extract the fitted parsnip model
  - `extract_spec_parsnip()`: Extract the model specification
  - `extract_preprocessor()`: Extract the preprocessor (formula/recipe)
  - `extract_mold()`: Extract the mold from the internal workflow
  - `extract_recipe()`: Extract the prepped recipe
  - `extract_parameter_set_dials()`: Extract tuning parameters

  These complement the existing `pull_tflow_*` functions and follow
  tidymodels naming conventions.

* Added `plug_tune()` function for specifying alternative tuning methods (#tuning):
  - Supports `tune_bayes` for Bayesian optimization
  - Supports `tune_sim_anneal` for simulated annealing
  - Supports `tune_race_anova` and `tune_race_win_loss` for racing methods
  - Default remains `tune_grid` for exhaustive grid search

* Added `control_bayes` parameter to `control_tidyflow()` for controlling
  Bayesian optimization settings.

## Improvements

* Improved error messages throughout the package using the `cli` package (#ux):
  - Error messages now include contextual hints with suggested fixes
  - Function names and arguments are properly formatted
  - Clearer indication of what went wrong and how to fix it

* Improved documentation for `pull_tflow_*` functions to reference the new
  `extract_*` methods.

## Internal changes

* Replaced deprecated `workflows::pull_workflow_fit()` with
  `workflows::extract_fit_parsnip()`.

* Replaced deprecated `workflows::pull_workflow_mold()` with
  `workflows::extract_mold()` in tests.

* Replaced deprecated `tune::parameters()` with
  `hardhat::extract_parameter_set_dials()` for extracting tuning parameters
  from model specifications and recipes.

These changes ensure compatibility with the latest tidymodels ecosystem
(workflows, tune, parsnip, dials >= 1.0.0) which deprecated the
`pull_workflow_*` and `parameters()` families of functions in favor
of the new `extract_*` functions.
