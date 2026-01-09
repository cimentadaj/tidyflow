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

## Internal changes

* Replaced deprecated `workflows::pull_workflow_fit()` with
 `workflows::extract_fit_parsnip()`.

* Replaced deprecated `workflows::pull_workflow_mold()` with
 `workflows::extract_mold()` in tests.

* Replaced deprecated `tune::parameters()` with
 `hardhat::extract_parameter_set_dials()` for extracting tuning parameters
  from model specifications and recipes. The internal `parameters.tidyflow()`
  method now uses the modern extraction API.

These changes ensure compatibility with the latest tidymodels ecosystem
(workflows, tune, parsnip, dials >= 1.0.0) which deprecated the
`pull_workflow_*` and `parameters()` families of functions in favor
of the new `extract_*` functions.
