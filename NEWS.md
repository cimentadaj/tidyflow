# tidyflow (development version)

## Breaking changes

* Minimum R version requirement increased from 3.2 to 4.1 to align with
  workflows package requirements.

* Minimum workflows package version increased from 0.1.3 to 1.0.0.

## Internal changes

* Replaced deprecated `workflows::pull_workflow_fit()` with
  `workflows::extract_fit_parsnip()` (#compatibility).

* Replaced deprecated `workflows::pull_workflow_mold()` with
  `workflows::extract_mold()` in tests (#compatibility).

These changes ensure compatibility with the latest tidymodels/workflows
package (>= 1.0.0) which deprecated the `pull_workflow_*` family of
functions in favor of the new `extract_*` functions.
