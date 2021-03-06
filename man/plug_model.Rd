% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit-action-model.R
\name{plug_model}
\alias{plug_model}
\alias{drop_model}
\alias{replace_model}
\title{Add a model to a tidyflow}
\usage{
plug_model(x, spec, formula = NULL)

drop_model(x)

replace_model(x, spec, formula = NULL)
}
\arguments{
\item{x}{A tidyflow.}

\item{spec}{A parsnip model specification.}

\item{formula}{An optional formula override to specify the terms of the
model. Typically, the terms are extracted from the formula or recipe
preprocessing methods. However, some models (like survival and bayesian
models) use the formula not to preprocess, but to specify the structure
of the model. In those cases, a formula specifying the model structure
must be passed unchanged into the model call itself. This argument is
used for those purposes.}
}
\value{
\code{x}, updated with either a new or removed model.
}
\description{
\itemize{
\item \code{plug_model()} adds a parsnip model to the tidyflow.
\item \code{drop_model()} removes the model specification as well as any fitted
model object. Any extra formulas are also removed. Doesn't remove any steps
from the \code{pre} stage.
\item \code{replace_model()} first removes the model then adds the new specification to
the tidyflow.
}
}
\details{
\code{plug_model()} is a required step to construct a minimal tidyflow.
}
\examples{
library(parsnip)

# Define two competing model:
lm_model <- set_engine(linear_reg(), "lm")
regularized_model <- set_engine(lm_model, "glmnet")

# Define a minimal tidyflow: data + formula + model
wf <-
  mtcars \%>\%
  tidyflow() \%>\%
  plug_formula(mpg ~ .) \%>\% 
  plug_model(lm_model)

wf

# We can drop the model at any time and the remaining steps
# are intact
drop_model(wf)

# We can fit the model with `fit`:
fitted <- fit(wf)

# Extract the model if needed:
fitted \%>\%
  pull_tflow_fit()

# If we remove the model from the fitted `tidyflow`,
# the fit is dropped:
drop_model(fitted)

# We could replace the model from the initial tidyflow with
# the regularized model with `replace_model`

## TODO: when https://github.com/cimentadaj/tidyflow/issues/4 is fixed
## replace wf with fitted here.

reg_fitted <-
  wf \%>\%
  replace_model(regularized_model) \%>\%
  fit()

reg_fitted \%>\%
  pull_tflow_fit()

}
