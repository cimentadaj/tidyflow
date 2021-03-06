% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyflow.R
\name{tidyflow}
\alias{tidyflow}
\title{Create a tidyflow}
\usage{
tidyflow(data = NULL, seed = NULL)
}
\arguments{
\item{data}{A data frame or tibble used to begin the tidyflow. This is
optional as the data can be specified with \code{\link[=plug_data]{plug_data()}}.}

\item{seed}{A seed to be used for reproducibility across the complete
workflow. This seed is used for every step, to ensure the same result
when doing random splitting, resampling and model fitting.}
}
\value{
A new \code{tidyflow} object.
}
\description{
A \code{tidyflow} is a container object that aggregates information required to
fit and predict from a model. This information might be the main dataset,
specified through \code{plug_data()}, a recipe used in
preprocessing, specified through \code{plug_recipe()}, or the model specification
to fit, specified through \code{plug_model()}. However, it supports more
complex workflows, such as \code{plug_split()}, \code{plug_resample()},
\code{plug_grid()}.
}
\examples{
library(recipes)
library(rsample)
library(dials)
library(parsnip)
library(tune)

wflow <-
 mtcars \%>\%
 tidyflow(seed = 23113) \%>\%
 plug_recipe(~ recipe(mpg ~ cyl, .x) \%>\% step_log(cyl))

# tidyflow gives a prinout of the current specification
# in the order of execution:
wflow

# The minimum tidyflow is: data + recipe/formula + model
wflow <-
 wflow \%>\%
 plug_model(set_engine(linear_reg(), "lm"))

# The output shows that we have the data, the recipe and the model
wflow

# We can fit that model and we get a brief print out of the model:
fit(wflow)

# We can add further steps and the print out will tell you the
# workflow specification:
wflow <-
 wflow \%>\%
 plug_split(initial_split) \%>\%
 plug_resample(vfold_cv, v = 2) \%>\%
 plug_grid(grid_regular)

# The print out shows that we have a split/resample/grid
# now set correcly.
wflow

}
