% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rabinoffmmmr_class.R
\name{predict.mmmr_fit}
\alias{predict.mmmr_fit}
\title{predict.mmmr_fit}
\usage{
\method{predict}{mmmr_fit}(
  object,
  newdata = NULL,
  new_prophet = FALSE,
  compute_gammatrans = FALSE,
  full_table = FALSE,
  ...
)
}
\arguments{
\item{object}{An mmmr object}

\item{newdata}{A dataframe to get new predictions from.}

\item{new_prophet}{If set to TRUE, trend, holidays, and seasonality columns will re-computed and added to the data. If FALSE, then the prophet model from the fit will be used to add trend, holidays, and seasonality.}

\item{compute_gammatrans}{If set to TRUE, new values of gammaTrans will be computed with the new data. If false, gammaTrans computed with the training data will be used.}

\item{full_table}{If set to TRUE, the return value will include the modified data as well as the predictions}

\item{...}{Not currently used}
}
\value{
An S3 object of type mmmr
}
\description{
Get predictions from an mmmr_fit object
}
