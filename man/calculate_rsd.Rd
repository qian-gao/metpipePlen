% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_rsd.R
\name{calculate_rsd}
\alias{calculate_rsd}
\title{calculate_rsd}
\usage{
calculate_rsd(
  data,
  type,
  impute = FALSE,
  names.suffix = NULL,
  other.eval = FALSE
)
}
\arguments{
\item{data}{A data frame object, sample x feature.}

\item{type}{A vector indicating sample types.}

\item{impute}{TRUE/FALSE, if impute missing values, if TRUE, half minimum is imputed.}

\item{names.suffix}{Suffix to add to the output variable names.}

\item{other.eval}{TRUE/FALSE, if calculate other summary (MAD and VAR).}
}
\value{
A list of summaries for all types of samples.
}
\description{
Calculate summary for subsets of a dataset. It is deprecated.
Use calculate_summary instead.
}
