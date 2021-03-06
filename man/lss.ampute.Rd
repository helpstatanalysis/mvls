% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lssampute.R
\name{lss.ampute}
\alias{lss.ampute}
\title{lss.ampute}
\usage{
lss.ampute(n, time, mean, sd, sd.e, perc.up, perc.zero, perc.down, perc.ampute,
  ampute = "mcar", set.seed = NULL)
}
\arguments{
\item{n}{It's the number of record (subjects).}

\item{time}{It's the number of longitudinal mesurements.}

\item{mean}{It's the mean for the first longitudinal mesurements.}

\item{sd}{It's the standard deviation for the first mesurment.}

\item{sd.e}{It's the error on single mesurements.}

\item{perc.up}{It's the percentage (0.00 to 1.00) of the positive trend longitudinal mesurements.}

\item{perc.zero}{It's the percentage (0.00 to 1.00) of the zero trend longitudinal mesurements.}

\item{perc.down}{It's the percentage (0.00 to 1.00) of the negative trend longitudinal mesurements.}

\item{perc.ampute}{It's the percentage of missing data (after amputation)}

\item{ampute}{It's the type of amputation methods (default it's "mcar"): "mcar", "mar" and "mnar" are allowed.}

\item{set.seed}{Default it's NULL, but it could be set as you prefer.}
}
\value{
$data It's the database with real values,

$data.ampute It's the database with missing data.
}
\description{
Create and ampute a longitudinal dataset
}
\author{
Lorenzo Querci <lorenzo.querci@studio.unibo.it>
}
