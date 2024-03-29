% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import.R
\name{normalize_Scales}
\alias{normalize_Scales}
\title{Normalizes item responses}
\usage{
normalize_Scales(x)
}
\arguments{
\item{x}{csv file name}

\item{Items}{data frame item meta data}

\item{Parts}{data frame with traits}
}
\value{
ESM table (tbl_esm)
}
\description{
when augmentation meta data is available,
rating scale responses are reversed and scaled to a unit interval
}
\author{
Martin Schmettow
}
