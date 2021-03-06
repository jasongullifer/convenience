% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/serr.R
\name{serr}
\alias{serr}
\alias{serr_}
\title{Computes the standard error of the mean over an id variable.}
\usage{
serr(data, dv, id, ..., confint_serr = 1, return_interim_data = F)

serr_(data, dv, id, grouping, confint_serr = 1,
  return_interim_data = F)
}
\arguments{
\item{data}{A dataframe or datatable}

\item{dv}{Variable representing your dependent variable. Use the bare variable name when using \code{serr}.}

\item{id}{Variable representing an id variable (e.g., "subject" or "item"). Use the bare variable name when using \code{serr}.}

\item{...}{Variables to group by (e.g., conditions) when using \code{serr}. Use bare variable names separated by commas.}

\item{confint_serr}{Size (in standard errors) for confidence intervals. Default confidence intervals are 1 SERR. To get 95\% confidence intervals, enter 1.96.}

\item{return_interim_data}{TRUE or FALSE, whether to return the interim dataset (e.g., subject-level data) as well as the grand mean dataset (via a list object). By default (FALSE), only the grand mean dataset is returned.}

\item{grouping}{Character vector of the set of variables to group by (e.g.,
conditions) when using \code{serr_}.}
}
\description{
Note: this package is deprecated. Please use \code{sem} instead. This function will compute the standard error of the mean given a raw
(trial-by-trial) dataset, a dependent variable of interest, an id variable
(e.g., "subjects" or "items"), and a set of grouping variables (e.g.,
"condition"). It also outputs several statistics: SD, N, and confidence
intervals. \cr\cr Two versions of the function are provided. The standard
\code{serr} function will accept \code{dplyr}-like arguments (without quotes)
for column names. The other \code{serr_} function works on quoted strings,
and all grouping variables must be supplied as a vector of characters to the
\code{grouping} argument.
}
\examples{
library(languageR)
lexdec.rt <- lexdec[lexdec$Correct=="correct",]
serr_summary.rt <- serr(data = lexdec.rt, dv = RT, id = Subject, Class, Complex)
print(serr_summary.rt)
}
\keyword{error}
\keyword{grouped}
\keyword{mean,}
\keyword{of}
\keyword{operations}
\keyword{standard}
\keyword{the}
