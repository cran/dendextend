% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pvclust.R
\name{na_locf}
\alias{na_locf}
\title{Last Observation Carried Forward}
\source{
\url{https://stat.ethz.ch/pipermail/r-help/2003-November/042126.html}
\url{https://stackoverflow.com/questions/5302049/last-observation-carried-forward-na-locf-on-panel-cross-section-time-series}

This could probably be solved MUCH faster using Rcpp.
}
\usage{
na_locf(x, first_na_value = 0, recursive = TRUE, ...)
}
\arguments{
\item{x}{some vector}

\item{first_na_value}{If the first observation is NA, fill it with "first_na_value"}

\item{recursive}{logical (TRUE). Should na_locf be re-run until all NA values are filled?}

\item{...}{ignored.}
}
\value{
The original vector, but with all the missing values filled by the value
before them.
}
\description{
A function for replacing each NA with the most recent non-NA prior to it.
}
\examples{
na_locf(c(NA, NA))
na_locf(c(1, NA))
na_locf(c(1, NA, NA, NA))
na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), recursive = FALSE)
\dontrun{

# library(microbenchmark)
# library(zoo)

# microbenchmark(
#  na_locf = na_locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4)),
#  na.locf = na.locf(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4))
#) # my implementation is 6 times faster :)

#microbenchmark(
#  na_locf = na_locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000)),
#  na.locf = na.locf(rep(c(1, NA, NA, NA, 2, 2, NA, 3, NA, 4), 1000))
# ) # my implementation is 3 times faster

}

}
\seealso{
\link[zoo]{na.locf}
}
