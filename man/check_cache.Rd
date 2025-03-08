% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caching.R
\name{check_cache}
\alias{check_cache}
\title{Check Cache Status}
\usage{
check_cache(bfc, hash, time_mins = 5)
}
\arguments{
\item{bfc}{A `BiocFileCache` object used to manage cached data.}

\item{hash}{A character string representing the unique hash for the cached data.}

\item{time_days}{An integer specifying the maximum time (in days) for cache validity.
Default is 7 days.}
}
\value{
A named list with two logical values:
\itemize{
  \item `cache_exists` - `TRUE` if the cache entry exists, `FALSE` otherwise.
  \item `is_up_to_date` - `TRUE` if the cache is still valid, `FALSE` if it is outdated.
}
}
\description{
This function checks if a cached entry exists for a given hash in the BiocFileCache.
It determines whether the cache is valid, up-to-date, or needs to be refreshed.
}
\keyword{internal}
