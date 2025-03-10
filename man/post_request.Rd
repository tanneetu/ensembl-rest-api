% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{post_request}
\alias{post_request}
\title{Perform a POST Request to an API}
\usage{
post_request(url, body)
}
\arguments{
\item{url}{A character string specifying the API endpoint.}

\item{body}{A list containing the data to be sent in the request body.}
}
\value{
A structured response object parsed from JSON.
}
\description{
Sends a POST request to the specified URL with a JSON-formatted body.
It includes appropriate headers and parses the response into a structured format.
}
\keyword{internal}
