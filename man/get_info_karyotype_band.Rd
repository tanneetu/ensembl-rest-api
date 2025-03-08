% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extras.R
\name{get_info_karyotype_band}
\alias{get_info_karyotype_band}
\title{Find information about available cytogenetic band information for a given species.}
\usage{
get_info_karyotype_band(species)
}
\arguments{
\item{species}{A character string of species name or alias.}
}
\value{
A list containing cytogenetic band information retrieved from the Ensembl REST API.
}
\description{
Fetches the band information from the `info_assembly_species` endpoint function and process it for better retrieval.
}
\keyword{internal}
