% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/information.R
\name{info_genomes_division_division_name}
\alias{info_genomes_division_division_name}
\title{Find information about all genomes in a given division.}
\usage{
info_genomes_division_division_name(division)
}
\arguments{
\item{division}{A character string specifying the division. Accepted values:
`"Metazoa"`, `"Fungi"`, `"Plants"`, `"Protists"`, `"Vertebrates"`, or `"Bacteria"`.}
}
\value{
A list containing genome data from the Ensembl REST API.
}
\description{
Fetches genome information from the Ensembl REST API for a given division. May be large for Ensembl Bacteria.
}
\examples{
# Retrieve genome data for Ensembl Plants
info_genomes_division_division_name(division = "Vertebrates")
}
\seealso{
\url{https://rest.ensembl.org/documentation/info/info_genomes_division}
}
