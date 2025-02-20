# Implemented Ensembl API Endpoints
# info/species
# info/genomes/genome_name

#' Retrieve Species Information from Ensembl REST API
#'
#' @description
#' Sends a GET request to the Ensembl REST API to retrieve species-related information for speciefic division.
#'
#' @param division A character string specifying the Ensembl or Ensembl Genomes division (e.g., `"EnsemblVertebrates"`).
#' @param ... Named arguments representing optional parameters for the API request.
#'
#' @return A list of species data retrieved from the Ensembl REST API.
#' @keywords internal
get_info_species <- function(division = NULL, ...){

  optional_params<- list(division = division, ...)

  endpoint <- "/info/species/"

  url <- build_url(endpoint, optional_params = optional_params)

  result<- get_request(url)

  species_data <- result$species

  return(species_data)
}

#' Lists all available species, their aliases, available adaptor groups and data release.
#'
#' @description
#' Fetches species information from the Ensembl REST API for a given division.
#' The function supports caching to avoid redundant API calls and improve performance.
#'
#' @param division A character string specifying the division. Accepted values:
#' `"Metazoa"`, `"Fungi"`, `"Plants"`, `"Protists"`, `"Vertebrates"`, or `"Bacteria"`.
#' @param ... Named arguments representing optional parameters for the API request.
#'
#' @return A list containing species data from the Ensembl REST API.
#'
#' @import BiocFileCache rappdirs
#' @export
#' @details
#' More details at \url{https://rest.ensembl.org/documentation/info/species}
#' @examples
#' # Retrieve species data for Ensembl Plants
#' info_species(division = "Plants")
#'
#' # Retrieve species data with additional parameters
#' info_species(division = "Vertebrates", strain_collection = "mouse")
info_species<-function(division = NULL, ...){

  if (is.null(division) || length(division) == 0 ) {
    stop("Division is missing! Please provide a valid division name.")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  # Mapping from usual division names to Ensembl division names
  ensembl_division <- switch(
    division,
    "Metazoa" = "EnsemblMetazoa",
    "Fungi" = "EnsemblFungi",
    "Plants" = "EnsemblPlants",
    "Protists" = "EnsemblProtists",
    "Vertebrates" = "EnsemblVertebrates",
    "Bacteria" = "EnsemblBacteria",
    stop("Invalid division name! Choose from: Metazoa, Fungi, Plants, Protists, Vertebrates, Bacteria")
  )

  endpoint <- "/info/species/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, division = ensembl_division, ...)

  result_data <- fetch_data_with_cache(hash, get_info_species, division= ensembl_division, ...)

  return(result_data)
}

#' Retrieve Genome Information from Ensembl REST API
#'
#' @description
#' Sends a GET request to the Ensembl REST API to retrieve specific genome information.
#'
#' @param name A character string specifying the name of the genome.
#'
#' @return A list containing genome-related information retrieved from the Ensembl REST API.
#' @keywords internal
get_info_genomes_genome_name <- function(name = NULL){

  mandatory_params <- name

  endpoint<-"/info/genomes/"

  url<- build_url(endpoint, mandatory_params)

  result<- get_request(url)

  return(result)
}

#' Retrieve genome information for given genome name.
#'
#' @description
#' Fetches genome-related information for a given genome name using the Ensembl REST API.
#' The function supports caching to avoid redundant API calls and improve performance.
#'
#' @param name A character string specifying the name of the genome.
#'
#' @return A list containing genome-related information retrieved from the Ensembl REST API.
#'
#' @import BiocFileCache rappdirs
#' @export
#' @details
#' More details at \url{https://rest.ensembl.org/documentation/info/info_genome}
#' @examples
#' # Retrieve genome information for Arabidopsis thaliana
#' info_genomes_genome_name(name = "arabidopsis_thaliana")
info_genomes_genome_name <- function(name= NULL){

  if (is.null(name) || length(name) == 0 || name == "") {
    stop("Genome name is missing! Please provide a valid genome name.")
  }

  # Validate the genome name
  name <- validate_genome_name(name)

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/info/genomes/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, name = name)

  result_data <- fetch_data_with_cache(hash, get_info_genomes_genome_name, name= name)

  return(result_data)
}

get_info_genomes_division_division_name<- function(division =NULL){

  endpoint <- "/info/genomes/division/"

  url <- build_url(endpoint, mandatory_params = division)

  result<- get_request(url)

  return(result)
}

info_genomes_division_division_name<- function(division =NULL){

  if (is.null(division) || length(division) == 0 ) {
    stop("Division is missing! Please provide a valid division name.")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  # Mapping from usual division names to Ensembl division names
  ensembl_division <- switch(
    division,
    "Metazoa" = "EnsemblMetazoa",
    "Fungi" = "EnsemblFungi",
    "Plants" = "EnsemblPlants",
    "Protists" = "EnsemblProtists",
    "Vertebrates" = "EnsemblVertebrates",
    "Bacteria" = "EnsemblBacteria",
    stop("Invalid division name! Choose from: Metazoa, Fungi, Plants, Protists, Vertebrates, Bacteria")
  )

  endpoint <- "/info/genomes/division/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, division = ensembl_division)

  result_data <- fetch_data_with_cache(hash, get_info_genomes_division_division_name, division= ensembl_division)

  return(result_data)

}
