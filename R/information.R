# Implemented Ensembl API Endpoints
# info/species
# info/genomes/genome_name
# info/assembly/species

#' @title Retrieve Species Information from Ensembl REST API
#'
#' @description
#' Sends a GET request to the Ensembl REST API to retrieve species-related information for speciefic division.
#'
#' @param division A character string specifying the Ensembl division (e.g., `"EnsemblVertebrates"`).
#' @param ... Named arguments representing optional parameters for the API request.
#'
#' @return A list of species data retrieved from the Ensembl REST API.
#' @keywords internal
get_info_species <- function(division, ...){

  optional_params<- list(division = division, ...)

  endpoint <- "/info/species/"

  url <- build_url(endpoint, optional_params = optional_params)

  response<- get_request(url)

  result <- response$result

  species_data <- result$species

  return(species_data)
}

#' @title Lists all available species, their aliases, available adaptor groups and data release.
#'
#' @description
#' Fetches species information from the Ensembl REST API for a given division.
#'
#' @param division A character string specifying the division. Accepted values:
#' `"Metazoa"`, `"Fungi"`, `"Plants"`, `"Protists"`, `"Vertebrates"`, or `"Bacteria"`.
#' @param ... Named arguments representing optional parameters for the API request.
#'
#' @return A list containing species data from the Ensembl REST API.
#'
#' @seealso \url{https://rest.ensembl.org/documentation/info/species}
#'
#' @import BiocFileCache rappdirs
#' @export
#' @examples
#' # Retrieve species data for Ensembl Plants
#' info_species(division = "Plants")
#'
#' # Retrieve species data with additional parameters
#' info_species(division = "Vertebrates", strain_collection = "mouse")
info_species<-function(division, ...){

  if (missing(division) || division == "") {
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


#' @title Find genome information for given genome name.
#'
#' @description
#' Sends a GET request to the Ensembl REST API to retrieve specific genome information.
#'
#' @param name A character string specifying the name of the genome.
#'
#' @return A list containing either genome-related information retrieved from the Ensembl REST API
#' or an error message with a status code if the request fails.
#'
#' @keywords internal
get_info_genomes_genome_name <- function(name){

  mandatory_params <- name

  endpoint<-"/info/genomes/"

  url<- build_url(endpoint, mandatory_params)

  result<- get_request(url)

  return(result)
}

#' @title Find genome information for given genome name.
#'
#' @description
#' Fetches genome-related information for a given genome name using the Ensembl REST API.
#'
#' @param name A character string of the genome name.
#'
#' @return A list containing genome-related information from the Ensembl REST API.
#' If the API request fails, an error message with an HTTP status code is returned.
#'
#' @seealso \url{https://rest.ensembl.org/documentation/info/info_genome}
#'
#' @import BiocFileCache rappdirs
#' @export
#'
#' @examples
#' # Retrieve genome information for arabidopsis_thaliana
#' info_genomes_genome_name(name = "arabidopsis_thaliana")
info_genomes_genome_name <- function(name){

  if (missing(name) || name == "") {
    stop("Genome name is missing! Please provide a valid genome name.")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/info/genomes/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, name = name)

  result_data <- fetch_data_with_cache(hash, get_info_genomes_genome_name, name= name)

  # If error occurred
  if (!is.null(result_data$error_code)) {

    # Suggest valid genome names for status code 400
    if (result_data$error_code == 400) {

      validate_genome_name(name)

    } else {

      # Provide status code and error message
      message("Error: HTTP ", result_data$error_code, " - ", result_data$message)
    }

  } else {

    result <- result_data$result

    return(result)
  }
}


#' @title Find information about all genomes in a given division.
#'
#' @description
#' Fetches genome-related information for a specified division using the Ensembl REST API.
#'
#' @param division A character string specifying the Ensembl division (e.g., `"EnsemblVertebrates"`).
#'
#' @return A list containing genome-related information for the given division from the Ensembl REST API.
#'
#' @keywords internal
get_info_genomes_division_division_name<- function(division){

  endpoint <- "/info/genomes/division/"

  url <- build_url(endpoint, mandatory_params = division)

  response<- get_request(url)

  result <- response$result

  return(result)
}


#' @title Find information about all genomes in a given division.
#'
#' @description
#' Fetches genome information from the Ensembl REST API for a given division. May be large for Ensembl Bacteria.
#'
#' @param division A character string specifying the division. Accepted values:
#' `"Metazoa"`, `"Fungi"`, `"Plants"`, `"Protists"`, `"Vertebrates"`, or `"Bacteria"`.
#'
#' @return A list containing genome data from the Ensembl REST API.
#'
#' @seealso \url{https://rest.ensembl.org/documentation/info/info_genomes_division}
#'
#' @import BiocFileCache rappdirs
#' @export
#' @examples
#' # Retrieve genome data for Ensembl Plants
#' info_genomes_division_division_name(division = "Vertebrates")
info_genomes_division_division_name<- function(division){

  if (missing(division) || length(division) == 0 ) {
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



#' @title List the currently available assemblies for a species, along with toplevel sequences, chromosomes and cytogenetic bands.
#'
#' @description
#' Sends a GET request to the Ensembl REST API to retrieve assembly-related information for speciefic species.
#'
#' @param species A character string of species name or alias.
#' @param bands An optional boolean (`0` or `1`). If set to `1`, include cytogenetic band information.
#' Only display if band information is available.
#'
#' @return A list containing assembly-related information retrieved from the Ensembl REST API.
#' If the API request fails, an error message with an HTTP status code is returned.
#' @keywords internal
get_info_assembly_species <- function(species, bands = NULL){

  mandatory_params <- species

  optional_params <- list(bands = bands)

  endpoint<-"/info/assembly/"

  url<- build_url(endpoint, mandatory_params, optional_params)

  result<- get_request(url)

  return(result)
}


#' @title List the currently available assemblies for a species, along with toplevel sequences, chromosomes and cytogenetic bands.
#'
#' @description
#' Fetches assembly-related information for a specified species using the Ensembl REST API.
#' The function retrieves available genome assemblies, including toplevel sequences, chromosomes,
#' and optional cytogenetic band information.
#'
#' @param species A character string of species name or alias.
#' @param bands An optional boolean (`0` or `1`). If set to `1`, include cytogenetic band information.
#' Only display if band information is available.
#'
#' @return A list containing assembly-related information retrieved from the Ensembl REST API.
#' If the API request fails, an error message with an HTTP status code is returned.
#'
#' @seealso \url{https://rest.ensembl.org/documentation/info/assembly_info}
#'
#' @import BiocFileCache rappdirs
#'
#' @export
#'
#' @examples
#' # Retrieve assembly information for anopheles_melas
#' info_assembly_species(species = "anopheles_melas")
#'
#' # Retrieve assembly information for homo_sapiens including cytogenetic band information
#' info_assembly_species(species = "human", bands = 1)
info_assembly_species <- function(species, bands = NULL) {

  if(missing(species) || length(species) == 0 ){
    stop("Species is missing! Please provide a valid species name. ")
  }

  # Initialize BiocFileCache inside the function
  path<- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc<- BiocFileCache(path, ask = FALSE)

  endpoint<-"/info/assembly/"

  hash <- create_hash(endpoint, species = species, bands = bands)

  result_data <- fetch_data_with_cache(hash, get_info_assembly_species, species = species, bands = bands)

  # If error occurred
  if(!is.null(result_data$error_code)) {

    # Suggest valid species names for status code 400
    if(result_data$error_code == 400){

      validate_species_name(species)

    } else{

      # Provide status code and error message
      message("Error: HTTP ", result_data$error_code, " - ", result_data$message)
    }

  } else{

    result <- result_data$result

    return(result)
  }

}

