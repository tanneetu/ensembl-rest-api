#' @title Validate Genome Name Against Ensembl Genome Names List
#'
#' @description
#' This function validates a given genome name by checking its existence in the Ensembl database.
#' Ensembl genome names are fetched from the cache if available. Otherwise, they are retrieved from the API
#' and cached for future use.
#' If the genome name is found, the function returns `TRUE`. If not, it suggests the closest matches
#' based on the Damerau-Levenshtein distance and stops execution with an error message.
#'
#' @param genome_name A character string representing the genome name to validate.
#'
#' @return If the genome name is valid, logical `TRUE` is returned. If not, an error is thrown with suggested
#' closest matching genome names.
#'
#' @import BiocFileCache rappdirs
#' @importFrom stringdist stringdist
#' @keywords internal
validate_genome_name <- function(genome_name) {

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/info/genomes/division/"
  cache_key <- "name"

  # Create unique hash for caching
  hash <- create_hash(endpoint, cache_key = cache_key)

  # Check cache status
  cache_status <- check_cache(bfc,hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {

    all_genome_names <- suppressMessages(read_cache(bfc, hash))

  } else {
    message(" Fetching genome information from API and caching names...")

    divisions <- c("Metazoa", "Fungi", "Plants","Protists",
                   "Vertebrates","Bacteria")

    genome_list <- lapply(divisions, function(div) {
      genome_data <- suppressMessages(info_genomes_division_division_name(division = div))
      return(genome_data$name)
    }
    )

    # Combine all species names into a single vector
    all_genome_names <- do.call(c, genome_list)

    if (cache_status$cache_exists && !cache_status$is_up_to_date) {
      suppressMessages(update_cache(path, bfc, hash, all_genome_names))
    } else {
      suppressMessages(create_cache(path, bfc, hash, all_genome_names))
    }
  }

  # Convert genome_name to lowercase to ensure case-insensitive matching
  genome_name <- tolower(genome_name)
  all_genome_names <- unique(tolower(all_genome_names))

  # Validate genome_name
  if (genome_name %in% all_genome_names) {

    return (TRUE)

  } else {

    # Compute distance between the given name and the valid name list using Damerau-Levenshtein method
    distances <- stringdist(genome_name, all_genome_names, method = "dl")

    # Define a threshold to suggest closely related matches
    min_distance <- min(distances)
    distance_threshold <- min_distance + 1

    # Get indices of names within threshold
    closest_indices <- which(distances <= distance_threshold)

    best_matches <- all_genome_names[closest_indices]

    string_best_matches <- paste(best_matches, collapse = ", ")

    stop("Genome name not found! Did you mean any of these: ", string_best_matches, "?")

  }

}

#' @title Validate Species Name Against Ensembl Species List
#'
#' @description
#' This function validates a given species name by checking its existence in the Ensembl database.
#' Ensembl species names and aliases are fetched from the cache if available. Otherwise, they are retrieved from the API
#' and cached for future use.
#' If the species name is found, the function returns `TRUE`. If not, it suggests the closest matches
#' based on the Damerau-Levenshtein distance and stops execution with an error message.
#'
#' @param species_name A character string representing the species name to validate.
#'
#' @return If the species name is valid, logical `TRUE` is returned. If not, an error is thrown with suggested
#'         closest matching species names.
#'
#' @import BiocFileCache rappdirs
#' @importFrom stringdist stringdist
#' @keywords internal
validate_species_name <- function(species_name) {

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/info/species/"
  cache_key <- "name_aliases"

  # Create unique hash for caching
  hash <- create_hash(endpoint, cache_key = cache_key)

  cache_status <- check_cache(bfc,hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    all_species_data <- suppressMessages(read_cache(bfc, hash))
  } else {
    message("Fetching species information from API and caching names and aliases...")

    divisions <- c("Metazoa", "Fungi", "Plants","Protists",
                   "Vertebrates", "Bacteria")

    species_list <- lapply(divisions, function(div) {
      species_info <- suppressMessages(info_species(division = div))

      # Return separate data frames for each division
      return(species_info [, c("name","aliases")])
    })

    # Merge all the data frames
    all_species_data <- do.call(rbind, species_list)

    if (cache_status$cache_exists && !cache_status$is_up_to_date) {
      suppressMessages(update_cache(path, bfc, hash, all_species_data))
    } else {
      suppressMessages(create_cache(path, bfc, hash, all_species_data))
    }
  }

  # Convert species_name to lowercase to ensure case-insensitive matching
  species_name<- tolower(species_name)

  # Extract names and flatten aliases list
  all_species_names <- tolower(all_species_data$name)
  all_species_aliases <- unique(tolower(unlist(all_species_data$aliases)))
  all_species_aliases <- all_species_aliases[nzchar(all_species_aliases)]

  all_valid_names <- c(all_species_names, all_species_aliases)

  # Validate species_name
  if (species_name %in% all_valid_names) {

    return(TRUE)

  } else {

    # Compute distance between the given name and the valid name list using Damerau-Levenshtein method
    distances <- stringdist(species_name, all_valid_names, method = "dl")

    # Define a threshold to suggest closely related matches
    distance_threshold <- min(distances) + 1

    # Get indices of names within threshold
    closest_indices <- which(distances <= distance_threshold)

    best_matches <- all_valid_names[closest_indices]

    string_best_matches <- paste(best_matches, collapse = ", ")

    stop("Species name not found! Did you mean any of these: ", string_best_matches, "?")

  }

}


#' @title Find information about available cytogenetic band information for a given species.
#'
#' @description
#' Fetches the band information from the `info_assembly_species` endpoint function and process it for better retrieval.
#'
#' @param species A character string of species name or alias.
#'
#' @return A list containing cytogenetic band information retrieved from the Ensembl REST API.
#' @keywords internal
get_info_karyotype_band <- function(species) {

  raw_assembly_species_data <- info_assembly_species(species = species, bands = 1)

  raw_bands_data <- raw_assembly_species_data$top_level_region$bands

  # Combine all list elements (each representing a small data frame) into a single data frame
  bands_data <- do.call(rbind, raw_bands_data)

  # Fixed order of the columns
  col_order <- c("seq_region_name", "start", "end", "id", "stain", "assembly_name", "strand")

  bands_data <- bands_data[,col_order]

  #TODO
  # Sort the "seq_region_name" separately for numeric and character values

  # Reorder the rows based on the sorted values of character vector "seq_region_name"
  bands_data_sorted <- bands_data[order(bands_data$seq_region_name, bands_data$start), ]

  return(bands_data_sorted)
}


#' @title Find information about available cytogenetic band information for a given species.
#'
#' @param species A character string of species name or alias.
#'
#' @return A list containing cytogenetic band information retrieved from the Ensembl REST API.
#' @export
#'
#' @import BiocFileCache rappdirs
#'
#' @examples
#' # Retrieve karyotype band information for human.
#' info_karyotype_band(species = "human")
info_karyotype_band <- function(species) {

  if(missing(species) || length(species) == 0){
    stop("Species is missing! Please provide a valid species name. ")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc<- BiocFileCache(path, ask = FALSE)

  endpoint<-"/info/assembly/"

  # extra_func is used to distinguisih it from info_assembly_species with bands=1
  hash <- create_hash(endpoint, species = species, bands = 1, extra_func = TRUE)

  result_data <- fetch_data_with_cache(hash, get_info_karyotype_band, species = species)

  return(result_data)

}

get_crossref_id <- function(id, external_db){

  all_ids_data <- lapply(id, xref_id, external_db=external_db)
  #named_data <- setNames(all_ids_data, id)

  final_data <- do.call(rbind, all_ids_data)
  return(final_data)
}

crossref_id <- function(id, external_db){

  if(missing(id) || length(id) == 0 ){
    stop("ID is missing! Please provide a valid id.")
  }

  if(missing(external_db) || length(external_db) == 0){
    stop("External database name is missing! Please provide an external database name.")
  }

  path<- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc<- BiocFileCache(path, ask = FALSE)

  endpoint <- "/xrefs/id/"

  hash<- create_hash(endpoint, id = id, external_db = external_db, extra_func =1)

  result_data <- fetch_data_with_cache(hash, get_crossref_id, id = id, external_db = external_db)

  return(result_data)
}



