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

