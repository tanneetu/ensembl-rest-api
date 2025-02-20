#' Validate Genome Name Against Ensembl Species List Names
#'
#' @description
#' This function validates a given genome name against the available species names in the Ensembl database.
#' If the genome name is found, it is returned in lowercase. Otherwise, the function computes the
#' Damerau-Levenshtein distance to suggest the closest matching species names.
#' Species names are fetched from the cache if available; otherwise, they are retrieved from the API
#' and cached for future use.
#'
#' @param genome_name A character string representing the genome name to validate.
#'
#' @return If the genome name is valid, it is returned as a lowercase string. If not found, an error
#' is raised, suggesting the closest possible matches.
#'
#' @import BiocFileCache rappdirs
#' @importFrom stringdist stringdist
#' @keywords internal
validate_genome_name <- function(genome_name) {

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/info/species/"
  cache_key <- "name"

  # Create unique hash for caching
  hash <- create_hash(endpoint, cache_key = cache_key)

  # Check cache status
  cache_status <- check_cache(bfc,hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    species_names <- read_cache(bfc, hash)
  } else {
    message(" Fetching species information from API and caching names...")

    divisions <- c("Metazoa", "Fungi", "Plants","Protists",
                   "Vertebrates", "Bacteria")

    species_list <- lapply(divisions, function(div) {
      species_data <- info_species(division = div)
      return(species_data$name)
    }
    )

    # Combine all species names into a single vector
    species_names <- do.call(c, species_list)

    if (cache_status$cache_exists && !cache_status$is_up_to_date) {
      update_cache(path, bfc, hash, species_names)
    } else {
      create_cache(path, bfc, hash, species_names)
    }
  }

  # Convert genome_name to lowercase to ensure case-insensitive matching
  genome_name <- tolower(genome_name)

  # Validate genome_name
  if (genome_name %in% species_names) {
    return (genome_name)
  } else {

    # Compute distance using Damerau-Levenshtein method
    distances <- stringdist(genome_name, species_names, method = "dl")

    # Define a threshold
    min_distance <- min(distances)
    distance_threshold <- min_distance + 1

    # Get indices of names within threshold
    closest_indices <- which(distances <= distance_threshold)

    # Extract species names within the threshold
    best_matches <- species_names[closest_indices]

    string_best_matches <- paste(best_matches, collapse = ", ")

    # Return error with multiple suggestions
    stop(paste("Genome name not found! Did you mean any of these:",
               string_best_matches,"?"))
  }
}
