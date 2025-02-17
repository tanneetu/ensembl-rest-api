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

    # Cache decision
    if (cache_status$cache_exists && !cache_status$is_up_to_date) {
      update_cache(path, bfc, hash, species_names)  # Update existing cache
    } else {
      create_cache(path, bfc, hash, species_names)  # Create new cache
    }
  }

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
