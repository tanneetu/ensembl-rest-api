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
