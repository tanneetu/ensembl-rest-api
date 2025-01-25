# Function to validate the species name

validate_species <- function(species, bfc) {
  # Check if species data is in the cache
  cached_entry <- bfcquery(bfc, query = "species_data", field = "rname", exact = TRUE)

  if (nrow(cached_entry) == 0) {
    stop("Species data is not cached!")
  }

  # Load cached data
  species_data <- readRDS(cached_entry$rpath)
  valid_species <- sapply(species_data$species, function(x) x$name)

  # Check if the provided species is valid
  if (!(species %in% valid_species)) {
    stop("Invalid species provided!")
  }
}

