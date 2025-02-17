# Implemented Ensembl API Endpoints
# info/species
# info/genomes/genome_name

get_info_species <- function(division = NULL, ...){

  optional_params_list<- list(division = division, ...)

  string_optional_params <- query_string_optional_params(optional_params_list)

  endpoint <- "/info/species/"

  url <- build_url(endpoint, string_optional_params = string_optional_params)

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)

  result<- fromJSON(toJSON(content, auto_unbox = TRUE))

  species_data <- result$species

  assign("Check_Species", species_data, envir = .GlobalEnv)

  return(species_data)
}

info_species<-function(division = NULL, ...){

  if (is.null(division) || length(division) == 0) {
    stop("Division is missing! Please provide a valid division name.")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  all_params_list<- list(...)

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
  hash <- create_hash(endpoint, division = ensembl_division, params = all_params_list)

  # Check cache status (TRUE or FALSE)
  cache_status <- check_cache(bfc, hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    return(read_cache(bfc, hash))  # Load cached data
  }

  # Call get_info_species() to get result

  message("Fetching new data from API...")
  result_data <- get_info_species(division = ensembl_division, ...)

  # Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)  # Update existing cache
  } else if (!cache_status$cache_exists && !cache_status$is_up_to_date) {
    create_cache(path, bfc, hash, result_data)  # Create new cache
  }

  return(result_data)  # Return fetched data
}

get_info_genomes_genome_name <- function(name = NULL, ...){

  mandatory_params <- name

  optional_params <- list(...)

  string_optional_params <- query_string_optional_params(optional_params)

  endpoint<-"/info/genomes/"

  url<- build_url(endpoint, mandatory_params, string_optional_params)

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)

  result<- fromJSON(toJSON(content, auto_unbox = TRUE))

  return(result)
}

info_genomes_genome_name <- function(name= NULL, ...){

  if (is.null(name) || length(name) == 0) {
    stop("Genome name is missing! Please provide a valid genome name.")
  }

  # Validate the genome name
  name <- validate_genome_name(name)

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/info/genomes/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, name = name, ...)

  # Check cache status (TRUE or FALSE)
  cache_status <- check_cache(bfc, hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    return(read_cache(bfc, hash))  # Load cached data
  }

  message("Fetching new data from API...")
  result_data <- get_info_genomes_genome_name(name)

  #Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)  # Update existing cache
  } else if (!cache_status$cache_exists && !cache_status$is_up_to_date) {
    create_cache(path, bfc, hash, result_data)  # Create new cache
  }

  return(result_data)
}
