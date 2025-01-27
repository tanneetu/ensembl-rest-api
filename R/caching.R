# Initialize the BiocFileCache
path <- file.path(getwd(), "Cache")
bfc <- BiocFileCache("Cache", ask = FALSE)

# Species list endpoint
endpoint <- "/info/species/"
url <- build_url(endpoint, list(), "")

create_hash <- function(endpoint, ...){

  args <- list(...)

  # Sort named arguments alphabetically by their names
  sorted_args <- args[order(names(args))]

  # Sort values for atomic arguments
  sorted_args <- lapply(sorted_args, function(x) {
    if (is.atomic(x)) sort(x) else x  # Sort atomic vectors
  })

  # Combine sorted named arguments into a formatted string(e.g.: name=value1,value2)
  hash_named_parts <- mapply(function(name, value) {
    paste0(name, "=", paste(value, collapse=","))
  }, names(sorted_args), sorted_args)

  # Create the full hash string
  hash_string <- paste(endpoint,
                       paste0("$",paste(hash_named_parts, collapse = ",")),
                       sep = ""
  )

  # Generate an MD5 hash of the combined string
  hash_key <- paste0("EnsemblRestApi_", digest::digest(hash_string, algo = "md5", serialize = FALSE))

  return(hash_key)
  }


check_cache <- function(bfc, hash){
  # Check cached data
  cached_entry <- bfcquery(bfc, query= hash, field = "rname", exact = TRUE)

  if (nrow(cached_entry) > 0) {

    # Multiple entry handling
    # Get info of cache
    cache_info <- bfcinfo(bfc, rid = cached_entry$rid)

    # Convert access_time to POSIXct object
    access_time <- ymd_hms(cache_info$access_time)
    current_time <- now()

    # Updating the cache
    if (difftime(current_time, access_time, units = "mins") > 5) {
      message("Cache is outdated. Fetching new data...")
      }

    }

}
if (nrow(cached_entry) > 0) {

  cache_info <- bfcinfo(bfc, rid = cached_entry$rid)

  # Convert access_time to POSIXct object
  access_time <- ymd_hms(cache_info$access_time)
  current_time <- now()

  # Updating the cache
  if (difftime(current_time, access_time, units = "mins") > 5) {
    message("Cache is outdated. Fetching new data...")

    # Fetch new data from the API
    req <- request(url) |>
      req_headers("Accept" = "application/json")
    resp <- req |> req_perform()
    species_data <- resp_body_json(resp)

    # Save to cache directory by updating the existing entry
    species_file <- file.path(path, "species_data.rds")
    saveRDS(species_data, file = species_file)
    bfc[[cached_entry$rid]] <- species_file
    message("Local resource updated in the cache.")

  } else {
    # Load cached data
    species_data <- readRDS(cached_entry$rpath)
    message("Species data loaded from cache.")
  }
} else {
  # Fetch data from the API
  req <- request(url) |>
    req_headers("Accept" = "application/json")
  resp <- req |> req_perform()
  species_data <- resp_body_json(resp)

  # Save to cache
  species_file <- file.path(path, "species_data.rds")
  saveRDS(species_data, file = species_file)
  bfcadd(bfc, rname = "species_data", fpath = species_file, action = "asis")

  message("Species data fetched and cached.")
}
