# Initialize the BiocFileCache
path <- file.path(getwd(), "Cache")
bfc <- BiocFileCache("Cache", ask = FALSE)

# Species list endpoint
endpoint <- "/info/species/"
url <- build_url(endpoint, list(), "")

# Check cached data
cached_entry <- bfcquery(bfc, query= "species_data", field = "rname", exact = TRUE)
#print(cached_entry)

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
