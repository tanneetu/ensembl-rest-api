# Initialize the BiocFileCache

path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
bfc <- BiocFileCache(path, ask = FALSE)

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

create_cache <- function(bfc, url, hash, method = "GET", body = NULL, headers = list("Accept" = "application/json")) {
  # Fetch data from the API
  req <- request(url) |> req_headers(.headers = headers)

  # Add body for POST requests
  if (method == "POST" && !is.null(body)) {
    req <- req_body_json(req, body, auto_unbox = TRUE)
    # Testing request
    req_dry_run(req)
  }

  # Perform the request
  resp <- req |> req_perform()

  # Parse the response into JSON
  content <- resp_body_json(resp)

  api_data <- head(fromJSON(toJSON(content)))

  # Save the data to the cache
  cache_file <- tempfile(tmpdir = path)
  saveRDS(api_data, file = cache_file)

  # Add to BiocFileCache
  bfcadd(bfc, rname = hash, fpath = cache_file, action = "asis")
  message("Data fetched from API and cached successfully.")
}

check_cache <- function(bfc, hash, time_mins = 5) {
  # Query the cache for the given hash
  cached_entry <- bfcquery(bfc, query = hash, field = "rname", exact = TRUE)

  # If no cache entry exists, return FALSE
  if (nrow(cached_entry) == 0) {
    message("No cache found.")
    return(FALSE)
  }

  # If multiple cache entries exist, clear all
  if (nrow(cached_entry) > 1) {
    message("Multiple cache entries found. Removing all the entries...")
    clear_cache(bfc, hash)
    return(FALSE)
  }

  # Get cache info
  cache_info <- bfcinfo(bfc, rid = cached_entry$rid)

  # Convert access_time to POSIXct
  access_time <- ymd_hms(cache_info$access_time)
  current_time <- now()

  # Check if cache is outdated
  if (difftime(current_time, access_time, units = "mins") > time_mins) {
    message("Cache is outdated.")
    return(FALSE)
  } else {
    message("Cache is valid and up to date.")
    return(TRUE)
  }
}

update_cache <- function(bfc, url, hash, method = "GET", body = NULL, headers = list("Accept" = "application/json")) {
  message("Updating cache...")

  # Fetch fresh data from the API
  req <- request(url) |> req_headers(.headers = headers)

  if (method == "POST" && !is.null(body)) {
    req <- req_body_json(req, body, auto_unbox = TRUE)
    req_dry_run(req)  # Dry run for testing
  }

  # Perform the API request
  resp <- req |> req_perform()
  content <- resp_body_json(resp)
  api_data <- head(fromJSON(toJSON(content)))

  # Get existing cache entry
  cached_entry <- bfcquery(bfc, query = hash, field = "rname", exact = TRUE)

  if (nrow(cached_entry) > 0) {
    # Save new data to a file in the cache directory
    cache_file <- tempfile(tmpdir = path)
    #cache_file <- file.path(get_cache_location(), paste0(hash, ".rds"))
    saveRDS(api_data, file = cache_file)

    # Delete the old cached file (if it exists)
    if (file.exists(cached_entry$rpath)) {
      file.remove(cached_entry$rpath)
    }

    # Overwrite existing cache entry
    bfc[[cached_entry$rid]] <- cache_file
    message("Cache successfully updated.")
  } else {
    message("No existing cache found. Creating new cache entry...")
    create_cache(bfc, url, hash, method, body, headers)
  }
}

read_cache <- function(bfc, hash) {
  # Query the cache for the given hash
  cached_entry <- bfcquery(bfc, query = hash, field = "rname", exact = TRUE)

  # Retrieve local file path from cache
  cache_file <- cached_entry$rpath

  # Read and return cached data
  message("Loading data from cache...")
  return(readRDS(cache_file))
}

clear_cache <- function(bfc, hash) {
  # Query the cache for the given hash
  cached_entry <- bfcquery(bfc, query = hash, field = "rname", exact = TRUE)

  # Remove all cache entries for this hash
  for (i in seq_len(nrow(cached_entry))) {
    rid <- cached_entry$rid[i]

    # Delete the corresponding local file
    cache_file <- cached_entry$rpath[i]
    if (file.exists(cache_file)) {
      file.remove(cache_file)
    }

    # Remove from BiocFileCache
    bfcremove(bfc, rid)
  }

  message("Cache cleared successfully.")
  return(TRUE)
}

