# Initialize BiocFileCache inside the calling endpoint function
#path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
#bfc <- BiocFileCache(path, ask = FALSE)

#' Generate a Unique Hash for API Caching
#'
#' @description
#' This function generates a unique MD5 hash based on the API endpoint and
#' provided arguments. It ensures consistent hashing by sorting the arguments
#' before constructing the hash string.
#'
#' @param endpoint A character string specifying the API endpoint (e.g., `"/lookup/symbol/"`).
#' @param ... Additional arguments that are passed to the API endpoint function.
#'
#' @return A character string representing the unique MD5 hash.
#' @import digest
#' @keywords internal
create_hash <- function(endpoint, ...){

  args <- list(...)

  # Check if at least one argument is provided
  if (length(args) == 0) {
    stop("At least one argument must be provided.")
  }
  # Check if named arguments are given
  else if (is.null(names(args)) || any(names(args) == "")) {
    stop("All arguments must be named.")
  }
  else {
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

}

#' Create a Cache Entry for API Data
#'
#' @description
#' This function stores API response data in a cache directory using `BiocFileCache`.
#' A temporary file is created in the specified cache location, and the data is saved in `.rds` format.
#' The cache entry is then registered in the `BiocFileCache` system to allow for easy retrieval later.
#'
#' @param path A character string specifying the directory where the cache should be stored.
#' @param bfc A `BiocFileCache` object used to manage cached data.
#' @param hash A character string representing the unique hash for the cached data.
#' @param result_data The API response data to be saved in the cache.
#'
#' @return None. The function saves the data in the cache and prints a message.
#' @import BiocFileCache
#' @keywords internal
create_cache <- function(path, bfc, hash, result_data) {

  api_data <- result_data

  # Save the data to the cache
  cache_file <- tempfile(tmpdir = path)
  saveRDS(api_data, file = cache_file)

  # Add to BiocFileCache
  bfcadd(bfc, rname = hash, fpath = cache_file, action = "asis")
  message("Data fetched from API and cached successfully.")
}

#' Check Cache Status
#'
#' @description
#' This function checks if a cached entry exists for a given hash in the BiocFileCache.
#' It determines whether the cache is valid, up-to-date, or needs to be refreshed.
#'
#' @param bfc A `BiocFileCache` object used to manage cached data.
#' @param hash A character string representing the unique hash for the cached data.
#' @param time_days An integer specifying the maximum time (in days) for cache validity.
#'                  Default is 7 days.
#'
#' @return A named list with two logical values:
#' \itemize{
#'   \item `cache_exists` - `TRUE` if the cache entry exists, `FALSE` otherwise.
#'   \item `is_up_to_date` - `TRUE` if the cache is still valid, `FALSE` if it is outdated.
#' }
#' @import BiocFileCache
#' @importFrom lubridate ymd_hms now
#' @keywords internal
check_cache <- function(bfc, hash, time_days = 7) {
  # Query the cache for the given hash
  cached_entry <- bfcquery(bfc, query = hash, field = "rname", exact = TRUE)

  # If no cache entry exists, return FALSE
  if (nrow(cached_entry) == 0) {
    message("No cache found.")
    return(list(cache_exists = FALSE, is_up_to_date = FALSE))
  }

  # If multiple cache entries exist, clear all
  if (nrow(cached_entry) > 1) {
    message("Multiple cache entries found. Removing all the entries...")
    clear_cache(bfc, hash)
    return(list(cache_exists = FALSE, is_up_to_date = FALSE))
  }

  # Get cache info
  cache_info <- bfcinfo(bfc, rid = cached_entry$rid)

  # Convert access_time to POSIXct
  access_time <- ymd_hms(cache_info$access_time)
  current_time <- now()

  # Check if cache is outdated
  if (difftime(current_time, access_time, units = "days") > time_days) {
    message("Cache is outdated.")
    return(list(cache_exists = TRUE, is_up_to_date = FALSE))
  } else {
    message("Cache is valid and up to date.")
    return(list(cache_exists = TRUE, is_up_to_date = TRUE))
  }

}

#' Update the cache entry for API data
#'
#' @description
#' Updates an existing cache entry in BiocFileCache by replacing outdated data with newly fetched data.
#' It first saves the new data to a temporary file, deletes the old cached file if it exists,
#' and then updates the cache entry with the new file.
#'
#' @param path A character string specifying the directory where the cache should be stored.
#' @param bfc A `BiocFileCache` object used to manage cached data.
#' @param hash A character string representing the unique hash for the cached data.
#' @param result_data The API response data to be updated in the cache.
#'
#' @return None. The function updates the data in the cache and prints a message.
#' @import BiocFileCache
#' @keywords internal
update_cache <- function(path, bfc, hash, result_data) {
  message("Updating cache...")

  api_data <- result_data

  # Get existing cache entry
  cached_entry <- bfcquery(bfc, query = hash, field = "rname", exact = TRUE)

  if (nrow(cached_entry) > 0) {
    # Save new data to a file in the cache directory
    cache_file <- tempfile(tmpdir = path)

    #for permanent file
    #cache_file <- file.path(get_cache_location(), paste0(hash, ".rds"))
    saveRDS(api_data, file = cache_file)

    # Delete the old cached file (if it exists)
    if (file.exists(cached_entry$rpath)) {
      file.remove(cached_entry$rpath)
    }

    # Overwrite existing cache entry
    bfc[[cached_entry$rid]] <- cache_file
    message("Cache successfully updated.")
  }
}

#' Read cached data
#'
#' @description
#' Retrieves and loads cached data from the BiocFileCache system using a unique hash.
#' If the cache entry exists, it reads the stored `.rds` file and returns the data.
#'
#' @param bfc A `BiocFileCache` object used to manage cached data.
#' @param hash A character string representing the unique hash for the cached data.
#'
#' @return The cached data if available; otherwise, `NULL` if no valid cache exists.
#' @import BiocFileCache
#' @keywords internal
read_cache <- function(bfc, hash) {
  # Query the cache for the given hash
  cached_entry <- bfcquery(bfc, query = hash, field = "rname", exact = TRUE)

  # Retrieve local file path from cache
  cache_file <- cached_entry$rpath

  if (!file.exists(cache_file)) {
    message("Cache file not found. Returning NULL.")
    return(NULL)
  }

  # Read and return cached data
  message("Loading data from cache...")
  return(readRDS(cache_file))
}

#' Clear cached data
#'
#' @description
#' Removes all cached entries associated with a specific hash from the BiocFileCache system.
#' Deletes the corresponding local cache files and removes their entries from the cache database.
#'
#' @param bfc A `BiocFileCache` object used to manage cached data.
#' @param hash A character string representing the unique hash for the cached data.
#'
#' @return Logical `TRUE` if cache entries were successfully removed.
#' @import BiocFileCache
#' @keywords internal
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
