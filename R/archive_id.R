#' Retrieve the latest version for a set of identifiers
#'
#' @description
#' This function sends a POST request to the Ensembl REST API to retrieve information for given ID(s).
#'
#' @param id A character vector representing the ID(s) to be queried.
#'
#' @return A parsed JSON response containing the API response with archive details.
#'
#' @import httr2 jsonlite
#' @keywords internal
post_archive_id <-function(id = NULL){

  endpoint <- "/archive/id/"
  url <- build_url(endpoint)

  body <- list(id = id)

  req <- request(url) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(body, auto_unbox = FALSE)

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)

  result<- fromJSON(toJSON(content, auto_unbox = TRUE))

  return(result)
}


#' Retrieve the latest version for a set of identifiers
#'
#' @description
#' This function sends a POST request to the Ensembl REST API to retrieve information for given IDs.
#' It first checks if the requested data is available in the cache.
#' If a valid cached entry exists, it returns the cached data.
#' If the cache is outdated or missing, it fetches new data, updates the cache, and returns the latest results.
#'
#' @param id A character vector representing the ID(s) to be queried.
#'
#' @return A parsed JSON response containing the API response with archive details.
#'
#' @import BiocFileCache rappdirs
#' @export
#'
#' @examples
#' #Example for a single ID
#' archive_id( id = "ENSG00000157764")
#'
#' #Example for multiple IDs
#' archive_id(id = c("ENSG00000157764","ENSG00000248378"))
archive_id <- function(id = NULL){

  if (is.null(id) || length(id) == 0) {
    stop("ID is missing! Please provide a valid ID.")
  }

  # Initialize BiocFileCache inside the function
  path<- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc<- BiocFileCache(path, ask = FALSE)

  endpoint<- "/archive/id/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, id = id)

  # Check cache status (TRUE or FALSE)
  cache_status <- check_cache(bfc, hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    return(read_cache(bfc, hash))  # Load cached data
  }

  # Call post_archive_id() to get result

  message("Fetching new data from API...")
  result_data <- post_archive_id(id)

  # Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)  # Update existing cache
  } else if (!cache_status$cache_exists && !cache_status$is_up_to_date) {
    create_cache(path, bfc, hash, result_data)  # Create new cache
  }

  return(result_data)

}
