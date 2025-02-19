#' Retrieve the latest version for a set of identifiers
#'
#' @description
#' This function sends a POST request to the Ensembl REST API to retrieve information for given ID(s).
#'
#' @param id A character vector representing the Ensembl stable ID(s) to be queried.
#'
#' @return A data frame containing the archive details from Ensembl REST API.
#'
#' @import httr2 jsonlite
#' @keywords internal
post_archive_id <-function(id = NULL){

  endpoint <- "/archive/id/"
  url <- build_url(endpoint)

  body <- list(id = id)

  result<- post_request(url,body)

  return(result)
}


#' Retrieve the latest version for a set of identifiers
#'
#' @description
#' This function sends a request to the Ensembl REST API to retrieve information for given IDs.
#' It first checks if the requested data is available in the cache.
#' If a valid cached entry exists, it returns the cached data.
#' If the cache is outdated or missing, it fetches new data, updates the cache, and returns the latest results.
#'
#' @param id A character vector representing the Ensembl stable ID(s) to be queried.
#'
#' @return A data frame containing the archive details from Ensembl REST API.
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
#'
#' More details at \url{https://rest.ensembl.org/documentation/info/archive_id_get}
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

  message("Fetching new data from API...")
  result_data <- post_archive_id(id)

  # Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)
  } else if (!cache_status$cache_exists && !cache_status$is_up_to_date) {
    create_cache(path, bfc, hash, result_data)
  }

  return(result_data)

}
