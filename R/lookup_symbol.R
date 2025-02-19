#' Perform POST Request to Ensembl REST API for Symbol Lookup
#'
#' @description
#' Sends a POST request to the Ensembl REST API to look up information for a set of symbols.
#' Separates the optional parameters from the whole parameters list.
#' Right now, no optional parameter is used in this function.
#' Constructs a query URL based on the mandatory and optional parameters and includes the symbols in the request body.
#'
#' @param species A character string specifying the species. This is a required parameter.
#' @param symbols A character vector of one or multiple gene symbols to look up. This is passed as the body of the POST request.
#'
#' @return A parsed JSON response containing the lookup results for the specified symbols.
#' @keywords internal
post_lookup_symbol <- function(species = NULL, symbols = NULL) {

  all_params_list <- as.list(environment())

  mandatory_params_name <- c("species", "symbols")

  mandatory_params <- all_params_list[mandatory_params_name]
  optional_params_names <- setdiff(names(all_params_list), c(mandatory_params_name))
  optional_params <- all_params_list[optional_params_names]

  endpoint <- "/lookup/symbol/"

  url <- build_url(endpoint, mandatory_params, optional_params)

  body <- list(symbols = symbols)

  result<- post_request(url,body)

  return(result)
}

#' Find the species and database for a set of symbols in a linked external database.
#'
#' @description
#' The function constructs the API request, validates the input parameters, and returns the retrieved information as a list.
#' It expects the species and symbol(s) as input.
#' It first checks if the requested data is available in the cache.
#' If a valid cached entry exists, it returns the cached data.
#' If the cache is outdated or missing, it fetches new data, updates the cache, and returns the latest results.
#'
#' @param species A character string of species name
#' @param symbols A character vector of one or multiple symbols
#'
#' @return A list containing the lookup result from the Ensembl REST API.
#'
#' @import BiocFileCache rappdirs
#' @export
#'
#' @examples
#' #Example for a single symbol
#' lookup_symbol(species = "homo_sapiens", symbols = "BRCA2")
#'
#' #Example for multiple symbols
#' lookup_symbol(species = "homo_sapiens", symbols = c("BRCA2", "BRAF"))
#'
#' More details at \url{https://rest.ensembl.org/documentation/info/symbol_lookup}
lookup_symbol <- function(species = NULL, symbols = NULL) {

  if (is.null(species) || !nzchar(as.character(species))) {
    stop("Species is missing!")
  }

  if (is.null(symbols) || !nzchar(as.character(symbols))) {
    stop("Symbol is missing!")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/lookup/symbol/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, species = species, symbols = symbols)

  # Check cache status (TRUE or FALSE)
  cache_status <- check_cache(bfc, hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    return(read_cache(bfc, hash))  # Load cached data
  }

  message("Fetching new data from API...")
  result_data <- post_lookup_symbol(species, symbols)

  # Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)
  } else if (!cache_status$cache_exists && !cache_status$is_up_to_date) {
    create_cache(path, bfc, hash, result_data)
  }

  return(result_data)
}
