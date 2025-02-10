#' Perform POST Request to Ensembl REST API for Symbol Lookup
#'
#' @description
#' Sends a POST request to the Ensembl REST API to look up information for a set of symbols.
#' Separates the optional parameters from the whole parameters list.
#' Right now, no optional parameter is used in this function.
#' Constructs a query URL based on the mandatory and optional parameters and includes the symbols in the request body.
#'
#' @param species A character string specifying the species (e.g., "homo_sapiens"). This is a required parameter.
#' @param symbols A character vector of one or multiple gene symbols to look up. This is passed as the body of the POST request.
#'
#' @return A parsed JSON response containing the lookup results for the specified symbols.
#' @keywords internal
post_lookup_symbol <- function(species = NULL, symbols = NULL) {

  all_params_list <- as.list(environment())

  mandatory_params_name <- c("species")

  check_param <- function(param_value) {

    is.null(param_value) ||!nzchar(as.character(param_value)) ||
      !is.character(param_value)
  }

  is_missing <- sapply(mandatory_params_name, function(param) {
    check_param(all_params_list[[param]])
  })

  if (any(is_missing)) {
    stop("Mandatory parameters are missing!")
  }

  mandatory_params <- all_params_list[mandatory_params_name]
  optional_params_names <- setdiff(names(all_params_list), c(mandatory_params_name, "symbols"))
  optional_params <- all_params_list[optional_params_names]

  string_optional_params <- query_string_optional_params(optional_params)

  endpoint <- "/lookup/symbol/"
  url <- build_url(endpoint, mandatory_params, string_optional_params)

  body <- list(symbols = symbols)

  req <- request(url) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(body, auto_unbox = FALSE)

  #req_dry_run(req)

  resp <- req |> req_perform()

  content <- resp_body_json(resp)

  result <- head(fromJSON(toJSON(content)))

  return(result)
}

#' Find the species and database for a set of symbols in a linked external database.
#'
#' @description
#' The function expects the species and symbol(s) as input.
#' It constructs the API request, validates the input parameters, and returns the retrieved information as a list.
#' It first checks if the requested data is available in the cache.
#' If a valid cached entry exists, it returns the cached data.
#' If the cache is outdated or missing, it fetches new data, updates the cache, and returns the latest results.
#'
#' @param species A character string of species name
#' @param symbols A character vector of one or multiple symbols
#'
#' @return A list containing the lookup result from the Ensembl REST API.
#' If caching is enabled, it may return previously stored results instead of making an API request.
#'
#' @import BiocFileCache rappdirs
#' @export
#'
#' @examples
#' #Example for a single symbol
#' lookup_symbol("homo_sapiens","BRCA2")
#'
#' #Example for multiple symbols
#' lookup_symbol("homo_sapiens", c("BRCA2", "BRAF"))
lookup_symbol <- function(species = NULL, symbols = NULL) {

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  # Error handling for mandatory parameters
  if (length(species) == 0) {
    stop("Species is missing!")
  }
  if (length(symbols) == 0) {
    stop("Symbols are missing!")
  }

  endpoint <- "/lookup/symbol/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, species = species, symbols = symbols)

  # Check cache status (TRUE or FALSE)
  cache_status <- check_cache(bfc, hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    return(read_cache(bfc, hash))  # Load cached data
  }

  # Call post_lookup_symbol() to get both result and URL

  message("Fetching new data from API...")
  result_data <- post_lookup_symbol(species, symbols)

  # Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)  # Update existing cache
  } else if (!cache_status$cache_exists && !cache_status$is_up_to_date) {
    create_cache(path, bfc, hash, result_data)  # Create new cache
  }

  return(result_data)  # Return fetched data
}
