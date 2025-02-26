#' Perform POST Request to Ensembl REST API for Symbol Lookup
#'
#' @description
#' Sends a POST request to the Ensembl REST API to look up information for a set of symbols of specific species.
#' The function analyzes the response, validates the species name, and provides error-checking feedback on invalid inputs.
#'
#' @param species A character string specifying the species. This is a required parameter.
#' @param symbols A character vector of one or multiple gene symbols to look up. This is passed as the body of the POST request.
#'
#' @return A parsed JSON response containing the lookup results for the specified symbols.
#' If the response is empty or partially empty result along with a warning message is delivered.
#'
#' @keywords internal
post_lookup_symbol <- function(species = NULL, symbols = NULL) {

  endpoint <- "/lookup/symbol/"
  mandatory_params <- species
  url <- build_url(endpoint, mandatory_params)
  body <- list(symbols = symbols)

  result<- post_request(url,body)

  return(result)

}


#' Find the species and database for a set of symbols in a linked external database.
#'
#' @description
#' The function constructs the API request, validates the input parameters, and returns the retrieved information as a list.
#' The function supports caching to avoid redundant API calls and improve performance.
#'
#' @param species A character string of species name.
#' @param symbols A character vector of one or multiple symbols.
#'
#' @return A list containing the lookup result from the Ensembl REST API.
#'
#' @import BiocFileCache rappdirs
#' @export
#' @details
#' More details at \url{https://rest.ensembl.org/documentation/info/symbol_lookup}
#'
#' @examples
#' #Example for a single symbol
#' lookup_symbol(species = "homo_sapiens", symbols = "BRCA2")
#'
#' #Example for multiple symbols
#' lookup_symbol(species = "homo_sapiens", symbols = c("BRCA2", "BRAF"))
lookup_symbol <- function(species = NULL, symbols = NULL) {

  if (is.null(species) || !nzchar(as.character(species))) {
    stop("Species is missing!")
  }

  if (is.null(symbols) || length(symbols) == 0 || all(symbols == "")) {
    stop("Symbol is missing!")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/lookup/symbol/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, species = species, symbols = symbols)

  result_data <- fetch_data_with_cache(hash, post_lookup_symbol, species = species, symbols = symbols)

  # If species is invalid, provide suggestion of species names
  if (length(result_data) == 0 && !validate_species_name(species) ) {

    return(result_data)
  }

  missing_symbols <- setdiff(symbols, names(result_data))

  if (length(missing_symbols) > 0) {
    message("Check your input: Some symbols may be incorrect.")
  }

  return(result_data)
}
