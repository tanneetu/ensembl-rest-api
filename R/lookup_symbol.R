#' @title Perform POST Request to Ensembl REST API for Symbol Lookup
#'
#' @description
#' Sends a POST request to the Ensembl REST API to look up information for a set of symbols of specific species.
#' It constructs the request URL, sends the query, and returns the parsed response.
#'
#' @param species A character string specifying the species.
#' @param symbols A character vector of one or multiple gene symbols to look up.
#'
#' @return A parsed JSON response containing the lookup results for the specified symbols.
#' If the response is empty, the function returns an empty list.
#'
#' @keywords internal
post_lookup_symbol <- function(species, symbols) {

  endpoint <- "/lookup/symbol/"
  mandatory_params <- species
  url <- build_url(endpoint, mandatory_params)
  body <- list(symbols = symbols)

  result<- post_request(url,body)

  return(result)

}

#' @title Find species-specific database information for given gene symbols from an external linked database.
#'
#' @description
#' Looks up gene symbols in the Ensembl REST API for a specified species.
#'
#' @param species A character string of species name or alias.
#' @param symbols A character vector of one or multiple symbols.
#' A name or symbol from an annotation source has been linked to a genetic feature.
#'
#' @return A list containing the lookup results from the Ensembl REST API.
#' For partially valid symbols, a message is issued, and results are returned for the valid ones.
#'
#' @seealso \url{https://rest.ensembl.org/documentation/info/symbol_lookup}
#'
#' @import BiocFileCache rappdirs
#' @export
#'
#' @examples
#' # Example for an alias and single symbol
#' lookup_symbol(species = "human", symbols = "BRCA2")
#'
#' # Example for a species name and multiple symbols
#' lookup_symbol(species = "homo_sapiens", symbols = c("BRCA2", "BRAF"))
lookup_symbol <- function(species, symbols) {

  if (missing(species) || !nzchar(as.character(species))) {
    stop("Species is missing! Please provide a valid species name.")
  }

  if (missing(symbols) || length(symbols) == 0 || all(symbols == "")) {
    stop("Symbol is missing! Please provide valid symbol name.")
  }

  # Initialize BiocFileCache inside the function
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/lookup/symbol/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, species = species, symbols = symbols)

  result_data <- fetch_data_with_cache(hash, post_lookup_symbol, species = species, symbols = symbols)

  # Handling empty result list
  if (length(result_data) == 0 ) {

    # Validate the species and if invalid, provide error message suggesting similar valid species names
    is_valid_species <- validate_species_name(species)

    # If species is valid, provide error message to check all the symbols
    if(is_valid_species){

      stop("Check your input: Symbols may be incorrect.")
    }
  } else {

    # Handling partially empty list
    missing_symbols <- setdiff(symbols, names(result_data))

    if (length(missing_symbols) > 0) {
      message("Check your input: Some symbols may be incorrect.")
    }
  }

  return(result_data)
}
