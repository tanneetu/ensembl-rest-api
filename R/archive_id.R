#' @title Retrieve the latest version for a set of identifiers
#'
#' @description
#' This function sends a POST request to the Ensembl REST API to retrieve information for given ID(s).
#'
#' @param id A character vector representing one or more Ensembl stable ID to be queried.
#'
#' @return A data frame containing the archive details from Ensembl REST API.
#'
#' @keywords internal
post_archive_id <-function(id){

  endpoint <- "/archive/id/"
  url <- build_url(endpoint)

  body <- list(id = id)

  result<- post_request(url,body)

  return(result)
}


#' @title Retrieve the latest version for a set of identifiers
#'
#' @description
#' This function sends a request to the Ensembl REST API to retrieve information for given IDs.
#'
#' @param id A character vector representing one or more Ensembl stable ID to be queried.
#'
#' @return A list containing the archive details from Ensembl REST API.
#'
#' @seealso \url{https://rest.ensembl.org/documentation/info/archive_id_get}
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
archive_id <- function(id){

  if (missing(id) || length(id) == 0 || all(id == "")) {
    stop("ID is missing! Please provide a valid ID.")
  }

  # Initialize BiocFileCache inside the function
  path<- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc<- BiocFileCache(path, ask = FALSE)

  endpoint<- "/archive/id/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, id = id)

  result_data <- fetch_data_with_cache(hash, post_archive_id, id=id)

  return(result_data)

}
