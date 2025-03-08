#' @title Request multiple types of sequence by stable identifier.
#'
#' @description
#' This function sends a POST request to the Ensembl REST API to retrieve sequence data for given IDs.
#' It processes the API response, converts sequences into appropriate Biostrings objects.
#'
#' @param id A character vector representing the Ensembl stable ID.
#' @param ... Named arguments representing optional parameters for the API request.
#'
#' @return A named list containing Biostrings objects (`DNAStringSet`, `AAStringSet`) for each requested ID.
#'
#' @import Biostrings
#' @keywords internal
post_sequence_id <- function(id, ...) {

  optional_params<- list(...)

  endpoint <- "/sequence/id/"

  url <- build_url(endpoint, optional_params = optional_params)

  body <- list(ids = id)

  content<- post_request(url, body)

  # Split into separate data frames by `query`
  content <- split(x = content, f = as.factor(content$query))

  # Uses splitted data frames of API response to create Biostring object
  create_biostring_object <- function(df) {
    id <- as.character(df$id)
    sequence <- as.character(df$seq)
    molecule <- as.character(df$molecule)
    query <- as.character(df$query)

    # Determine molecule type and create the appropriate Biostring object
    if ("dna" %in% molecule) {
      seq_obj <- DNAStringSet(sequence, use.names = TRUE)
    } else if ("protein" %in% molecule) {
      seq_obj <- AAStringSet(sequence, use.names = TRUE)
    } else {
      stop("Unknown type detected!")
    }

    # Assign unique names
    names(seq_obj) <- paste0(query, "|", id)

    message("Biostring Object has been created!")

    return(seq_obj)
  }

  # Create the biostring objects from the lists of API result
  biostring_objects <- lapply(content, create_biostring_object)

  return(biostring_objects)
}

#' @title Request multiple types of sequence by stable identifier.
#'
#' @description
#' This function sends a request to the Ensembl REST API to retrieve sequence data for given IDs.
#'
#' @param id A character vector representing the Ensembl stable ID.
#' @param ... Named arguments representing optional parameters for the API request.
#'
#' @return A named list containing Biostrings objects (`DNAStringSet`, `AAStringSet`) for each requested sequence ID.
#'
#' @seealso \url{https://rest.ensembl.org/documentation/info/sequence_id}
#'
#' @import BiocFileCache rappdirs
#' @export
#' @examples
#' # Retrieve a DNA sequence
#' sequence_id(id = "ENSG00000157764",type = "genomic")
#'
#' # Retrieve multiple sequences
#' sequence_id(id = c("ENSG00000157764", "ENSP00000288602"))
sequence_id <- function(id, ...) {

  if (missing(id) || length(id) == 0 || all(id == "")) {
    stop("ID is missing! Please provide a valid ID.")
  }

  # Initialize BiocFileCache
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  endpoint <- "/sequence/id/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, id = id, ...)

  result_data <- fetch_data_with_cache(hash, post_sequence_id, id = id, ...)

  return(result_data)
}
