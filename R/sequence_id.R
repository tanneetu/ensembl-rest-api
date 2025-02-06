
#' Request multiple types of sequence by stable identifier.
#'
#' @description
#' This function sends a POST request to the Ensembl REST API to retrieve sequence data for given IDs.
#' It processes the API response, converts sequences into appropriate Biostrings objects.
#'
#' @param ... Named arguments representing optional parameters for the API request, including `id`, which is required for retrieving sequences.
#'
#' @return A named list containing Biostrings objects (`DNAStringSet`, `AAStringSet`) for each requested ID.
#'
#' @keywords internal
#' @import httr2 jsonlite Biostrings
#'
post_sequence_id <- function(...) {

  all_params_list<- list(...)

  # Extract 'id' from the list
  if ("id" %in% names(all_params_list)) {
    id_values <- all_params_list[["id"]]
    all_params_list[["id"]] <- NULL  # Remove 'id' from optional parameters
  } else {
    id_values <- NULL
  }

  string_optional_params <- query_string_optional_params(all_params_list)

  endpoint <- "/sequence/id/"

  url <- build_url(endpoint, string_optional_params = string_optional_params)

  # Only 'id' goes inside the body
  body <- list(ids = id_values)

  req <- request(url) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(body, auto_unbox = FALSE)

  req_dry_run(req)

  resp <- req |> req_perform()
  content <- resp_body_json(resp)
  content<- fromJSON(toJSON(content, auto_unbox = TRUE))

  # Split into separate data frames by `query`
  content <- split(x = content, f = as.factor(content$query))

  # Debug purpose
  assign("Temp_Result", content, envir = .GlobalEnv)

  # Uses splitted data frames of API response to create Biostring object
  create_biostring_object <- function(df) {
    id <- as.character(df$id)
    sequence <- as.character(df$seq)
    molecule <- as.character(df$molecule)
    query <- as.character(df$query)

    # Determine molecule type and create the appropriate Biostring object
    if (molecule == "dna") {
      seq_obj <- DNAStringSet(sequence, use.names = TRUE)
    } else if (molecule == "protein") {
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

  # Check the result type
  #str(biostring_objects)

  return(biostring_objects)
}

#' Request multiple types of sequence by stable identifier.
#'
#' @description
#' This function sends a POST request to the Ensembl REST API to retrieve sequence data for given IDs.
#' It first checks if the requested data is available in the cache.
#' If a valid cached entry exists, it returns the cached data.
#' If the cache is outdated or missing, it fetches new data, updates the cache, and returns the latest results.
#'
#' @param ... Named arguments representing optional parameters for the API request, including `id`, which is required for retrieving sequences.
#'
#' @return A named list containing Biostrings objects (`DNAStringSet`, `AAStringSet`) for each requested sequence ID.
#' If caching is enabled, it may return previously stored results instead of making an API request.
#'
#' @import httr2 jsonlite Biostrings BiocFileCache
#' @export
#'
#' @examples
#' # Retrieve a DNA sequence
#' sequence_id(id = "ENSG00000157764")
#'
#' # Retrieve multiple sequences
#' sequence_id(id = c("ENSG00000157764", "ENSP00000288602"))
sequence_id <- function(...) {

  # Initialize BiocFileCache
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  # Extract parameters as a list
  all_params_list <- list(...)

  # Ensure 'id' is included for sequence retrieval and hash creation
  if ("id" %in% names(all_params_list)) {
    id_values <- all_params_list[["id"]]
    print(id_values)
  } else {
    stop("ID values are missing!")
  }

  endpoint <- "/sequence/id/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, ...)

  # Check cache status (TRUE or FALSE)
  cache_status <- check_cache(bfc, hash)

  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    message("Loading data from cache...")
    return(read_cache(bfc, hash))  # Load cached data
  }

  # Fetch new data from API using `post_sequence_id`
  message("Fetching new data from API...")
  result_data <- post_sequence_id(...)

  # Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)
  } else if (!cache_status$cache_exists) {
    create_cache(path, bfc, hash, result_data)
  }
  return(result_data)  # Return fetched data
}
