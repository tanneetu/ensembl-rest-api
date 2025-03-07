#' @title Builds URL for ENSEMBL REST API Endpoints
#'
#' @description
#' This internal function constructs a complete URL for API requests to the Ensembl REST API.
#' It combines the server URL, endpoint, mandatory parameters, and optional parameters if it's not empty.
#' It also rectifies a malformed URL that contains spaces.
#'
#' @param endpoint A character string specifying the API endpoint (e.g., "/lookup/symbol/").
#' @param mandatory_params A list of required parameters to be included in the URL path.
#' @param optional_params A named list of optional parameters.
#'
#' @return A complete URL as a character string to be used for API requests.
#' @import  httr2
#' @keywords internal
build_url <- function(endpoint, mandatory_params = NULL, optional_params = NULL) {

  server <- "https://rest.ensembl.org"

  # Handle missing mandatory parameters
  if (is.null(mandatory_params) || all(mandatory_params == "")) {
    query_string <- ""
  } else {
    query_string <- paste(unlist(mandatory_params), collapse = "/")
  }

  base_url <- paste(server, endpoint, query_string, sep = "")

  # Clean optional_parameters: remove NULL and empty values
  optional_params <- optional_params[unlist(lapply(optional_params, function(x) !is.null(x) && nzchar(as.character(x))))]

  string_optional_params <- query_string_optional_params(optional_params)

  # Append optional parameters only if they exist
  if (!is.null(string_optional_params) && nzchar(string_optional_params)) {
    full_url <- paste0(base_url, "?", string_optional_params)
  } else {
    full_url <- base_url
  }

  #"https://rest.ensembl.org/info/assembly/silver fox?"

  clean_url <- gsub(" ", "%20",full_url)

  print(clean_url)

  return(clean_url)
}

#' @title Creates a query string containing optional parameters
#'
#' @description
#' Creates a query string containing optional parameters in "key=value" format, separated by semicolons.
#'
#' @param optional_params A named list of optional parameters.
#'
#' @return A character string containing the formatted query parameters or an empty string if no parameter is provided.
#' @keywords internal
query_string_optional_params <- function(optional_params) {

  if (length(optional_params) == 0) return("")

  query_string <- paste(names(optional_params), optional_params, sep = "=", collapse = ";")

  return(query_string)
}

#' @title Perform a POST Request to an API
#'
#' @description
#' Sends a POST request to the specified URL with a JSON-formatted body.
#' It includes appropriate headers and parses the response into a structured format.
#'
#' @param url A character string specifying the API endpoint.
#' @param body A list containing the data to be sent in the request body.
#'
#' @return A structured response object parsed from JSON.
#' @import  httr2 jsonlite
#' @keywords internal
post_request <- function(url, body) {

  if(length(unlist(body)) > 1000){
    stop("Request body exceeds the maximum allowed size of 1000 elements. Please reduce the input size")
  }

  print(body)

  req <- request(url) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(body, auto_unbox = FALSE)

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)  # Parse response

  result <- fromJSON(toJSON(content, auto_unbox = TRUE))  # Convert JSON

  return(result)
}

#' @title Perform a GET Request to an API
#'
#' @description
#' Sends a GET request to the specified URL and retrieves data in JSON format and returns a structured response.
#' If the request fails, an error code and message are returned instead.
#'
#' @param url A character string specifying the API endpoint.
#'
#' @return A list containing either the parsed JSON response or an error message with an HTTP status code.
#'
#' @import  httr2 jsonlite
#' @keywords internal
get_request <-function(url){

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  # Suppress the error and continue to execute
  resp <- req |>
    req_error(is_error = \(resp) FALSE) |>
    req_perform()

  #print(resp)

  status_code <- resp_status(resp)

  if (status_code != 200) {
    return(list(error_code = status_code, message = resp_body_string(resp)))
  }

  content <- resp_body_json(resp, auto_unbox = FALSE) # Parse response

  result<- fromJSON(toJSON(content, auto_unbox = TRUE)) # Convert JSON

  return(list(error_code = NULL, result = result))

  #"https://rest.ensembl.org/lookup/symbol/homo_sapiens/BRCA2?"
}

#' @title Fetch Data with Caching Mechanism
#'
#' @description
#' This internal function handles fetching data from an API while implementing caching.
#' It first checks if the requested data is available in the cache.
#' If cached data is up to date, it returns the cached data; otherwise, it fetches new data,
#' updates the cache, and returns the latest results.
#'
#' @param hash A unique identifier for caching the fetched data.
#' @param fetch_function A function that retrieves the required data when the cache is outdated or missing.
#' @param ... Additional arguments to be passed to the fetch function.
#'
#' @return The fetched data, either from cache or a fresh API request.
#' @import BiocFileCache rappdirs
#' @keywords internal
fetch_data_with_cache <- function(hash, fetch_function, ...) {

  # Initialize BiocFileCache
  path <- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc <- BiocFileCache(path, ask = FALSE)

  # Check cache status
  cache_status <- check_cache(bfc, hash)

  # If data exists and is up to date, return cached data
  if (cache_status$cache_exists && cache_status$is_up_to_date) {
    return(read_cache(bfc, hash))
  }

  message("Fetching new data from API...")
  result_data <- fetch_function(...)

  # # For valid get request response, cache only the result
  # if(is.null(result_data$error_code)){
  #
  #   result<-result_data$result
  #
  #   # Cache decision based on status
  #   if (cache_status$cache_exists && !cache_status$is_up_to_date) {
  #     update_cache(path, bfc, hash, result)
  #   } else if (!cache_status$cache_exists) {
  #     create_cache(path, bfc, hash, result)
  #   }
  #
  #   # Return the original get request response
  #   return(result_data)
  # }

  # Cache decision based on status
  if (cache_status$cache_exists && !cache_status$is_up_to_date) {
    update_cache(path, bfc, hash, result_data)
  } else if (!cache_status$cache_exists) {
    create_cache(path, bfc, hash, result_data)
  }

  return(result_data)
}
