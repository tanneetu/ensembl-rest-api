#' Builds URL for ENSEMBL REST API Endpoints
#'
#' @description
#' This internal function constructs a complete URL for API requests to the Ensembl REST API.
#' It combines the server URL, endpoint, mandatory parameters, and optional parameters if it's not empty.
#'
#' @param endpoint A character string specifying the API endpoint (e.g., "/lookup/symbol/").
#' @param mandatory_params A list of required parameters to be included in the URL path.
#' @param optional_params A named list of optional parameters.
#'
#' @return A complete URL as a character string to be used for API requests.
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
  print(full_url)
  return(full_url)
}

#' Creates a string containing optional parameters
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

#' Perform a POST Request to an API
#'
#' @description
#' Sends a POST request to the specified URL with a JSON-formatted body.
#' It includes appropriate headers and parses the response into a structured format.
#'
#' @param url A character string specifying the API endpoint.
#' @param body A list containing the data to be sent in the request body.
#'
#' @return A structured response object parsed from JSON.
#' @keywords internal
#' @import  httr2 jsonlite
post_request <- function(url, body) {

  req <- request(url) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(body, auto_unbox = FALSE)

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)  # Parse response

  result <- fromJSON(toJSON(content, auto_unbox = TRUE))  # Convert JSON

  return(result)
}

#' Perform a GET Request to an API
#'
#' @description
#' Sends a GET request to the specified URL and retrieves data in JSON format.
#'
#' @param url A character string specifying the API endpoint.
#'
#' @return A structured response object parsed from JSON.
#' @keywords internal
#' @import  httr2 jsonlite
get_request <-function(url){

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE) # Parse response

  result<- fromJSON(toJSON(content, auto_unbox = TRUE)) # Convert JSON

  return (result)
}
