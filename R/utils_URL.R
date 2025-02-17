#' Builds URL for ENSEMBL REST API Endpoints
#'
#' @description
#' This internal function constructs a complete URL for API requests to the Ensembl REST API.
#' It combines the server URL, endpoint, mandatory parameters, and a string of optional parameters if it's not empty.
#'
#' @param endpoint A character string specifying the API endpoint (e.g., "/lookup/symbol/").
#' @param mandatory_params A list of required parameters to be included in the URL path.
#' @param string_optional_params A character string containing optional parameters in "key=value" format, separated by semicolons.
#'
#' @return A complete URL as a character string to be used for API requests.
#' @keywords internal
#' @import  httr2 jsonlite
build_url <- function(endpoint, mandatory_params = NULL, optional_params = NULL) {

  server <- "https://rest.ensembl.org"

  # Handle missing mandatory parameters
  if (is.null(mandatory_params) || all(mandatory_params == "")) {
    query_string <- ""
  } else {
    query_string <- paste(unlist(mandatory_params), collapse = "/")
  }

  base_url <- paste(server, endpoint, query_string, sep = "")

  string_optional_params <- query_string_optional_params(optional_params)

  # Append optional parameters only if they exist
  if (!is.null(string_optional_params) && nzchar(string_optional_params)) {
    full_url <- paste(base_url, "?", string_optional_params, sep="")
  } else {
    full_url <- base_url
  }
  return(full_url)
}

#' Creates a string containing optional parameters
#'
#' @description
#' Creates a query string containing optional parameters in "key=value" format, separated by semicolons.
#' Removes any NULL values from the optional parameters before constructing the string.
#'
#' @param optional_params A named list of optional parameters.
#'
#' @return A character string containing the formatted query parameters or an empty string if no parameter is provided.
#' @keywords internal
query_string_optional_params <- function(optional_params) {

  optional_params <- optional_params[!sapply(optional_params, is.null)]

  if (length(optional_params) == 0) return("")

  query_string <- paste(
    paste(names(optional_params), "=", optional_params, sep=""),
    collapse = ";"
  )

  return(query_string)
}


post_request <- function(url, body) {

  req <- request(url) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(body, auto_unbox = FALSE)

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)  # Parse response

  result <- fromJSON(toJSON(content, auto_unbox = TRUE))  # Convert JSON

  return(result)
}

get_request <-function(url){

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE) # Parse response

  result<- fromJSON(toJSON(content, auto_unbox = TRUE)) # Convert JSON

  return (result)
}
