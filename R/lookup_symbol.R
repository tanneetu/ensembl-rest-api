library(httr2)
library(jsonlite)

build_url <- function(endpoint, mandatory_params, string_optional_params) {

  server <- "https://rest.ensembl.org"

  query_string <- paste(unlist(mandatory_params), collapse = "/")

  base_url <- paste(server, endpoint, query_string,"?", sep = "")

  # Append the optional parameter query string to the base URL
  if (nzchar(string_optional_params)) {
    full_url <- paste(base_url, string_optional_params, sep = "")
  } else {
    full_url <- base_url
  }
  return(full_url)
}

query_string_optional_params <- function(optional_params) {

  # Remove any NULL values from the optional parameters
  optional_params <- optional_params[!sapply(optional_params, is.null)]

  if (length(optional_params) == 0) return("")

  # Create key-value pairs for URL
  query_string <- paste(
    paste(names(optional_params), "=", optional_params, sep=""),
    collapse = ";"
  )
  return(query_string)
}

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

lookup_symbol <- function(species = NULL, symbols = NULL) {

  if (length(species) == 0) {
    stop("Species is missing!")}
  else if (length(symbols) == 0) {
    stop("Symbol is missing!")}
  else{
    return(post_lookup_symbol(species, symbols))}
}

