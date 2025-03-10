get_xref_id <- function(id, ...){

  mandatory_params <- id
  optional_params <- list(...)

  endpoint <- "/xrefs/id/"

  url <- build_url(endpoint, mandatory_params, optional_params)

  response <- get_request(url)

  result <- response$result

  return(result)

}

xref_id <- function(id, ...){

  if(missing(id) || length(id) == 0){
    stop("ID is missing! Please provide a valid ID.")
  }

  # Initialize BiocFileCache inside the function
  path<- rappdirs::user_cache_dir(appname = "EnsemblRestApiCache")
  bfc<- BiocFileCache(path, ask = FALSE)

  endpoint <- "/xrefs/id/"

  # Create unique hash for caching
  hash <- create_hash(endpoint, id=id, ...)

  result_data <- fetch_data_with_cache(hash, get_xref_id, id = id, ...)

  return(result_data)
}
