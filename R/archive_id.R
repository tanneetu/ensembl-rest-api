get_archive_id<-function(id = NULL){

  endpoint <- "/archive/id/"

  mandatory_params <- id

  url <- build_url(endpoint, mandatory_params = mandatory_params)

  req <- request(url) |>
      req_headers("Accept" = "application/json")
  resp <- req |> req_perform()
  content <- resp_body_json(resp)

  result <- head(fromJSON(toJSON(content)))

  return(result)
}

post_archive_id <-function(...){

  all_params_list <- list(...)

  # Extract 'id' from the list
  if ("id" %in% names(all_params_list)) {
    id_values <- all_params_list[["id"]]
    all_params_list[["id"]] <- NULL  # Remove 'id' from optional parameters
  }else {
    id_values <- NULL
  }

  endpoint <- "/archive/id/"
  url <- build_url(endpoint)

  body <- list(id = id_values)

  req <- request(url) |>
    req_headers("Accept" = "application/json") |>
    req_body_json(body, auto_unbox = FALSE)

  req_dry_run(req)

  resp <- req |> req_perform()

  content <- resp_body_json(resp)

  result <- head(fromJSON(toJSON(content)))

  return(result)
}
