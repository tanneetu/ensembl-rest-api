# Implemented Ensembl API Endpoints
# info/species

get_info_species <- function(){

  endpoint <- "/info/species/"
  url <- build_url(endpoint)

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  #req_dry_run(req)

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)

  result<- fromJSON(toJSON(content, auto_unbox = TRUE))

  return(result)

}
