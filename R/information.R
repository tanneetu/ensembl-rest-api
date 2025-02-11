# Implemented Ensembl API Endpoints
# info/species

get_info_species <- function(division = NULL, ...){

  if (is.null(division) || length(division) == 0) {
    stop("Division is missing! Please provide a valid division name.")
  }

  # Mapping from usual division names to Ensembl division names
  ensembl_division <- switch(
    division,
    "Metazoa" = "EnsemblMetazoa",
    "Fungi" = "EnsemblFungi",
    "Plants" = "EnsemblPlants",
    "Protists" = "EnsemblProtists",
    "Vertebrates" = "EnsemblVertebrates",
    "Bacteria" = "EnsemblBacteria",
    stop("Invalid division name! Choose from: Metazoa, Fungi, Plants, Protists, Vertebrates, Bacteria")
  )

  print(ensembl_division)

  optional_params_list<- list(division = ensembl_division, ...)

  string_optional_params <- query_string_optional_params(optional_params_list)

  endpoint <- "/info/species/"
  url <- build_url(endpoint, string_optional_params = string_optional_params)

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)

  result<- fromJSON(toJSON(content, auto_unbox = TRUE))

  assign("Check_Result", result, envir = .GlobalEnv)

  species_data <- result$species

  assign("Check_Species", species_data, envir = .GlobalEnv)

  return(species_data)
}

get_info_genomes_genome_name <- function(name){

  species_data <- get_info_species()
  species_name <- species_data$name

  if(!(name %in% species_name)) stop("Genome name is invalid!")

  mandatory_params <- name

  endpoint<-"/info/genomes/"

  url<- build_url(endpoint, mandatory_params = mandatory_params)

  req <- request(url) |>
    req_headers("Accept" = "application/json")

  resp <- req |> req_perform()

  content <- resp_body_json(resp, auto_unbox = FALSE)

  result<- fromJSON(toJSON(content, auto_unbox = TRUE))

  return(result)
}
