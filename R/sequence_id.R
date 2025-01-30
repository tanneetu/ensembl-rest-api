
post_sequence_id <- function(..., output_dir = "fasta_files") {

  all_params_list<- list(...)

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Extract 'id' from the list (if provided)
  if ("id" %in% names(all_params_list)) {
    id_values <- all_params_list[["id"]]
    all_params_list[["id"]] <- NULL  # Remove 'id' from optional parameters
  } else {
    id_values <- NULL
  }

  string_optional_params <- query_string_optional_params(all_params_list)

  endpoint <- "/sequence/id/"

  url <- build_url(endpoint, string_optional_params = string_optional_params)

  # Only 'id' needs to go inside the body
  body <- list(ids = id_values)

  req <- request(url) |>
    req_headers("Accept" = "text/x-fasta") |>
    req_body_json(body, auto_unbox = FALSE)

  req_dry_run(req)

  resp <- req |> req_perform()

  if ("content_type" %in% names(all_params_list) &&
      !is.null(all_params_list[["content_type"]]) &&
      all_params_list[["content_type"]] == "text/x-fasta") {
    content <- resp_body_string(resp)  # Keep as plain text

    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_name <- paste0("output_", timestamp, ".fasta")
    file_path <- file.path(output_dir, file_name)

    # Save to file
    writeLines(content, file_path)

    message("FASTA file saved as: ", file_path)
    return(file_path)
  } else {
    content <- resp_body_json(resp)  # Parse JSON normally
    content <- head(fromJSON(toJSON(content)))
  }
  return(content)
}
