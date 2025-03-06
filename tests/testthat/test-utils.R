# Capture the real response to be saved to the mock file
capture_requests({
  real_response <- get_request("https://rest.ensembl.org/lookup/symbol/homo_sapiens/BRCA2?")
})

capture_requests({
  error_response <- get_request("https://rest.ensembl.org/lookup/symbol/homo_sapiens/BR?")
})



# From the mock file the response will be loaded
with_mock_api({
  test_that("get_request() returns correct response", {
    response <- get_request("https://rest.ensembl.org/lookup/symbol/homo_sapiens/BRCA2?")

    expect_null(response$error_code)  # No error should occur
    expect_type(response$result, "list")  # The result should be a list
  })
})


with_mock_api({
  test_that("get_request() handles API errors", {

    response <- get_request("https://rest.ensembl.org/lookup/symbol/homo_sapiens/BR?")

    expect_false(is.null(response$error_code))  # Error code should not be NULL
    expect_false(is.null(response$message))     # The error message should not be NULL
    expect_type(response$message, "character")  # The error message should be a string

  })
})


# without_internet({
#   response <- get_request("https://rest.ensembl.org/lookup/symbol/homo_sapiens/BRCA2")
#   print(response)
# })

