# Run once in the console only when updating the API response
# Capture the API responses to be saved to the mock file
capture_requests({
  # Correct species and correct symbol
  real_response <- lookup_symbol("human", "BRCA2")

  # Correct species and one symbol is wrong (Partially empty list case)
  partial_response <- lookup_symbol("human", c("BRCA2", "BR"))

})

# capture_requests({
#
#   # Wrong species
#   empty_response1 <- lookup_symbol("hum", c("BRCA2", "BRAF"))
#
# })
#
# capture_requests({
#
#   # Correct species but all symbols are wrong
#   empty_response2 <- lookup_symbol("human", c("BRC2", "BR"))
#
# })



# From the mock file the response will be loaded
with_mock_api({

  test_that("lookup_symbol() returns correct response", {
    response <- lookup_symbol("human", "BRCA2")
    expect_type(response, "list")
  })

  test_that("lookup_symbol() handles partially incorrect symbols", {
    expect_message(
      response <- lookup_symbol("human", c("BRCA2", "BR")),
      "Check your input: Some symbols may be incorrect."
    )
    #expect_type(response, "list")
    expect_true("BRCA2" %in% names(response))  # BRCA2 should exist
    expect_false("BR" %in% names(response))  # BR should be missing

  })

})

test_that("lookup_symbol() handles invalid species gracefully", {

  #stub(lookup_symbol, "validate_species_name", function(species) FALSE)

  stub(lookup_symbol, "fetch_data_with_cache", function(...) list())

  expect_error(
    lookup_symbol("hum", c("BRCA2", "BRAF")),
    regexp = "Species name not found!",  # Match only the fixed text
    fixed = TRUE  # Ensure exact match
  )

})


# with_mock_api({
#
#   test_that("lookup_symbol() handles all incorrect symbols", {
#     expect_error(
#       lookup_symbol("human", c("BRC2", "BR")),
#       regexp = "Check your input: Symbols may be incorrect.",
#       fixed = TRUE
#     )
#   })
#
# })
#
# test_that("lookup_symbol() handles invalid species", {
#   expect_error(
#     lookup_symbol("hum", c("BRCA2", "BRAF")),
#     regexp = "Species name not found!",
#     fixed = TRUE  # Ensures exact match for the error message
#   )
# })

# test_that("lookup_symbol() handles all incorrect symbols", {
#   expect_error(
#     lookup_symbol("human", c("BRC2", "BR")),
#     regexp = "Check your input: Symbols may be incorrect.",
#     fixed = TRUE
#   )
# })

