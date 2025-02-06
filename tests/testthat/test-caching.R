
test_that("create_hash generates consistent hashes", {

  # Test with different values in one argument
  hash1 <- create_hash("/lookup/symbol/", gene = "BRCA1")
  hash2 <- create_hash("/lookup/symbol/", gene = "BRCA2")
  expect_false(hash1 == hash2)  # Different input value should result in different hash

  # Test with multiple values and arguments
  hash3 <- create_hash("/sequence/id/", id = c("ENSG00000157764", "ENSP00000288602"))
  hash4 <- create_hash("/sequence/id/", id = c("ENSG00000157764", "ENSP00000288602"), type="cdna")
  expect_false(hash3 == hash4)  # Optional argument should create different hash

  # Test with multiple named arguments with different order
  hash5 <- create_hash("/lookup/symbol/", gene = c("BRCA1", "TP53"), species = "homo_sapiens")
  hash6 <- create_hash("/lookup/symbol/", species = "homo_sapiens", gene = c("TP53","BRCA1"))
  expect_equal(hash5, hash6)  # Order should not matter
})

test_that("create_hash error handling works correctly", {
  expect_error(create_hash("/lookup/symbol/"), "At least one argument must be provided.")  # No arguments
  expect_error(create_hash("/lookup/symbol/", "BRCA1"), "All arguments must be named.")  # Unnamed argument
})
