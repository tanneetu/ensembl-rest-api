
test_that("create_hash generates consistent hashes", {

  # Test if the computed hash matches the expected hash
  expected_hash <- "EnsemblRestApi_815d513f224ebcb949c6c44994ab7fd7"
  computed_hash <- create_hash("/lookup/symbol/", gene = "BRCA1")
  expect_equal(computed_hash, expected_hash)

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

test_that("create_cache creates a correct cache entry", {

  bfc <- BiocFileCache( tempfile(), ask = FALSE)

  test_data <- list(a=1, b="hello")
  test_hash <- "EnsemblRestApi_1hello12345"

  create_cache(tempdir(), bfc, test_hash, test_data)

  # Check if cache entry exists
  cache_entry <- bfcquery(bfc, query = test_hash, field = "rname", exact = TRUE)
  expect_true(nrow(cache_entry) == 1)

  # Check if cached data is correct
  cache_file <- bfcpath(bfc, cache_entry$rid)
  cached_data <- readRDS(cache_file)

  expect_equal(cached_data, test_data)
})


test_that("update_cache correctly updates the cache and removes old file", {

  bfc <- BiocFileCache(tempfile(), ask = FALSE)

  initial_data <- list(value = "initial data")
  test_hash <- "EnsemblRestApi_UpdateTest"

  create_cache(tempdir(), bfc, test_hash, initial_data)

  # Get old cache file path before update
  cache_entry_before <- bfcquery(bfc, query = test_hash, field = "rname", exact = TRUE)
  old_cache_file <- bfcpath(bfc, cache_entry_before$rid)

  # Old cache file should exist before update
  expect_true(file.exists(old_cache_file))

  # Update the existing cache entry
  updated_data <- list(value = "updated data")
  update_cache(tempdir(), bfc, test_hash, updated_data)

  # Ensure old cache file is removed
  expect_false(file.exists(old_cache_file))

  cache_entry_after <- bfcquery(bfc, query = test_hash, field = "rname", exact = TRUE)
  new_cache_file <- bfcpath(bfc, cache_entry_after$rid)

  # Verify updated data is correct
  cached_data <- readRDS(new_cache_file)
  expect_equal(cached_data, updated_data)
})




