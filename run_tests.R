# Script to run comprehensive tests
library(testthat)

# Set working directory
setwd("c:/rstats/pricing-psm/pricesensitivitymeter")

# Load the package
library(pricesensitivitymeter)

# Run all tests
cat("Running comprehensive test suite...\n")
test_results <- test_dir("tests/testthat", reporter = "summary")

# Print results
cat("\n=== TEST RESULTS SUMMARY ===\n")
print(test_results)
