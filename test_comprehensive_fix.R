# Comprehensive test script to verify ECDF bug fix across all functions
library(pricesensitivitymeter)

# Create test data that triggers the bug
set.seed(123)
tch <- round(rnorm(n = 50, mean = 8.5, sd = 1), digits = 2)
ch <- round(rnorm(n = 50, mean = 10, sd = 1), digits = 2)
ex <- round(rnorm(n = 50, mean = 12, sd = 0.75), digits = 2)
tex <- round(rnorm(n = 50, mean = 15, sd = 1), digits = 2)  # Higher mean to trigger bug

data_psm_test <- data.frame(tch, ch, ex, tex)

cat("=== TESTING ORIGINAL FUNCTIONS ===\n")

# Test original psm_analysis
cat("Testing original psm_analysis...\n")
result_original <- psm_analysis(
  toocheap = "tch",
  cheap = "ch",
  expensive = "ex",
  tooexpensive = "tex",
  data = data_psm_test
)

cat("Original function ECDF max values:\n")
cat("  ecdf_toocheap max:", max(result_original$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), "\n")
cat("  ecdf_cheap max:", max(result_original$data_vanwestendorp$ecdf_cheap), "\n")
cat("  ecdf_expensive max:", max(result_original$data_vanwestendorp$ecdf_expensive), "\n")
cat("  ecdf_tooexpensive max:", max(result_original$data_vanwestendorp$ecdf_tooexpensive), "\n")

# Test original weighted function (if survey package available)
if (requireNamespace("survey", quietly = TRUE)) {
  cat("\nTesting original psm_analysis_weighted...\n")
  data_psm_test$weights <- runif(50, 0.5, 2)
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = data_psm_test)
  
  result_weighted_original <- psm_analysis_weighted(
    toocheap = "tch",
    cheap = "ch",
    expensive = "ex",
    tooexpensive = "tex",
    design = design
  )
  
  cat("Original weighted function ECDF max values:\n")
  cat("  ecdf_toocheap max:", max(result_weighted_original$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), "\n")
  cat("  ecdf_cheap max:", max(result_weighted_original$data_vanwestendorp$ecdf_cheap), "\n")
  cat("  ecdf_expensive max:", max(result_weighted_original$data_vanwestendorp$ecdf_expensive), "\n")
  cat("  ecdf_tooexpensive max:", max(result_weighted_original$data_vanwestendorp$ecdf_tooexpensive), "\n")
}

cat("\n=== TESTING REFACTORED FUNCTIONS ===\n")

# Test refactored psm_analysis
cat("Testing refactored psm_analysis...\n")
result_refactored <- psm_analysis_refactored(
  toocheap = "tch",
  cheap = "ch",
  expensive = "ex",
  tooexpensive = "tex",
  data = data_psm_test
)

cat("Refactored function ECDF max values:\n")
cat("  ecdf_toocheap max:", max(result_refactored$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), "\n")
cat("  ecdf_cheap max:", max(result_refactored$data_vanwestendorp$ecdf_cheap), "\n")
cat("  ecdf_expensive max:", max(result_refactored$data_vanwestendorp$ecdf_expensive), "\n")
cat("  ecdf_tooexpensive max:", max(result_refactored$data_vanwestendorp$ecdf_tooexpensive), "\n")

# Test refactored weighted function (if survey package available)
if (requireNamespace("survey", quietly = TRUE)) {
  cat("\nTesting refactored psm_analysis_weighted...\n")
  
  result_weighted_refactored <- psm_analysis_weighted_refactored(
    toocheap = "tch",
    cheap = "ch",
    expensive = "ex",
    tooexpensive = "tex",
    design = design
  )
  
  cat("Refactored weighted function ECDF max values:\n")
  cat("  ecdf_toocheap max:", max(result_weighted_refactored$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), "\n")
  cat("  ecdf_cheap max:", max(result_weighted_refactored$data_vanwestendorp$ecdf_cheap), "\n")
  cat("  ecdf_expensive max:", max(result_weighted_refactored$data_vanwestendorp$ecdf_expensive), "\n")
  cat("  ecdf_tooexpensive max:", max(result_weighted_refactored$data_vanwestendorp$ecdf_tooexpensive), "\n")
}

cat("\n=== VERIFICATION ===\n")

# Check if all ECDFs reach proper bounds
check_bounds <- function(result, name) {
  all_max_correct <- all(
    max(result$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE) == 1.0,
    max(result$data_vanwestendorp$ecdf_cheap) == 1.0,
    max(result$data_vanwestendorp$ecdf_expensive) == 1.0,
    max(result$data_vanwestendorp$ecdf_tooexpensive) == 1.0
  )
  
  all_min_correct <- all(
    min(result$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE) == 0.0,
    min(result$data_vanwestendorp$ecdf_cheap) == 0.0,
    min(result$data_vanwestendorp$ecdf_expensive) == 0.0,
    min(result$data_vanwestendorp$ecdf_tooexpensive) == 0.0
  )
  
  cat(name, "- All ECDFs reach maximum of 1.0:", all_max_correct, "\n")
  cat(name, "- All ECDFs reach minimum of 0.0:", all_min_correct, "\n")
  
  return(all_max_correct && all_min_correct)
}

original_ok <- check_bounds(result_original, "Original")
refactored_ok <- check_bounds(result_refactored, "Refactored")

if (requireNamespace("survey", quietly = TRUE)) {
  weighted_original_ok <- check_bounds(result_weighted_original, "Weighted Original")
  weighted_refactored_ok <- check_bounds(result_weighted_refactored, "Weighted Refactored")
  
  all_ok <- original_ok && refactored_ok && weighted_original_ok && weighted_refactored_ok
} else {
  all_ok <- original_ok && refactored_ok
}

cat("\n=== FINAL RESULT ===\n")
if (all_ok) {
  cat("✓ SUCCESS: All functions now work correctly with proper ECDF bounds!\n")
} else {
  cat("✗ FAILURE: Some functions still have ECDF boundary issues.\n")
}

# Compare results between original and refactored to ensure consistency
cat("\n=== CONSISTENCY CHECK ===\n")
tolerance <- 1e-10

idp_match <- abs(result_original$idp - result_refactored$idp) < tolerance
opp_match <- abs(result_original$opp - result_refactored$opp) < tolerance
lower_match <- abs(result_original$pricerange_lower - result_refactored$pricerange_lower) < tolerance
upper_match <- abs(result_original$pricerange_upper - result_refactored$pricerange_upper) < tolerance

cat("Original vs Refactored consistency:\n")
cat("  IDP match:", idp_match, "\n")
cat("  OPP match:", opp_match, "\n")
cat("  Price range lower match:", lower_match, "\n")
cat("  Price range upper match:", upper_match, "\n")

consistency_ok <- idp_match && opp_match && lower_match && upper_match
cat("Overall consistency:", consistency_ok, "\n")

if (all_ok && consistency_ok) {
  cat("\n🎉 COMPREHENSIVE FIX SUCCESSFUL! 🎉\n")
  cat("All functions work correctly and consistently.\n")
} else {
  cat("\n❌ Issues remain that need to be addressed.\n")
}
