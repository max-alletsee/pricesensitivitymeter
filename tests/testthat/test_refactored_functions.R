context("Refactored PSM Functions")

#----
# Helper functions for testing refactored functions
#----

create_test_data_for_comparison <- function(n = 20, seed = 999) {
  set.seed(seed)
  data.frame(
    toocheap = rnorm(n, 5, 1),
    cheap = rnorm(n, 8, 1),
    expensive = rnorm(n, 12, 1),
    tooexpensive = rnorm(n, 15, 1),
    pi_cheap = sample(1:5, n, replace = TRUE),
    pi_expensive = sample(1:5, n, replace = TRUE),
    weights = runif(n, 0.5, 2)
  )
}

# Function to compare PSM results (allowing for small numerical differences)
compare_psm_results <- function(result1, result2, tolerance = 1e-10) {
  # Compare key numeric results
  expect_equal(result1$idp, result2$idp, tolerance = tolerance)
  expect_equal(result1$opp, result2$opp, tolerance = tolerance)
  expect_equal(result1$pricerange_lower, result2$pricerange_lower, tolerance = tolerance)
  expect_equal(result1$pricerange_upper, result2$pricerange_upper, tolerance = tolerance)
  expect_equal(result1$invalid_cases, result2$invalid_cases)
  expect_equal(result1$total_sample, result2$total_sample)
  expect_equal(result1$weighted, result2$weighted)
  expect_equal(result1$nms, result2$nms)
  
  # Compare data structures
  expect_equal(nrow(result1$data_input), nrow(result2$data_input))
  expect_equal(nrow(result1$data_vanwestendorp), nrow(result2$data_vanwestendorp))
  
  # Compare NMS results if present
  if (result1$nms && result2$nms) {
    expect_equal(result1$price_optimal_reach, result2$price_optimal_reach, tolerance = tolerance)
    expect_equal(result1$price_optimal_revenue, result2$price_optimal_revenue, tolerance = tolerance)
    expect_equal(nrow(result1$data_nms), nrow(result2$data_nms))
  }
}

#----
# Integration with existing workflow tests
#----

test_that("refactored functions work with existing summary and plot methods", {
  test_data <- create_test_data_for_comparison(10)
  
  # Test that refactored result works with existing methods
  refactored_result <- psm_analysis_refactored(
    data = test_data, toocheap = "toocheap", cheap = "cheap",
    expensive = "expensive", tooexpensive = "tooexpensive"
  )
  
  # Should work with existing summary method and produce output
  expect_output(summary(refactored_result), "Van Westendorp Price Sensitivity Meter Analysis")
  
  # Should work with existing plot method
  expect_silent(psm_plot(refactored_result))
  
  # Should also work with refactored plot method
  expect_silent(psm_plot_refactored(refactored_result))
})