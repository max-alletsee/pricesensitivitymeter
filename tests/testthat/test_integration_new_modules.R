context("Integration Tests for New PSM Modules")

#----
# Helper functions for integration testing
#----

create_integration_test_data <- function(n = 20, seed = 456) {
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

#----
# Integration tests: Constants + Validation
#----

test_that("constants and validation work together", {
  # Constants should be used in validation
  expect_silent(validate_method_choice(
    "min", 
    get_psm_constant("VALID_INTERSECTION_METHODS"), 
    "test_method"
  ))
  
  expect_silent(validate_method_choice(
    "original", 
    get_psm_constant("VALID_ACCEPTABLE_RANGES"), 
    "test_range"
  ))
  
  # Invalid values should still be caught
  expect_error(
    validate_method_choice(
      "invalid", 
      get_psm_constant("VALID_INTERSECTION_METHODS"), 
      "test_method"
    ),
    class = "psm_input_error"
  )
})

test_that("constants are used consistently in validation", {
  # Test that default values from constants work in validation
  expect_silent(validate_numeric_scalar(
    get_psm_constant("DEFAULT_INTERPOLATION_STEPS"), 
    "interpolation_steps"
  ))
  
  expect_silent(validate_numeric_scalar(
    get_psm_constant("ROUNDING_DIGITS"), 
    "rounding_digits"
  ))
  
  # NMS defaults should be valid
  nms_defaults <- get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED")
  expect_silent(validate_calibration_values(
    nms_defaults, 
    get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOCHEAP"),
    get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOEXPENSIVE")
  ))
})

#----
# Integration tests: Validation + Data Processing
#----

test_that("validation and data processing work together", {
  # Valid data should pass through validation and processing
  result <- prepare_psm_data(
    toocheap = c(1, 2, 3), cheap = c(2, 3, 4), 
    expensive = c(3, 4, 5), tooexpensive = c(4, 5, 6),
    validate = TRUE
  )
  
  expect_equal(result$invalid_cases, 0)
  expect_equal(result$total_sample, 3)
  expect_equal(nrow(result$data), 3)
  
  # Invalid data should be detected and handled
  result_invalid <- prepare_psm_data(
    toocheap = c(4, 2, 3), cheap = c(3, 3, 4), 
    expensive = c(2, 4, 5), tooexpensive = c(1, 5, 6),
    validate = FALSE
  )
  
  expect_gt(result_invalid$invalid_cases, 0)
})

test_that("validation errors propagate correctly through data processing", {
  # Invalid input should cause validation error before data processing
  expect_error(
    prepare_psm_data(
      toocheap = "not_numeric", cheap = c(2, 3), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    class = "psm_data_error"
  )
})

#----
# Integration tests: Constants + Data Processing
#----

test_that("constants are used correctly in data processing", {
  # Test that default constants are used
  test_data <- create_integration_test_data(10)
  
  result <- prepare_psm_data(
    toocheap = test_data$toocheap, cheap = test_data$cheap,
    expensive = test_data$expensive, tooexpensive = test_data$tooexpensive,
    pi_cheap = test_data$pi_cheap, pi_expensive = test_data$pi_expensive
  )
  
  # Should use default NMS constants
  expect_true(result$nms_requested)
  expect_true("pi_cheap_cal" %in% names(result$data))
  
  # Calibrated values should match default constants
  expected_calibration <- get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED")
  pi_scale <- get_psm_constant("NMS_DEFAULTS.PI_SCALE")
  
  # Check that calibration used correct constants
  for (i in seq_along(pi_scale)) {
    matching_rows <- which(result$data$pi_cheap == pi_scale[i])
    if (length(matching_rows) > 0) {
      expect_equal(
        unique(result$data$pi_cheap_cal[matching_rows]), 
        expected_calibration[i]
      )
    }
  }
})

test_that("ECDF calculation uses constants correctly", {
  test_data <- create_integration_test_data(10)
  
  # Prepare data
  prepared_data <- prepare_psm_data(
    toocheap = test_data$toocheap, cheap = test_data$cheap,
    expensive = test_data$expensive, tooexpensive = test_data$tooexpensive
  )
  
  # Calculate ECDF with interpolation using default constants
  ecdf_result <- calculate_ecdf_data(
    prepared_data$data,
    interpolate = TRUE,
    interpolation_steps = get_psm_constant("DEFAULT_INTERPOLATION_STEPS")
  )
  
  expect_true(is.data.frame(ecdf_result))
  expect_gt(nrow(ecdf_result), 4)  # Should have more rows due to interpolation
})

#----
# Integration tests: All modules together
#----

test_that("complete workflow integration works correctly", {
  test_data <- create_integration_test_data(15)
  
  # Step 1: Validation (should use constants)
  validation_result <- validate_psm_inputs(
    toocheap = test_data$toocheap, cheap = test_data$cheap,
    expensive = test_data$expensive, tooexpensive = test_data$tooexpensive,
    pi_cheap = test_data$pi_cheap, pi_expensive = test_data$pi_expensive,
    intersection_method = "min",  # From constants
    acceptable_range = "original"  # From constants
  )
  
  expect_true(validation_result$nms_requested)
  
  # Step 2: Data preparation
  prepared_data <- prepare_psm_data(
    toocheap = test_data$toocheap, cheap = test_data$cheap,
    expensive = test_data$expensive, tooexpensive = test_data$tooexpensive,
    pi_cheap = test_data$pi_cheap, pi_expensive = test_data$pi_expensive,
    validate = TRUE
  )
  
  expect_true(prepared_data$nms_requested)
  expect_lte(prepared_data$invalid_cases, prepared_data$total_sample)
  
  # Step 3: ECDF calculation
  ecdf_data <- calculate_ecdf_data(
    prepared_data$data,
    weighted = prepared_data$weighted,
    survey_design = prepared_data$survey_design,
    interpolate = TRUE
  )
  
  expect_true(is.data.frame(ecdf_data))
  expect_true(all(c("price", "ecdf_cheap", "ecdf_expensive") %in% names(ecdf_data)))
  
  # Step 4: Price point identification
  price_points <- identify_price_points(
    ecdf_data,
    intersection_method = "min",
    acceptable_range = "original"
  )
  
  expect_true(is.list(price_points))
  expect_true(all(c("idp", "opp", "pricerange_lower", "pricerange_upper") %in% names(price_points)))
  
  # Step 5: NMS analysis (if requested)
  if (prepared_data$nms_requested) {
    nms_results <- calculate_nms_analysis(prepared_data, ecdf_data)
    
    expect_true(is.list(nms_results))
    expect_true(all(c("data_nms", "price_optimal_reach", "price_optimal_revenue") %in% names(nms_results)))
  }
})

test_that("weighted analysis integration works correctly", {
  skip_if_not_installed("survey")
  
  test_data <- create_integration_test_data(12)
  
  # Create survey design
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = test_data)
  
  # Step 1: Validation with survey design
  validation_result <- validate_psm_inputs(
    toocheap = "toocheap", cheap = "cheap",
    expensive = "expensive", tooexpensive = "tooexpensive",
    pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
    design = design
  )
  
  expect_true(validation_result$nms_requested)
  
  # Step 2: Data preparation with survey design
  prepared_data <- prepare_psm_data(
    toocheap = "toocheap", cheap = "cheap",
    expensive = "expensive", tooexpensive = "tooexpensive",
    pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
    design = design,
    validate = TRUE
  )
  
  expect_true(prepared_data$weighted)
  expect_true(prepared_data$nms_requested)
  expect_true(inherits(prepared_data$survey_design, "survey.design"))
  
  # Step 3: Weighted ECDF calculation
  ecdf_data <- calculate_ecdf_data(
    prepared_data$data,
    weighted = prepared_data$weighted,
    survey_design = prepared_data$survey_design
  )
  
  expect_true(is.data.frame(ecdf_data))
  
  # Step 4: NMS analysis with weights
  nms_results <- calculate_nms_analysis(prepared_data, ecdf_data)
  
  expect_true(is.list(nms_results))
  expect_true(is.data.frame(nms_results$data_nms))
})

#----
# Performance integration tests
#----

test_that("integrated workflow has reasonable performance", {
  # Test with moderately large dataset
  test_data <- create_integration_test_data(100)
  
  start_time <- Sys.time()
  
  # Complete workflow
  prepared_data <- prepare_psm_data(
    toocheap = test_data$toocheap, cheap = test_data$cheap,
    expensive = test_data$expensive, tooexpensive = test_data$tooexpensive,
    pi_cheap = test_data$pi_cheap, pi_expensive = test_data$pi_expensive,
    validate = TRUE
  )
  
  ecdf_data <- calculate_ecdf_data(
    prepared_data$data,
    interpolate = TRUE
  )
  
  price_points <- identify_price_points(ecdf_data)
  
  if (prepared_data$nms_requested) {
    nms_results <- calculate_nms_analysis(prepared_data, ecdf_data)
  }
  
  end_time <- Sys.time()
  execution_time <- as.numeric(end_time - start_time)
  
  # Should complete in reasonable time (less than 2 seconds for 100 observations)
  expect_lt(execution_time, 2)
})

test_that("memory usage is reasonable for integrated workflow", {
  # Test memory usage doesn't explode
  test_data <- create_integration_test_data(50)
  
  gc_before <- gc()
  
  # Run complete workflow
  prepared_data <- prepare_psm_data(
    toocheap = test_data$toocheap, cheap = test_data$cheap,
    expensive = test_data$expensive, tooexpensive = test_data$tooexpensive,
    pi_cheap = test_data$pi_cheap, pi_expensive = test_data$pi_expensive,
    validate = TRUE
  )
  
  ecdf_data <- calculate_ecdf_data(prepared_data$data, interpolate = TRUE)
  price_points <- identify_price_points(ecdf_data)
  nms_results <- calculate_nms_analysis(prepared_data, ecdf_data)
  
  gc_after <- gc()
  
  # Memory usage should be reasonable (less than 50MB for 50 records)
  memory_used <- (gc_after[2,2] - gc_before[2,2]) * 1024^2  # Convert to bytes
  expect_lt(memory_used, 50 * 1024^2)  # Less than 50MB
})

#----
# Error propagation tests
#----

test_that("errors propagate correctly through integrated workflow", {
  # Error in validation should stop workflow
  expect_error(
    {
      validation_result <- validate_psm_inputs(
        toocheap = c(1, 2), cheap = c(2, 3),
        expensive = c(3, 4), tooexpensive = c(4, 5),
        intersection_method = "invalid"  # Should cause error
      )
    },
    class = "psm_input_error"
  )
  
  # Error in data preparation should stop workflow
  expect_error(
    {
      prepared_data <- prepare_psm_data(
        toocheap = c(5, 5), cheap = c(4, 4),  # Invalid preferences
        expensive = c(3, 3), tooexpensive = c(2, 2),
        validate = TRUE
      )
    },
    "All respondents have intransitive preference structures"
  )
})

#----
# Consistency tests
#----

test_that("results are consistent across multiple runs", {
  # Same input should produce same output
  test_data <- create_integration_test_data(20, seed = 789)
  
  run_workflow <- function() {
    prepared_data <- prepare_psm_data(
      toocheap = test_data$toocheap, cheap = test_data$cheap,
      expensive = test_data$expensive, tooexpensive = test_data$tooexpensive,
      validate = TRUE
    )
    
    ecdf_data <- calculate_ecdf_data(prepared_data$data)
    price_points <- identify_price_points(ecdf_data)
    
    return(price_points)
  }
  
  result1 <- run_workflow()
  result2 <- run_workflow()
  
  expect_equal(result1$idp, result2$idp)
  expect_equal(result1$opp, result2$opp)
  expect_equal(result1$pricerange_lower, result2$pricerange_lower)
  expect_equal(result1$pricerange_upper, result2$pricerange_upper)
})

test_that("constants remain unchanged during workflow", {
  # Store original constant values
  original_rounding <- get_psm_constant("ROUNDING_DIGITS")
  original_interpolation <- get_psm_constant("DEFAULT_INTERPOLATION_STEPS")
  original_methods <- get_psm_constant("VALID_INTERSECTION_METHODS")
  
  # Run workflow
  test_data <- create_integration_test_data(10)
  
  prepared_data <- prepare_psm_data(
    toocheap = test_data$toocheap, cheap = test_data$cheap,
    expensive = test_data$expensive, tooexpensive = test_data$tooexpensive
  )
  
  ecdf_data <- calculate_ecdf_data(prepared_data$data)
  price_points <- identify_price_points(ecdf_data)
  
  # Constants should be unchanged
  expect_equal(get_psm_constant("ROUNDING_DIGITS"), original_rounding)
  expect_equal(get_psm_constant("DEFAULT_INTERPOLATION_STEPS"), original_interpolation)
  expect_equal(get_psm_constant("VALID_INTERSECTION_METHODS"), original_methods)
})

#----
# Edge case integration tests
#----

test_that("integrated workflow handles edge cases", {
  # Single observation
  result_single <- prepare_psm_data(
    toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4
  )
  
  ecdf_single <- calculate_ecdf_data(result_single$data)
  points_single <- identify_price_points(ecdf_single)
  
  expect_true(is.list(points_single))
  expect_true(all(sapply(points_single, is.numeric)))
  
  # All toocheap missing
  result_no_toocheap <- prepare_psm_data(
    toocheap = c(NA, NA), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  ecdf_no_toocheap <- calculate_ecdf_data(result_no_toocheap$data)
  points_no_toocheap <- identify_price_points(ecdf_no_toocheap)
  
  expect_true(is.list(points_no_toocheap))
  # Some price points may be NA when toocheap is missing
})

test_that("workflow handles mixed valid/invalid data correctly", {
  # Mix of valid and invalid preferences
  result_mixed <- prepare_psm_data(
    toocheap = c(1, 4, 2), cheap = c(2, 3, 3),
    expensive = c(3, 2, 4), tooexpensive = c(4, 1, 5),
    validate = TRUE
  )
  
  expect_gt(result_mixed$invalid_cases, 0)
  expect_lt(nrow(result_mixed$data), result_mixed$total_sample)
  
  # Should still be able to complete workflow with valid cases
  ecdf_mixed <- calculate_ecdf_data(result_mixed$data)
  points_mixed <- identify_price_points(ecdf_mixed)
  
  expect_true(is.list(points_mixed))
})
