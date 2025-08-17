# Testing Strategy for Refactored PSM Package

## Overview

The refactored code structure enables much better testing through:
- **Unit tests** for individual functions
- **Integration tests** for complete workflows
- **Performance tests** for regression detection
- **Edge case tests** for robustness

## Current Testing Gaps

### Existing Tests (Good Coverage)
✅ Integration tests for main functions  
✅ Input validation scenarios  
✅ Error handling for invalid inputs  
✅ NMS extension functionality  
✅ Edge cases (missing data, invalid preferences)  

### Missing Tests (Need to Add)
❌ Unit tests for internal functions  
❌ Performance regression tests  
❌ Weighted vs unweighted consistency tests  
❌ Constants and configuration tests  
❌ Error message consistency tests  

## Proposed Testing Structure

```
tests/
├── testthat/
│   ├── test_constants.R              # NEW: Test constants and configuration
│   ├── test_validation.R             # NEW: Test validation utilities
│   ├── test_data_processing.R        # NEW: Test data processing functions
│   ├── test_ecdf_calculations.R      # NEW: Test ECDF calculations
│   ├── test_price_points.R           # NEW: Test price point identification
│   ├── test_nms_analysis.R           # NEW: Test NMS extension
│   ├── test_integration.R            # ENHANCED: Integration tests
│   ├── test_performance.R            # NEW: Performance regression tests
│   ├── test_consistency.R            # NEW: Weighted vs unweighted consistency
│   ├── test_data_input.R             # EXISTING: Keep current tests
│   ├── test_data_input_weighted.R    # EXISTING: Keep current tests
│   ├── test_data_output.R            # EXISTING: Keep current tests
│   └── test_data_output_weighted.R   # EXISTING: Keep current tests
```

## Unit Testing Strategy

### 1. Constants Testing
```r
# Test that constants are properly defined and accessible
test_that("PSM constants are properly defined", {
  expect_true(is.list(PSM_CONSTANTS))
  expect_true("ROUNDING_DIGITS" %in% names(PSM_CONSTANTS))
  expect_equal(get_psm_constant("ROUNDING_DIGITS"), 2L)
})
```

### 2. Validation Function Testing
```r
# Test individual validation functions
test_that("validate_logical_scalar works correctly", {
  expect_silent(validate_logical_scalar(TRUE, "test"))
  expect_error(validate_logical_scalar(c(TRUE, FALSE), "test"))
  expect_error(validate_logical_scalar("yes", "test"))
})
```

### 3. Data Processing Testing
```r
# Test data preparation functions
test_that("prepare_psm_data handles vectors correctly", {
  result <- prepare_psm_data(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  expect_false(result$weighted)
  expect_false(result$nms_requested)
  expect_equal(nrow(result$data), 2)
})
```

### 4. ECDF Calculation Testing
```r
# Test ECDF calculations
test_that("calculate_ecdf_data produces correct structure", {
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  result <- calculate_ecdf_data(psmdata)
  expect_true("price" %in% names(result))
  expect_true("ecdf_cheap" %in% names(result))
  expect_true(all(result$ecdf_cheap >= 0 & result$ecdf_cheap <= 1))
})
```

## Integration Testing Strategy

### 1. End-to-End Workflow Testing
```r
test_that("complete PSM workflow produces consistent results", {
  # Test that refactored functions produce identical results to original
  original_result <- psm_analysis(toocheap = c(1,2), cheap = c(2,3), 
                                 expensive = c(3,4), tooexpensive = c(4,5))
  refactored_result <- psm_analysis_refactored(toocheap = c(1,2), cheap = c(2,3),
                                              expensive = c(3,4), tooexpensive = c(4,5))
  
  expect_equal(original_result$idp, refactored_result$idp)
  expect_equal(original_result$opp, refactored_result$opp)
})
```

### 2. Weighted vs Unweighted Consistency
```r
test_that("weighted analysis with equal weights matches unweighted", {
  # Create data with equal weights
  data <- data.frame(
    tc = c(1,2,3), ch = c(2,3,4), ex = c(3,4,5), te = c(4,5,6),
    weights = c(1,1,1)
  )
  
  # Unweighted analysis
  unweighted <- psm_analysis(data = data, toocheap = "tc", cheap = "ch",
                            expensive = "ex", tooexpensive = "te")
  
  # Weighted analysis with equal weights
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = data)
  weighted <- psm_analysis_weighted(toocheap = "tc", cheap = "ch",
                                   expensive = "ex", tooexpensive = "te", design = design)
  
  expect_equal(unweighted$idp, weighted$idp, tolerance = 0.01)
})
```

## Performance Testing Strategy

### 1. Regression Testing
```r
test_that("refactored functions are not slower than originals", {
  # Generate larger dataset for meaningful timing
  n <- 1000
  data <- data.frame(
    tc = rnorm(n, 5, 1), ch = rnorm(n, 8, 1),
    ex = rnorm(n, 12, 1), te = rnorm(n, 15, 1)
  )
  
  # Time original function
  time_original <- system.time({
    result_original <- psm_analysis(data = data, toocheap = "tc", cheap = "ch",
                                   expensive = "ex", tooexpensive = "te")
  })
  
  # Time refactored function
  time_refactored <- system.time({
    result_refactored <- psm_analysis_refactored(data = data, toocheap = "tc", cheap = "ch",
                                                expensive = "ex", tooexpensive = "te")
  })
  
  # Refactored should not be more than 20% slower
  expect_lt(time_refactored[["elapsed"]], time_original[["elapsed"]] * 1.2)
})
```

### 2. Memory Usage Testing
```r
test_that("refactored functions use reasonable memory", {
  # Test memory usage doesn't explode with larger datasets
  n <- 5000
  data <- data.frame(
    tc = rnorm(n, 5, 1), ch = rnorm(n, 8, 1),
    ex = rnorm(n, 12, 1), te = rnorm(n, 15, 1)
  )
  
  # Monitor memory usage
  gc_before <- gc()
  result <- psm_analysis_refactored(data = data, toocheap = "tc", cheap = "ch",
                                   expensive = "ex", tooexpensive = "te")
  gc_after <- gc()
  
  # Memory usage should be reasonable (less than 100MB for 5k records)
  memory_used <- (gc_after[2,2] - gc_before[2,2]) * 1024^2  # Convert to bytes
  expect_lt(memory_used, 100 * 1024^2)  # Less than 100MB
})
```

## Error Handling Testing Strategy

### 1. Custom Error Classes
```r
test_that("custom error classes work correctly", {
  expect_error(
    validate_logical_scalar("not_logical", "test"),
    class = "psm_input_error"
  )
  
  expect_error(
    validate_price_vectors(c(1,2), c("a","b"), c(3,4), c(4,5)),
    class = "psm_data_error"
  )
})
```

### 2. Error Message Consistency
```r
test_that("error messages are consistent and helpful", {
  # Test that error messages follow consistent format
  expect_error(
    validate_logical_scalar(c(TRUE, FALSE), "validate"),
    "Parameter 'validate' must be a single logical value"
  )
  
  expect_error(
    validate_numeric_scalar("not_numeric", "interpolation_steps"),
    "Parameter 'interpolation_steps' must be a single numeric value"
  )
})
```

## Edge Case Testing Strategy

### 1. Boundary Conditions
```r
test_that("functions handle boundary conditions", {
  # Single observation
  expect_silent(psm_analysis_refactored(
    toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4
  ))
  
  # All identical prices (edge case)
  expect_error(psm_analysis_refactored(
    toocheap = c(2,2), cheap = c(2,2), expensive = c(2,2), tooexpensive = c(2,2)
  ))
})
```

### 2. Missing Data Scenarios
```r
test_that("functions handle missing data correctly", {
  # All toocheap missing
  expect_silent(psm_analysis_refactored(
    toocheap = c(NA, NA), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  ))
  
  # Some missing values in other variables
  expect_warning(psm_analysis_refactored(
    toocheap = c(1, 2), cheap = c(2, NA), 
    expensive = c(3, 4), tooexpensive = c(4, 5),
    validate = FALSE
  ))
})
```

## Test Data Management

### 1. Standardized Test Datasets
```r
# Create standardized test datasets for consistent testing
create_test_data <- function(n = 100, seed = 123) {
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
```

### 2. Test Fixtures
```r
# Create reusable test fixtures
setup_test_environment <- function() {
  list(
    simple_data = create_test_data(10),
    large_data = create_test_data(1000),
    edge_case_data = data.frame(
      toocheap = c(1, NA, 2),
      cheap = c(2, 2, 3),
      expensive = c(3, 3, 4),
      tooexpensive = c(4, 4, 5)
    )
  )
}
```

## Continuous Integration Testing

### 1. Automated Test Execution
- Run all tests on every commit
- Test against multiple R versions (3.5+)
- Test on different operating systems
- Generate coverage reports

### 2. Performance Monitoring
- Track execution time trends
- Alert on significant performance regressions
- Monitor memory usage patterns

## Benefits of Enhanced Testing

### 1. Confidence in Refactoring
- Ensure no functionality is lost during refactoring
- Catch regressions early
- Validate performance improvements

### 2. Maintainability
- Individual functions can be tested in isolation
- Easier to identify source of bugs
- Faster debugging cycle

### 3. Documentation
- Tests serve as executable documentation
- Examples of how functions should be used
- Clear specification of expected behavior

### 4. Quality Assurance
- Consistent error handling
- Proper edge case handling
- Performance guarantees

## Implementation Priority

### Phase 1 (High Priority)
1. Unit tests for validation functions
2. Unit tests for data processing functions
3. Integration tests for backward compatibility

### Phase 2 (Medium Priority)
1. Performance regression tests
2. Weighted vs unweighted consistency tests
3. Enhanced edge case testing

### Phase 3 (Low Priority)
1. Memory usage monitoring
2. Cross-platform testing
3. Stress testing with large datasets

This comprehensive testing strategy ensures that the refactored code maintains all existing functionality while providing better maintainability and performance.
