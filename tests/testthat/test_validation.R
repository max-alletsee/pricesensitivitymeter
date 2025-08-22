context("PSM Validation Functions")

#----
# Tests for custom error classes
#----

test_that("custom error classes work correctly", {
  # Test psm_input_error
  input_error <- psm_input_error("test input error")
  expect_s3_class(input_error, "psm_input_error")
  expect_s3_class(input_error, "error")
  expect_s3_class(input_error, "condition")
  expect_equal(input_error$message, "test input error")
  
  # Test psm_data_error
  data_error <- psm_data_error("test data error")
  expect_s3_class(data_error, "psm_data_error")
  expect_s3_class(data_error, "error")
  expect_s3_class(data_error, "condition")
  expect_equal(data_error$message, "test data error")
  
  # Test with call parameter
  input_error_with_call <- psm_input_error("test", call = sys.call())
  expect_false(is.null(input_error_with_call$call))
})

#----
# Tests for validate_logical_scalar
#----

test_that("validate_logical_scalar accepts valid inputs", {
  expect_silent(validate_logical_scalar(TRUE, "test_param"))
  expect_silent(validate_logical_scalar(FALSE, "test_param"))
})

test_that("validate_logical_scalar rejects invalid inputs", {
  # Multiple values
  expect_error(
    validate_logical_scalar(c(TRUE, FALSE), "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_logical_scalar(c(TRUE, FALSE), "test_param"),
    "Parameter 'test_param' must be a single logical value"
  )
  
  # Non-logical types
  expect_error(
    validate_logical_scalar("yes", "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_logical_scalar(1, "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_logical_scalar(factor(TRUE), "test_param"),
    class = "psm_input_error"
  )
  
  # NA values
  expect_error(
    validate_logical_scalar(NA, "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_logical_scalar(c(TRUE, NA), "test_param"),
    class = "psm_input_error"
  )
})

#----
# Tests for validate_numeric_scalar
#----

test_that("validate_numeric_scalar accepts valid inputs", {
  expect_silent(validate_numeric_scalar(1.5, "test_param"))
  expect_silent(validate_numeric_scalar(0, "test_param"))
  expect_silent(validate_numeric_scalar(-5.2, "test_param"))
  expect_silent(validate_numeric_scalar(1L, "test_param"))  # Integer is numeric
  
  # With allow_na = TRUE
  expect_silent(validate_numeric_scalar(NA, "test_param", allow_na = TRUE))
  expect_silent(validate_numeric_scalar(NA_real_, "test_param", allow_na = TRUE))
})

test_that("validate_numeric_scalar rejects invalid inputs", {
  # Multiple values
  expect_error(
    validate_numeric_scalar(c(1, 2), "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_numeric_scalar(c(1, 2), "test_param"),
    "Parameter 'test_param' must be a single numeric value"
  )
  
  # Non-numeric types
  expect_error(
    validate_numeric_scalar("1", "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_numeric_scalar(TRUE, "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_numeric_scalar(factor(1), "test_param"),
    class = "psm_input_error"
  )
  
  # NA values when not allowed
  expect_error(
    validate_numeric_scalar(NA, "test_param", allow_na = FALSE),
    class = "psm_input_error"
  )
  expect_error(
    validate_numeric_scalar(NA, "test_param"),  # Default is allow_na = FALSE
    "Parameter 'test_param' cannot contain NA values"
  )
})

#----
# Tests for validate_method_choice
#----

test_that("validate_method_choice accepts valid inputs", {
  valid_choices <- c("min", "max", "mean", "median")
  
  expect_silent(validate_method_choice("min", valid_choices, "test_param"))
  expect_silent(validate_method_choice("max", valid_choices, "test_param"))
  expect_silent(validate_method_choice("mean", valid_choices, "test_param"))
  expect_silent(validate_method_choice("median", valid_choices, "test_param"))
})

test_that("validate_method_choice rejects invalid inputs", {
  valid_choices <- c("min", "max", "mean", "median")
  
  # Invalid choice
  expect_error(
    validate_method_choice("invalid", valid_choices, "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_method_choice("invalid", valid_choices, "test_param"),
    "Parameter 'test_param' must be one of: min, max, mean, median"
  )
  
  # Multiple values
  expect_error(
    validate_method_choice(c("min", "max"), valid_choices, "test_param"),
    class = "psm_input_error"
  )
  expect_error(
    validate_method_choice(c("min", "max"), valid_choices, "test_param"),
    "Parameter 'test_param' must have length 1"
  )
  
  # Wrong type
  expect_error(
    validate_method_choice(1, valid_choices, "test_param"),
    class = "psm_input_error"
  )
})

#----
# Tests for validate_price_vectors
#----

test_that("validate_price_vectors accepts valid inputs", {
  # Standard case
  expect_silent(validate_price_vectors(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  ))
  
  # Single values
  expect_silent(validate_price_vectors(
    toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4
  ))
  
  # All toocheap NA (allowed by default)
  expect_silent(validate_price_vectors(
    toocheap = c(NA, NA), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  ))
  
  # Integer values (should be treated as numeric)
  expect_silent(validate_price_vectors(
    toocheap = c(1L, 2L), cheap = c(2L, 3L), 
    expensive = c(3L, 4L), tooexpensive = c(4L, 5L)
  ))
})

test_that("validate_price_vectors rejects invalid inputs", {
  # Non-vector inputs
  expect_error(
    validate_price_vectors(
      toocheap = matrix(c(1, 2)), cheap = c(2, 3), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    class = "psm_data_error"
  )
  expect_error(
    validate_price_vectors(
      toocheap = matrix(c(1, 2)), cheap = c(2, 3), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    "Price variable 'toocheap' must be a vector"
  )
  
  # Non-numeric types
  expect_error(
    validate_price_vectors(
      toocheap = c(1, 2), cheap = c("a", "b"), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    class = "psm_data_error"
  )
  expect_error(
    validate_price_vectors(
      toocheap = c(1, 2), cheap = c("a", "b"), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    "Price variable 'cheap' must be numeric"
  )
  
  # Unequal lengths
  expect_error(
    validate_price_vectors(
      toocheap = c(1, 2), cheap = c(2, 3, 4), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    class = "psm_data_error"
  )
  expect_error(
    validate_price_vectors(
      toocheap = c(1, 2), cheap = c(2, 3, 4), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    "All price variables must have the same length"
  )
  
  # Non-numeric toocheap when not all NA
  expect_error(
    validate_price_vectors(
      toocheap = c("a", "b"), cheap = c(2, 3), 
      expensive = c(3, 4), tooexpensive = c(4, 5)
    ),
    "Price variable 'toocheap' must be numeric"
  )
})

#----
# Tests for validate_data_frame_structure
#----

test_that("validate_data_frame_structure accepts valid inputs", {
  # Valid data frame
  valid_data <- data.frame(
    tc = c(1, 2), ch = c(2, 3), ex = c(3, 4), te = c(4, 5)
  )
  expect_silent(validate_data_frame_structure(
    valid_data, "tc", "ch", "ex", "te"
  ))
  
  # Valid matrix
  valid_matrix <- as.matrix(valid_data)
  expect_silent(validate_data_frame_structure(
    valid_matrix, "tc", "ch", "ex", "te"
  ))
  
  # Data frame with all toocheap NA
  data_with_na <- data.frame(
    tc = c(NA, NA), ch = c(2, 3), ex = c(3, 4), te = c(4, 5)
  )
  expect_silent(validate_data_frame_structure(
    data_with_na, "tc", "ch", "ex", "te"
  ))
})

test_that("validate_data_frame_structure rejects invalid inputs", {
  valid_data <- data.frame(
    tc = c(1, 2), ch = c(2, 3), ex = c(3, 4), te = c(4, 5)
  )
  
  # Non-data.frame/matrix input
  expect_error(
    validate_data_frame_structure(
      list(tc = c(1, 2)), "tc", "ch", "ex", "te"
    ),
    class = "psm_data_error"
  )
  expect_error(
    validate_data_frame_structure(
      list(tc = c(1, 2)), "tc", "ch", "ex", "te"
    ),
    "Data argument must be a data frame or matrix"
  )
  
  # Non-character column names
  expect_error(
    validate_data_frame_structure(valid_data, 1, "ch", "ex", "te"),
    class = "psm_input_error"
  )
  expect_error(
    validate_data_frame_structure(valid_data, 1, "ch", "ex", "te"),
    "Parameter 'toocheap' must be a single character value"
  )
  
  # Multiple column names
  expect_error(
    validate_data_frame_structure(valid_data, c("tc", "tc2"), "ch", "ex", "te"),
    class = "psm_input_error"
  )
  
  # Missing columns
  expect_error(
    validate_data_frame_structure(valid_data, "missing", "ch", "ex", "te"),
    class = "psm_data_error"
  )
  expect_error(
    validate_data_frame_structure(valid_data, "missing", "ch", "ex", "te"),
    "Could not find columns in data: missing"
  )
  
  # Non-numeric columns
  invalid_data <- data.frame(
    tc = c("a", "b"), ch = c(2, 3), ex = c(3, 4), te = c(4, 5)
  )
  expect_error(
    validate_data_frame_structure(invalid_data, "tc", "ch", "ex", "te"),
    class = "psm_data_error"
  )
  expect_error(
    validate_data_frame_structure(invalid_data, "tc", "ch", "ex", "te"),
    "Column 'tc' must contain numeric values"
  )
})

#----
# Tests for validate_nms_parameters
#----

test_that("validate_nms_parameters detects when NMS is not requested", {
  # Both NA
  expect_false(validate_nms_parameters(
    pi_cheap = NA, pi_expensive = NA, 
    pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
    pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
  ))
  
  # All NA
  expect_false(validate_nms_parameters(
    pi_cheap = c(NA, NA), pi_expensive = c(NA, NA), 
    pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
    pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
  ))
})

test_that("validate_nms_parameters accepts valid NMS inputs", {
  # Valid vector inputs
  expect_true(validate_nms_parameters(
    pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
    pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
    pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
  ))
  
  # Valid data frame inputs
  data <- data.frame(
    pi_ch = c(3, 4), pi_ex = c(2, 3)
  )
  expect_true(validate_nms_parameters(
    pi_cheap = "pi_ch", pi_expensive = "pi_ex", 
    pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
    pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0,
    data = data
  ))
})

test_that("validate_nms_parameters rejects invalid NMS inputs", {
  # Mismatched scale and calibration lengths
  expect_error(
    validate_nms_parameters(
      pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
      pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3),  # Too short
      pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
    ),
    class = "psm_input_error"
  )
  expect_error(
    validate_nms_parameters(
      pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
      pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3),
      pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
    ),
    "pi_scale and pi_calibrated must have the same length"
  )
  
  # Non-numeric calibration values
  expect_error(
    validate_nms_parameters(
      pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
      pi_scale = 5:1, pi_calibrated = c("a", "b", "c", "d", "e"),
      pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
    ),
    class = "psm_input_error"
  )
  
  # NaN calibration values
  expect_error(
    validate_nms_parameters(
      pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
      pi_scale = 5:1, pi_calibrated = c(0.7, NaN, 0.3, 0.1, 0),
      pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
    ),
    "Calibrated purchase intent values cannot be NaN"
  )
  
  # Infinite calibration values
  expect_error(
    validate_nms_parameters(
      pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
      pi_scale = 5:1, pi_calibrated = c(0.7, Inf, 0.3, 0.1, 0),
      pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
    ),
    "Calibrated purchase intent values cannot be infinite"
  )
})

test_that("validate_nms_parameters warns about out-of-range calibration values", {
  # Negative values
  expect_warning(
    validate_nms_parameters(
      pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
      pi_scale = 5:1, pi_calibrated = c(0.7, -0.1, 0.3, 0.1, 0),
      pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
    ),
    "Some calibrated purchase intent values are negative"
  )
  
  # Values > 1
  expect_warning(
    validate_nms_parameters(
      pi_cheap = c(3, 4), pi_expensive = c(2, 3), 
      pi_scale = 5:1, pi_calibrated = c(1.5, 0.5, 0.3, 0.1, 0),
      pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0
    ),
    "Some calibrated purchase intent values are greater than 1"
  )
})

#----
# Tests for validate_survey_design
#----

test_that("validate_survey_design works correctly", {
  # Skip if survey package not available
  skip_if_not_installed("survey")
  
  # Valid survey design
  data <- data.frame(
    tc = c(1, 2, 3), ch = c(2, 3, 4), ex = c(3, 4, 5), te = c(4, 5, 6),
    weights = c(1, 1, 1)
  )
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = data)
  expect_silent(validate_survey_design(design))
  
  # Invalid input (not survey design)
  expect_error(
    validate_survey_design(data),
    class = "psm_input_error"
  )
  expect_error(
    validate_survey_design(data),
    "The design argument must be a survey.design object"
  )
})

#----
# Tests for main validation function
#----

test_that("validate_psm_inputs works with vector inputs", {
  result <- validate_psm_inputs(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  expect_true(is.list(result))
  expect_true("nms_requested" %in% names(result))
  expect_false(result$nms_requested)
})

test_that("validate_psm_inputs works with data frame inputs", {
  data <- data.frame(
    tc = c(1, 2), ch = c(2, 3), ex = c(3, 4), te = c(4, 5)
  )
  result <- validate_psm_inputs(
    toocheap = "tc", cheap = "ch", expensive = "ex", tooexpensive = "te",
    data = data
  )
  expect_true(is.list(result))
  expect_false(result$nms_requested)
})

test_that("validate_psm_inputs detects NMS requests", {
  result <- validate_psm_inputs(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5),
    pi_cheap = c(3, 4), pi_expensive = c(2, 3)
  )
  expect_true(result$nms_requested)
})

test_that("validate_psm_inputs validates all parameters", {
  # Invalid validate parameter
  expect_error(
    validate_psm_inputs(
      toocheap = c(1, 2), cheap = c(2, 3), 
      expensive = c(3, 4), tooexpensive = c(4, 5),
      validate = "yes"
    ),
    class = "psm_input_error"
  )
  
  # Invalid intersection method
  expect_error(
    validate_psm_inputs(
      toocheap = c(1, 2), cheap = c(2, 3), 
      expensive = c(3, 4), tooexpensive = c(4, 5),
      intersection_method = "invalid"
    ),
    class = "psm_input_error"
  )
  
  # Invalid interpolation steps when interpolate = TRUE
  expect_error(
    validate_psm_inputs(
      toocheap = c(1, 2), cheap = c(2, 3), 
      expensive = c(3, 4), tooexpensive = c(4, 5),
      interpolate = TRUE, interpolation_steps = "invalid"
    ),
    class = "psm_input_error"
  )
})
