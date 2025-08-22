context("PSM Constants")

#----
# Tests for PSM_CONSTANTS structure and accessibility
#----

test_that("PSM_CONSTANTS is properly defined", {
  expect_true(is.list(PSM_CONSTANTS))
  expect_true(length(PSM_CONSTANTS) > 0)
  
  # Check that all required top-level constants exist
  required_constants <- c(
    "ROUNDING_DIGITS", 
    "DEFAULT_INTERPOLATION_STEPS",
    "VALID_INTERSECTION_METHODS",
    "VALID_ACCEPTABLE_RANGES",
    "DEFAULT_COLORS",
    "DEFAULT_PLOT_SETTINGS",
    "DEFAULT_LINE_TYPES",
    "NMS_DEFAULTS"
  )
  
  expect_true(all(required_constants %in% names(PSM_CONSTANTS)))
})

test_that("PSM_CONSTANTS have correct types and values", {
  # Numeric constants
  expect_true(is.integer(PSM_CONSTANTS$ROUNDING_DIGITS))
  expect_equal(PSM_CONSTANTS$ROUNDING_DIGITS, 2L)
  
  expect_true(is.numeric(PSM_CONSTANTS$DEFAULT_INTERPOLATION_STEPS))
  expect_equal(PSM_CONSTANTS$DEFAULT_INTERPOLATION_STEPS, 0.01)
  
  # Character vector constants
  expect_true(is.character(PSM_CONSTANTS$VALID_INTERSECTION_METHODS))
  expect_equal(PSM_CONSTANTS$VALID_INTERSECTION_METHODS, c("min", "max", "mean", "median"))
  
  expect_true(is.character(PSM_CONSTANTS$VALID_ACCEPTABLE_RANGES))
  expect_equal(PSM_CONSTANTS$VALID_ACCEPTABLE_RANGES, c("original", "narrower"))
  
  # List constants
  expect_true(is.list(PSM_CONSTANTS$DEFAULT_COLORS))
  expect_true(is.list(PSM_CONSTANTS$DEFAULT_PLOT_SETTINGS))
  expect_true(is.list(PSM_CONSTANTS$NMS_DEFAULTS))
})

test_that("nested constants are properly structured", {
  # DEFAULT_COLORS structure
  expect_true("IDP" %in% names(PSM_CONSTANTS$DEFAULT_COLORS))
  expect_true("OPP" %in% names(PSM_CONSTANTS$DEFAULT_COLORS))
  expect_true("PRICE_RANGE" %in% names(PSM_CONSTANTS$DEFAULT_COLORS))
  expect_true("LINE_COLORS" %in% names(PSM_CONSTANTS$DEFAULT_COLORS))
  
  # Check color values are character
  expect_true(is.character(PSM_CONSTANTS$DEFAULT_COLORS$IDP))
  expect_true(is.character(PSM_CONSTANTS$DEFAULT_COLORS$OPP))
  expect_true(is.character(PSM_CONSTANTS$DEFAULT_COLORS$PRICE_RANGE))
  
  # LINE_COLORS should be named character vector
  expect_true(is.character(PSM_CONSTANTS$DEFAULT_COLORS$LINE_COLORS))
  expect_true(!is.null(names(PSM_CONSTANTS$DEFAULT_COLORS$LINE_COLORS)))
  
  # DEFAULT_PLOT_SETTINGS structure
  plot_settings <- PSM_CONSTANTS$DEFAULT_PLOT_SETTINGS
  expect_true("PRICE_RANGE_ALPHA" %in% names(plot_settings))
  expect_true("LINE_SIZE" %in% names(plot_settings))
  expect_true("POINT_SIZE_IDP" %in% names(plot_settings))
  expect_true("POINT_SIZE_OPP" %in% names(plot_settings))
  
  # Check plot setting types
  expect_true(is.numeric(plot_settings$PRICE_RANGE_ALPHA))
  expect_true(is.numeric(plot_settings$LINE_SIZE))
  expect_true(is.numeric(plot_settings$POINT_SIZE_IDP))
  expect_true(is.numeric(plot_settings$POINT_SIZE_OPP))
  
  # NMS_DEFAULTS structure
  nms_defaults <- PSM_CONSTANTS$NMS_DEFAULTS
  expect_true("PI_SCALE" %in% names(nms_defaults))
  expect_true("PI_CALIBRATED" %in% names(nms_defaults))
  expect_true("PI_CALIBRATED_TOOCHEAP" %in% names(nms_defaults))
  expect_true("PI_CALIBRATED_TOOEXPENSIVE" %in% names(nms_defaults))
  
  # Check NMS default types and values
  expect_true(is.numeric(nms_defaults$PI_SCALE))
  expect_true(is.numeric(nms_defaults$PI_CALIBRATED))
  expect_equal(length(nms_defaults$PI_SCALE), length(nms_defaults$PI_CALIBRATED))
  expect_equal(nms_defaults$PI_SCALE, 5:1)
  expect_equal(nms_defaults$PI_CALIBRATED, c(0.7, 0.5, 0.3, 0.1, 0))
})

#----
# Tests for get_psm_constant() accessor function
#----

test_that("get_psm_constant works with top-level constants", {
  expect_equal(get_psm_constant("ROUNDING_DIGITS"), 2L)
  expect_equal(get_psm_constant("DEFAULT_INTERPOLATION_STEPS"), 0.01)
  expect_equal(get_psm_constant("VALID_INTERSECTION_METHODS"), c("min", "max", "mean", "median"))
  expect_equal(get_psm_constant("VALID_ACCEPTABLE_RANGES"), c("original", "narrower"))
})

test_that("get_psm_constant works with nested constants", {
  expect_equal(get_psm_constant("DEFAULT_COLORS.IDP"), "#009E73")
  expect_equal(get_psm_constant("DEFAULT_COLORS.OPP"), "#009E73")
  expect_equal(get_psm_constant("DEFAULT_COLORS.PRICE_RANGE"), "grey50")
  
  expect_equal(get_psm_constant("DEFAULT_PLOT_SETTINGS.PRICE_RANGE_ALPHA"), 0.3)
  expect_equal(get_psm_constant("DEFAULT_PLOT_SETTINGS.LINE_SIZE"), 1)
  
  expect_equal(get_psm_constant("NMS_DEFAULTS.PI_SCALE"), 5:1)
  expect_equal(get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED"), c(0.7, 0.5, 0.3, 0.1, 0))
  expect_equal(get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOCHEAP"), 0)
  expect_equal(get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOEXPENSIVE"), 0)
})

test_that("get_psm_constant handles invalid paths correctly", {
  expect_error(get_psm_constant("NONEXISTENT_CONSTANT"), "Constant not found")
  expect_error(get_psm_constant("DEFAULT_COLORS.NONEXISTENT"), "Constant not found")
  expect_error(get_psm_constant("NONEXISTENT.NESTED"), "Constant not found")
})

test_that("get_psm_constant handles edge cases", {
  # Empty string
  expect_error(get_psm_constant(""), "Constant not found")
  
  # Multiple dots
  expect_error(get_psm_constant("DEFAULT_COLORS.LINE_COLORS.NONEXISTENT"), "Constant not found")
  
  # Valid nested access to complex structures
  line_colors <- get_psm_constant("DEFAULT_COLORS.LINE_COLORS")
  expect_true(is.character(line_colors))
  expect_true("too cheap" %in% names(line_colors))
  expect_true("not cheap" %in% names(line_colors))
})

#----
# Tests for constant value consistency
#----

test_that("color constants are valid hex colors or named colors", {
  # Test main colors
  expect_match(PSM_CONSTANTS$DEFAULT_COLORS$IDP, "^#[0-9A-Fa-f]{6}$")
  expect_match(PSM_CONSTANTS$DEFAULT_COLORS$OPP, "^#[0-9A-Fa-f]{6}$")
  
  # PRICE_RANGE can be named color
  expect_true(PSM_CONSTANTS$DEFAULT_COLORS$PRICE_RANGE %in% c("grey50") || 
              grepl("^#[0-9A-Fa-f]{6}$", PSM_CONSTANTS$DEFAULT_COLORS$PRICE_RANGE))
  
  # Line colors should be valid
  line_colors <- PSM_CONSTANTS$DEFAULT_COLORS$LINE_COLORS
  for (color in line_colors) {
    expect_true(grepl("^#[0-9A-Fa-f]{6}$", color) || color %in% colors())
  }
})

test_that("numeric constants are within reasonable ranges", {
  # Rounding digits should be reasonable
  expect_gte(PSM_CONSTANTS$ROUNDING_DIGITS, 0)
  expect_lte(PSM_CONSTANTS$ROUNDING_DIGITS, 10)
  
  # Interpolation steps should be positive and small
  expect_gt(PSM_CONSTANTS$DEFAULT_INTERPOLATION_STEPS, 0)
  expect_lt(PSM_CONSTANTS$DEFAULT_INTERPOLATION_STEPS, 1)
  
  # Plot settings should be reasonable
  expect_gte(PSM_CONSTANTS$DEFAULT_PLOT_SETTINGS$PRICE_RANGE_ALPHA, 0)
  expect_lte(PSM_CONSTANTS$DEFAULT_PLOT_SETTINGS$PRICE_RANGE_ALPHA, 1)
  expect_gt(PSM_CONSTANTS$DEFAULT_PLOT_SETTINGS$LINE_SIZE, 0)
  expect_gt(PSM_CONSTANTS$DEFAULT_PLOT_SETTINGS$POINT_SIZE_IDP, 0)
  expect_gt(PSM_CONSTANTS$DEFAULT_PLOT_SETTINGS$POINT_SIZE_OPP, 0)
})

test_that("NMS defaults are consistent", {
  nms <- PSM_CONSTANTS$NMS_DEFAULTS
  
  # PI_SCALE and PI_CALIBRATED should have same length
  expect_equal(length(nms$PI_SCALE), length(nms$PI_CALIBRATED))
  
  # PI_CALIBRATED should be probabilities (0-1)
  expect_true(all(nms$PI_CALIBRATED >= 0))
  expect_true(all(nms$PI_CALIBRATED <= 1))
  
  # Boundary calibrations should be 0-1
  expect_gte(nms$PI_CALIBRATED_TOOCHEAP, 0)
  expect_lte(nms$PI_CALIBRATED_TOOCHEAP, 1)
  expect_gte(nms$PI_CALIBRATED_TOOEXPENSIVE, 0)
  expect_lte(nms$PI_CALIBRATED_TOOEXPENSIVE, 1)
  
  # PI_SCALE should be numeric and ordered
  expect_true(is.numeric(nms$PI_SCALE))
  expect_true(length(unique(nms$PI_SCALE)) == length(nms$PI_SCALE)) # No duplicates
})

#----
# Tests for constant immutability (defensive programming)
#----

test_that("constants cannot be accidentally modified", {
  # Store original value
  original_rounding <- PSM_CONSTANTS$ROUNDING_DIGITS
  
  # Attempt to modify (this should not affect the original)
  temp_constants <- PSM_CONSTANTS
  temp_constants$ROUNDING_DIGITS <- 999
  
  # Original should be unchanged
  expect_equal(PSM_CONSTANTS$ROUNDING_DIGITS, original_rounding)
  
  # get_psm_constant should still return original value
  expect_equal(get_psm_constant("ROUNDING_DIGITS"), original_rounding)
})
