#' @title Input Validation Utilities for PSM Analysis
#' @description Internal validation functions for consistent error handling
#' @keywords internal
#' @noRd

# Custom error classes for better error handling
psm_input_error <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("psm_input_error", "error", "condition")
  )
}

psm_data_error <- function(message, call = NULL) {
  structure(
    list(message = message, call = call),
    class = c("psm_data_error", "error", "condition")
  )
}

#' Validate logical scalar parameters
#' @param value The value to validate
#' @param param_name Name of the parameter for error messages
#' @noRd
validate_logical_scalar <- function(value, param_name) {
  if (any(is.na(value)) || !is.logical(value) || length(value) != 1) {
    stop(psm_input_error(
      sprintf("Parameter '%s' must be a single logical value", param_name)
    ))
  }
}

#' Validate numeric scalar parameters
#' @param value The value to validate
#' @param param_name Name of the parameter for error messages
#' @param allow_na Whether NA values are allowed
#' @noRd
validate_numeric_scalar <- function(value, param_name, allow_na = FALSE) {
  # Check length first
  if (length(value) != 1) {
    stop(psm_input_error(
      sprintf("Parameter '%s' must be a single numeric value", param_name)
    ))
  }
  
  # If NA is allowed and the value is NA, it's valid
  if (allow_na && is.na(value)) {
    return(invisible(TRUE))
  }
  
  # Check for NA when not allowed
  if (!allow_na && is.na(value)) {
    stop(psm_input_error(
      sprintf("Parameter '%s' cannot contain NA values", param_name)
    ))
  }
  
  # Check if numeric
  if (!is.numeric(value)) {
    stop(psm_input_error(
      sprintf("Parameter '%s' must be a single numeric value", param_name)
    ))
  }
}

#' Validate method choice parameters
#' @param value The value to validate
#' @param valid_choices Vector of valid choices
#' @param param_name Name of the parameter for error messages
#' @noRd
validate_method_choice <- function(value, valid_choices, param_name) {
  if (length(value) != 1) {
    stop(psm_input_error(
      sprintf("Parameter '%s' must have length 1", param_name)
    ))
  }
  
  if (!value %in% valid_choices) {
    stop(psm_input_error(
      sprintf("Parameter '%s' must be one of: %s", 
              param_name, paste(valid_choices, collapse = ", "))
    ))
  }
}

#' Validate price data vectors
#' @param toocheap Too cheap price vector
#' @param cheap Cheap price vector  
#' @param expensive Expensive price vector
#' @param tooexpensive Too expensive price vector
#' @param allow_toocheap_na Whether too cheap can be all NA
#' @noRd
validate_price_vectors <- function(toocheap, cheap, expensive, tooexpensive, 
                                 allow_toocheap_na = TRUE) {
  
  # Check if all are vectors
  price_vars <- list(
    toocheap = toocheap, cheap = cheap, 
    expensive = expensive, tooexpensive = tooexpensive
  )
  
  for (var_name in names(price_vars)) {
    var_value <- price_vars[[var_name]]
    if (!is.vector(var_value)) {
      stop(psm_data_error(
        sprintf("Price variable '%s' must be a vector", var_name)
      ))
    }
  }
  
  # Check numeric types (with special handling for toocheap)
  if (!allow_toocheap_na || !all(is.na(toocheap))) {
    if (!is.numeric(toocheap)) {
      stop(psm_data_error(
        "Price variable 'toocheap' must be numeric (or all NA if not collected)"
      ))
    }
  }
  
  for (var_name in c("cheap", "expensive", "tooexpensive")) {
    var_value <- price_vars[[var_name]]
    if (!is.numeric(var_value)) {
      stop(psm_data_error(
        sprintf("Price variable '%s' must be numeric", var_name)
      ))
    }
  }
  
  # Check equal lengths
  lengths <- sapply(price_vars, length)
  if (!all(lengths == lengths[1])) {
    stop(psm_data_error(
      "All price variables must have the same length"
    ))
  }
}

#' Validate data frame structure for PSM analysis
#' @param data Data frame or matrix
#' @param toocheap Column name for too cheap prices
#' @param cheap Column name for cheap prices
#' @param expensive Column name for expensive prices
#' @param tooexpensive Column name for too expensive prices
#' @noRd
validate_data_frame_structure <- function(data, toocheap, cheap, expensive, tooexpensive) {
  
  # Check data structure
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop(psm_data_error(
      "Data argument must be a data frame or matrix"
    ))
  }
  
  # Check column name parameters first (input validation) - before any data checking
  col_values <- list(toocheap, cheap, expensive, tooexpensive)
  param_names <- c("toocheap", "cheap", "expensive", "tooexpensive")
  
  for (i in seq_along(col_values)) {
    param_value <- col_values[[i]]
    param_name <- param_names[i]
    
    # Check if it's a single character value
    if (!is.character(param_value) || length(param_value) != 1) {
      stop(psm_input_error(
        sprintf("Parameter '%s' must be a single character value", param_name)
      ))
    }
  }
  
  # Now we can safely use the column names for data validation
  col_names <- c(toocheap, cheap, expensive, tooexpensive)
  
  # Check if columns exist (data error)
  missing_cols <- col_names[!col_names %in% colnames(data)]
  if (length(missing_cols) > 0) {
    stop(psm_data_error(
      sprintf("Could not find columns in data: %s", 
              paste(missing_cols, collapse = ", "))
    ))
  }
  
  # Check column types
  for (i in seq_along(col_names)) {
    col_data <- data[, col_names[i]]
    param_name <- param_names[i]
    
    # Special handling for toocheap (can be all NA)
    if (param_name == "toocheap" && all(is.na(col_data))) {
      next
    }
    
    if (!is.numeric(col_data)) {
      stop(psm_data_error(
        sprintf("Column '%s' must contain numeric values", col_names[i])
      ))
    }
  }
}

#' Validate NMS (Newton Miller Smith) extension parameters
#' @param pi_cheap Purchase intent at cheap price
#' @param pi_expensive Purchase intent at expensive price  
#' @param pi_scale Purchase intent scale
#' @param pi_calibrated Calibrated purchase probabilities
#' @param pi_calibrated_toocheap Calibrated probability for too cheap
#' @param pi_calibrated_tooexpensive Calibrated probability for too expensive
#' @param data Optional data frame containing PI variables
#' @noRd
validate_nms_parameters <- function(pi_cheap, pi_expensive, pi_scale, pi_calibrated,
                                  pi_calibrated_toocheap, pi_calibrated_tooexpensive,
                                  data = NULL) {
  
  # Check if NMS analysis is requested
  nms_requested <- !all(is.na(pi_cheap)) && !all(is.na(pi_expensive))
  if (!nms_requested) {
    return(FALSE)  # No NMS analysis requested
  }
  
  # Validate scale and calibration lengths match
  if (length(pi_scale) != length(pi_calibrated)) {
    stop(psm_input_error(
      "pi_scale and pi_calibrated must have the same length"
    ))
  }
  
  # If data frame is provided, validate column references
  if (!is.null(data)) {
    validate_nms_data_frame(data, pi_cheap, pi_expensive, pi_scale)
  } else {
    validate_nms_vectors(pi_cheap, pi_expensive, pi_scale)
  }
  
  # Validate calibration values
  validate_calibration_values(pi_calibrated, pi_calibrated_toocheap, pi_calibrated_tooexpensive)
  
  return(TRUE)  # NMS analysis is valid and requested
}

#' Validate NMS parameters when using data frame
#' @noRd
validate_nms_data_frame <- function(data, pi_cheap, pi_expensive, pi_scale) {
  # Check column existence
  if (!pi_cheap %in% colnames(data) || !pi_expensive %in% colnames(data)) {
    stop(psm_data_error(
      "Could not find purchase intent columns in data"
    ))
  }
  
  # Check data types and scale consistency
  pi_cheap_data <- data[, pi_cheap]
  pi_expensive_data <- data[, pi_expensive]
  
  validate_pi_data_consistency(pi_cheap_data, pi_expensive_data, pi_scale)
}

#' Validate NMS parameters when using vectors
#' @noRd
validate_nms_vectors <- function(pi_cheap, pi_expensive, pi_scale) {
  # Check vector types
  if (!is.vector(pi_cheap) || !is.vector(pi_expensive)) {
    stop(psm_input_error(
      "Purchase intent variables must be vectors when not using data frame"
    ))
  }
  
  # Check equal lengths
  if (length(pi_cheap) != length(pi_expensive)) {
    stop(psm_input_error(
      "Purchase intent variables must have equal length"
    ))
  }
  
  validate_pi_data_consistency(pi_cheap, pi_expensive, pi_scale)
}

#' Validate purchase intent data consistency with scale
#' @noRd
validate_pi_data_consistency <- function(pi_cheap_data, pi_expensive_data, pi_scale) {
  # Check numeric types
  if (!is.numeric(pi_cheap_data) || !is.numeric(pi_expensive_data)) {
    stop(psm_data_error(
      "Purchase intent data must be numeric"
    ))
  }
  
  if (!is.numeric(pi_scale)) {
    stop(psm_input_error(
      "Purchase intent scale must be numeric"
    ))
  }
  
  # Check that all PI data values are in the defined scale
  if (!all(unique(pi_cheap_data) %in% unique(pi_scale))) {
    stop(psm_data_error(
      "pi_cheap contains values not defined in pi_scale"
    ))
  }
  
  if (!all(unique(pi_expensive_data) %in% unique(pi_scale))) {
    stop(psm_data_error(
      "pi_expensive contains values not defined in pi_scale"
    ))
  }
}

#' Validate calibration probability values
#' @noRd
validate_calibration_values <- function(pi_calibrated, pi_calibrated_toocheap, pi_calibrated_tooexpensive) {
  # Check numeric type
  if (!is.numeric(pi_calibrated)) {
    stop(psm_input_error(
      "Calibrated purchase intent values must be numeric"
    ))
  }
  
  # Check for invalid values
  if (any(is.nan(pi_calibrated))) {
    stop(psm_input_error(
      "Calibrated purchase intent values cannot be NaN"
    ))
  }
  
  if (any(is.infinite(pi_calibrated))) {
    stop(psm_input_error(
      "Calibrated purchase intent values cannot be infinite"
    ))
  }
  
  # Warn about values outside [0,1] range
  if (any(pi_calibrated < 0)) {
    warning("Some calibrated purchase intent values are negative. ",
            "Interpretation of reach/revenue values not recommended.")
  }
  
  if (any(pi_calibrated > 1)) {
    warning("Some calibrated purchase intent values are greater than 1. ",
            "Interpretation of reach/revenue values not recommended.")
  }
}

#' Validate survey design object for weighted analysis
#' @param design Survey design object
#' @noRd
validate_survey_design <- function(design) {
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop(psm_input_error(
      "The 'survey' package is required for weighted analysis. ",
      "Please install it or use psm_analysis() for unweighted analysis."
    ))
  }
  
  if (!inherits(design, "survey.design")) {
    stop(psm_input_error(
      "The design argument must be a survey.design object created with svydesign()"
    ))
  }
}

#' Main validation function for PSM inputs
#' @param ... All PSM function parameters
#' @noRd
validate_psm_inputs <- function(toocheap, cheap, expensive, tooexpensive, 
                               data = NA, validate = TRUE, interpolate = FALSE,
                               interpolation_steps = 0.01, intersection_method = "min",
                               acceptable_range = "original", pi_cheap = NA, 
                               pi_expensive = NA, pi_scale = 5:1, 
                               pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
                               pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0,
                               design = NULL) {
  
  # Validate basic parameters
  validate_logical_scalar(validate, "validate")
  validate_logical_scalar(interpolate, "interpolate")
  
  if (interpolate) {
    validate_numeric_scalar(interpolation_steps, "interpolation_steps")
  }
  
  validate_method_choice(intersection_method, 
                        get_psm_constant("VALID_INTERSECTION_METHODS"),
                        "intersection_method")
  
  validate_method_choice(acceptable_range,
                        get_psm_constant("VALID_ACCEPTABLE_RANGES"), 
                        "acceptable_range")
  
  # Validate data structure
  if (!is.null(design)) {
    # Weighted analysis
    validate_survey_design(design)
    validate_data_frame_structure(design$variables, toocheap, cheap, expensive, tooexpensive)
    data_for_nms <- design$variables
  } else if (is.data.frame(data) || is.matrix(data)) {
    # Unweighted analysis with data frame
    validate_data_frame_structure(data, toocheap, cheap, expensive, tooexpensive)
    data_for_nms <- data
  } else {
    # Unweighted analysis with vectors
    validate_price_vectors(toocheap, cheap, expensive, tooexpensive)
    data_for_nms <- NULL
  }
  
  # Validate NMS parameters if provided
  nms_requested <- validate_nms_parameters(pi_cheap, pi_expensive, pi_scale, pi_calibrated,
                                         pi_calibrated_toocheap, pi_calibrated_tooexpensive,
                                         data_for_nms)
  
  return(list(nms_requested = nms_requested))
}
