#' @title Data Processing Utilities for PSM Analysis
#' @description Internal functions for data preparation and ECDF calculations
#' @keywords internal
#' @noRd

#' Prepare PSM data from various input formats
#' @param toocheap Too cheap prices (vector or column name)
#' @param cheap Cheap prices (vector or column name)
#' @param expensive Expensive prices (vector or column name)
#' @param tooexpensive Too expensive prices (vector or column name)
#' @param data Optional data frame containing price variables
#' @param design Optional survey design object for weighted analysis
#' @param pi_cheap Purchase intent at cheap price (optional)
#' @param pi_expensive Purchase intent at expensive price (optional)
#' @param pi_scale Purchase intent scale (optional)
#' @param pi_calibrated Calibrated purchase probabilities (optional)
#' @param validate Whether to validate price preferences
#' @noRd
prepare_psm_data <- function(toocheap, cheap, expensive, tooexpensive, 
                           data = NA, design = NULL, pi_cheap = NA, pi_expensive = NA,
                           pi_scale = get_psm_constant("NMS_DEFAULTS.PI_SCALE"),
                           pi_calibrated = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED"),
                           validate = TRUE) {
  
  # Validate inputs before processing - this should catch type errors early
  if (!is.null(design)) {
    # For survey design, validate the column references exist and are numeric
    validate_data_frame_structure(design$variables, toocheap, cheap, expensive, tooexpensive)
  } else if (is.data.frame(data) || is.matrix(data)) {
    # For data frame, validate the column references exist and are numeric  
    validate_data_frame_structure(data, toocheap, cheap, expensive, tooexpensive)
  } else {
    # For vectors, validate they are numeric and proper length
    validate_price_vectors(toocheap, cheap, expensive, tooexpensive)
  }
  
  # Determine data source and extract price data
  if (!is.null(design)) {
    # Weighted analysis with survey design
    psmdata <- extract_data_from_design(design, toocheap, cheap, expensive, tooexpensive)
    weighted <- TRUE
    original_design <- design
  } else if (is.data.frame(data) || is.matrix(data)) {
    # Unweighted analysis with data frame
    psmdata <- extract_data_from_dataframe(data, toocheap, cheap, expensive, tooexpensive)
    weighted <- FALSE
    original_design <- NULL
  } else {
    # Unweighted analysis with vectors
    psmdata <- data.frame(
      toocheap = toocheap,
      cheap = cheap,
      expensive = expensive,
      tooexpensive = tooexpensive
    )
    weighted <- FALSE
    original_design <- NULL
  }
  
  # Add NMS data if provided
  nms_requested <- !all(is.na(pi_cheap)) && !all(is.na(pi_expensive))
  if (nms_requested) {
    psmdata <- add_nms_data(psmdata, pi_cheap, pi_expensive, pi_scale, pi_calibrated, 
                           data, design)
  }
  
  # Validate price preferences and optionally filter data
  validation_result <- validate_price_preferences(psmdata, validate)
  
  # FIXED: Create new survey design with mapped data if needed
  if (weighted && !is.null(original_design)) {
    # Get the weights from the original design
    original_weights <- weights(original_design)
    
    # Create data frame with mapped column names
    mapped_data <- validation_result$data
    
    # If validation filtered the data, we need to match the weights
    if (validation_result$invalid_cases > 0 && validate) {
      # Find which rows were kept after validation - assuming validation_result$data contains valid rows only
      # We need to get the weights for the valid rows
      if (nrow(mapped_data) == (validation_result$total_sample - validation_result$invalid_cases)) {
        # Create a temporary full dataset to identify which rows were kept
        temp_full_data <- data.frame(
          toocheap = original_design$variables[, toocheap],
          cheap = original_design$variables[, cheap],
          expensive = original_design$variables[, expensive],
          tooexpensive = original_design$variables[, tooexpensive]
        )
        
        # Apply same validation logic
        if (all(is.na(temp_full_data$toocheap))) {
          temp_full_data$valid <- (temp_full_data$tooexpensive > temp_full_data$expensive) & 
                                   (temp_full_data$expensive > temp_full_data$cheap)
          na_mask <- is.na(temp_full_data$tooexpensive) | is.na(temp_full_data$expensive) | is.na(temp_full_data$cheap)
        } else {
          temp_full_data$valid <- (temp_full_data$tooexpensive > temp_full_data$expensive) & 
                                   (temp_full_data$expensive > temp_full_data$cheap) & 
                                   (temp_full_data$cheap > temp_full_data$toocheap)
          na_mask <- is.na(temp_full_data$tooexpensive) | is.na(temp_full_data$expensive) | 
                     is.na(temp_full_data$cheap) | is.na(temp_full_data$toocheap)
        }
        temp_full_data$valid[na_mask] <- FALSE
        
        # Get weights for valid rows only
        valid_indices <- which(temp_full_data$valid)
        mapped_data$survey_weights <- original_weights[valid_indices]
      } else {
        # Fallback: use first N weights (not ideal but prevents error)
        warning("Weight alignment issue - using simple truncation. Results may be approximate.")
        mapped_data$survey_weights <- original_weights[1:nrow(mapped_data)]
      }
    } else {
      # No filtering, use all weights
      mapped_data$survey_weights <- original_weights
    }
    
    # Create new survey design with mapped data and properly aligned weights
    survey_design <- survey::svydesign(
      ids = ~1,
      weights = ~survey_weights,
      data = mapped_data
    )
  } else {
    survey_design <- NULL
  }
  
  # Return prepared data structure
  list(
    data = validation_result$data,
    weighted = weighted,
    survey_design = survey_design,
    nms_requested = nms_requested,
    invalid_cases = validation_result$invalid_cases,
    total_sample = validation_result$total_sample
  )
}

#' Extract price data from survey design object
#' @noRd
extract_data_from_design <- function(design, toocheap, cheap, expensive, tooexpensive) {
  data.frame(
    toocheap = design$variables[, toocheap],
    cheap = design$variables[, cheap],
    expensive = design$variables[, expensive],
    tooexpensive = design$variables[, tooexpensive]
  )
}

#' Extract price data from data frame
#' @noRd
extract_data_from_dataframe <- function(data, toocheap, cheap, expensive, tooexpensive) {
  data.frame(
    toocheap = data[, toocheap],
    cheap = data[, cheap],
    expensive = data[, expensive],
    tooexpensive = data[, tooexpensive]
  )
}

#' Add NMS (Newton Miller Smith) data to PSM data frame
#' @noRd
add_nms_data <- function(psmdata, pi_cheap, pi_expensive, pi_scale, pi_calibrated,
                        data = NULL, design = NULL) {
  
  if (!is.null(design)) {
    # Extract from survey design
    psmdata$pi_cheap <- design$variables[, pi_cheap]
    psmdata$pi_expensive <- design$variables[, pi_expensive]
  } else if (!is.null(data) && (is.data.frame(data) || is.matrix(data))) {
    # Extract from data frame
    psmdata$pi_cheap <- data[, pi_cheap]
    psmdata$pi_expensive <- data[, pi_expensive]
  } else {
    # Use vectors directly
    psmdata$pi_cheap <- pi_cheap
    psmdata$pi_expensive <- pi_expensive
  }
  
  # Add calibrated purchase intent values
  psmdata$pi_cheap_cal <- calibrate_purchase_intent(psmdata$pi_cheap, pi_scale, pi_calibrated)
  psmdata$pi_expensive_cal <- calibrate_purchase_intent(psmdata$pi_expensive, pi_scale, pi_calibrated)
  
  return(psmdata)
}

#' Calibrate purchase intent values based on scale
#' @noRd
calibrate_purchase_intent <- function(pi_data, pi_scale, pi_calibrated) {
  pi_calibrated_values <- rep(NA, length(pi_data))
  
  for (i in seq_along(pi_scale)) {
    pi_calibrated_values[pi_data == pi_scale[i]] <- pi_calibrated[i]
  }
  
  return(pi_calibrated_values)
}

#' Validate price preferences and optionally filter invalid cases
#' @noRd
validate_price_preferences <- function(psmdata, validate) {
  
  # Determine validation logic based on whether toocheap is available
  if (all(is.na(psmdata$toocheap))) {
    # No "too cheap" data: validate cheap < expensive < too expensive
    psmdata$valid <- (psmdata$tooexpensive > psmdata$expensive) & 
                     (psmdata$expensive > psmdata$cheap)
    
    # Set invalid if any required values are NA
    na_mask <- is.na(psmdata$tooexpensive) | is.na(psmdata$expensive) | is.na(psmdata$cheap)
    psmdata$valid[na_mask] <- FALSE
    
  } else {
    # Full validation: too cheap < cheap < expensive < too expensive
    psmdata$valid <- (psmdata$tooexpensive > psmdata$expensive) & 
                     (psmdata$expensive > psmdata$cheap) & 
                     (psmdata$cheap > psmdata$toocheap)
    
    # Set invalid if any values are NA
    na_mask <- is.na(psmdata$tooexpensive) | is.na(psmdata$expensive) | 
               is.na(psmdata$cheap) | is.na(psmdata$toocheap)
    psmdata$valid[na_mask] <- FALSE
  }
  
  # Calculate validation statistics
  invalid_cases <- sum(!psmdata$valid)
  total_sample <- nrow(psmdata)
  
  # Check if all cases are invalid - use proper custom error class
  if (total_sample == invalid_cases) {
    error_obj <- psm_data_error("All respondents have intransitive preference structures")
    signalCondition(error_obj)
    stop(error_obj)
  }
  
  # Issue warning if there are invalid cases and validation is not enabled
  if (invalid_cases > 0 && !validate) {
    warning("Some respondents have inconsistent price structures. ",
            "Consider using 'validate = TRUE' to analyze only consistent cases.")
  }
  
  # Filter data if validation is enabled
  if (validate && invalid_cases > 0) {
    # Determine which columns to keep
    if ("pi_cheap" %in% names(psmdata)) {
      keep_cols <- c("toocheap", "cheap", "expensive", "tooexpensive", 
                     "pi_cheap", "pi_expensive", "pi_cheap_cal", "pi_expensive_cal")
    } else {
      keep_cols <- c("toocheap", "cheap", "expensive", "tooexpensive")
    }
    
    psmdata <- psmdata[psmdata$valid, keep_cols, drop = FALSE]
  }
  
  # Remove the temporary 'valid' column before returning
  psmdata$valid <- NULL
  
  list(
    data = psmdata,
    invalid_cases = invalid_cases,
    total_sample = total_sample
  )
}

#' Calculate empirical cumulative distribution functions for price data
#' @param psmdata Prepared PSM data frame
#' @param weighted Whether to use weighted calculations
#' @param survey_design Survey design object (for weighted analysis)
#' @param interpolate Whether to apply interpolation
#' @param interpolation_steps Step size for interpolation
#' @noRd
calculate_ecdf_data <- function(psmdata, weighted = FALSE, survey_design = NULL,
                               interpolate = FALSE, 
                               interpolation_steps = get_psm_constant("DEFAULT_INTERPOLATION_STEPS")) {
  
  # Create base price grid
  if (weighted) {
    # For weighted analysis, round prices to avoid precision issues
    rounding_digits <- get_psm_constant("ROUNDING_DIGITS")
    price_grid <- sort(unique(c(
      round(psmdata$toocheap, digits = rounding_digits),
      round(psmdata$cheap, digits = rounding_digits),
      round(psmdata$expensive, digits = rounding_digits),
      round(psmdata$tooexpensive, digits = rounding_digits)
    )))
  } else {
    price_grid <- sort(unique(c(
      psmdata$toocheap, psmdata$cheap, 
      psmdata$expensive, psmdata$tooexpensive
    )))
  }
  
  # Initialize ECDF data frame
  data_ecdf <- data.frame(price = price_grid)
  
  # Calculate ECDFs based on analysis type
  if (weighted) {
    data_ecdf <- calculate_weighted_ecdfs(data_ecdf, survey_design)
  } else {
    data_ecdf <- calculate_unweighted_ecdfs(data_ecdf, psmdata)
  }
  
  # Apply interpolation if requested
  if (interpolate) {
    data_ecdf <- apply_interpolation(data_ecdf, interpolation_steps)
  }
  
  # Calculate derived ECDFs
  data_ecdf$ecdf_not_cheap <- 1 - data_ecdf$ecdf_cheap
  data_ecdf$ecdf_not_expensive <- 1 - data_ecdf$ecdf_expensive
  
  return(data_ecdf)
}

#' Calculate unweighted ECDFs
#' @noRd
calculate_unweighted_ecdfs <- function(data_ecdf, psmdata) {
  
  # Handle "too cheap" (may be all NA)
  if (!all(is.na(psmdata$toocheap))) {
    ecdf_toocheap <- ecdf(psmdata$toocheap)
    ecdf_values <- ecdf_toocheap(data_ecdf$price)
    # Ensure proper boundary behavior: values beyond max should give 1, beyond min should give 0
    ecdf_values[data_ecdf$price >= max(psmdata$toocheap, na.rm = TRUE)] <- 1
    ecdf_values[data_ecdf$price <= min(psmdata$toocheap, na.rm = TRUE)] <- 0
    data_ecdf$ecdf_toocheap <- 1 - ecdf_values
  } else {
    data_ecdf$ecdf_toocheap <- NA
  }
  
  # Calculate other ECDFs
  ecdf_cheap <- ecdf(psmdata$cheap)
  ecdf_values <- ecdf_cheap(data_ecdf$price)
  # Ensure proper boundary behavior: values beyond max should give 1, beyond min should give 0
  ecdf_values[data_ecdf$price >= max(psmdata$cheap, na.rm = TRUE)] <- 1
  ecdf_values[data_ecdf$price <= min(psmdata$cheap, na.rm = TRUE)] <- 0
  data_ecdf$ecdf_cheap <- 1 - ecdf_values
  
  ecdf_expensive <- ecdf(psmdata$expensive)
  ecdf_values <- ecdf_expensive(data_ecdf$price)
  # Ensure proper boundary behavior: values beyond max should give 1, beyond min should give 0
  ecdf_values[data_ecdf$price >= max(psmdata$expensive, na.rm = TRUE)] <- 1
  ecdf_values[data_ecdf$price <= min(psmdata$expensive, na.rm = TRUE)] <- 0
  data_ecdf$ecdf_expensive <- ecdf_values
  
  ecdf_tooexpensive <- ecdf(psmdata$tooexpensive)
  ecdf_values <- ecdf_tooexpensive(data_ecdf$price)
  # Ensure proper boundary behavior: values beyond max should give 1, beyond min should give 0
  ecdf_values[data_ecdf$price >= max(psmdata$tooexpensive, na.rm = TRUE)] <- 1
  ecdf_values[data_ecdf$price <= min(psmdata$tooexpensive, na.rm = TRUE)] <- 0
  data_ecdf$ecdf_tooexpensive <- ecdf_values
  
  return(data_ecdf)
}

#' Calculate weighted ECDFs using survey package
#' @noRd
calculate_weighted_ecdfs <- function(data_ecdf, survey_design) {
  
  # Get variable names from the design
  design_vars <- survey_design$variables
  
  # Handle "too cheap" (may be all NA)
  if ("toocheap" %in% names(design_vars) && !all(is.na(design_vars$toocheap))) {
    ecdf_toocheap <- survey::svycdf(formula = ~toocheap, design = survey_design)
    ecdf_values <- ecdf_toocheap$toocheap(data_ecdf$price)
    # Ensure proper boundary behavior and handle NAs
    ecdf_values[is.na(ecdf_values)] <- 0  
    ecdf_values[data_ecdf$price >= max(design_vars$toocheap, na.rm = TRUE)] <- 1
    ecdf_values[data_ecdf$price <= min(design_vars$toocheap, na.rm = TRUE)] <- 0
    data_ecdf$ecdf_toocheap <- 1 - ecdf_values
    # Ensure bounds are strictly between 0 and 1
    data_ecdf$ecdf_toocheap[data_ecdf$ecdf_toocheap < 0] <- 0
    data_ecdf$ecdf_toocheap[data_ecdf$ecdf_toocheap > 1] <- 1
  } else {
    data_ecdf$ecdf_toocheap <- NA
  }
  
  # Calculate other ECDFs - check which variables exist
  formula_vars <- character(0)
  if ("cheap" %in% names(design_vars)) formula_vars <- c(formula_vars, "cheap")
  if ("expensive" %in% names(design_vars)) formula_vars <- c(formula_vars, "expensive")
  if ("tooexpensive" %in% names(design_vars)) formula_vars <- c(formula_vars, "tooexpensive")
  
  if (length(formula_vars) > 0) {
    formula_str <- paste("~", paste(formula_vars, collapse = " + "))
    ecdf_others <- survey::svycdf(formula = as.formula(formula_str), design = survey_design)
    
    if ("cheap" %in% formula_vars) {
      ecdf_values <- ecdf_others$cheap(data_ecdf$price)
      # Ensure proper boundary behavior and handle NAs
      ecdf_values[is.na(ecdf_values)] <- 0  
      ecdf_values[data_ecdf$price >= max(design_vars$cheap, na.rm = TRUE)] <- 1
      ecdf_values[data_ecdf$price <= min(design_vars$cheap, na.rm = TRUE)] <- 0
      data_ecdf$ecdf_cheap <- 1 - ecdf_values
      # Ensure bounds are strictly between 0 and 1
      data_ecdf$ecdf_cheap[data_ecdf$ecdf_cheap < 0] <- 0
      data_ecdf$ecdf_cheap[data_ecdf$ecdf_cheap > 1] <- 1
    } else {
      data_ecdf$ecdf_cheap <- NA
    }

    if ("expensive" %in% formula_vars) {
      ecdf_values <- ecdf_others$expensive(data_ecdf$price)
      # Ensure proper boundary behavior and handle NAs
      ecdf_values[is.na(ecdf_values)] <- 0  
      ecdf_values[data_ecdf$price >= max(design_vars$expensive, na.rm = TRUE)] <- 1
      ecdf_values[data_ecdf$price <= min(design_vars$expensive, na.rm = TRUE)] <- 0
      data_ecdf$ecdf_expensive <- ecdf_values
      # Ensure bounds are strictly between 0 and 1
      data_ecdf$ecdf_expensive[data_ecdf$ecdf_expensive < 0] <- 0
      data_ecdf$ecdf_expensive[data_ecdf$ecdf_expensive > 1] <- 1
    } else {
      data_ecdf$ecdf_expensive <- NA
    }

    if ("tooexpensive" %in% formula_vars) {
      ecdf_values <- ecdf_others$tooexpensive(data_ecdf$price)
      # Ensure proper boundary behavior and handle NAs
      ecdf_values[is.na(ecdf_values)] <- 0  
      ecdf_values[data_ecdf$price >= max(design_vars$tooexpensive, na.rm = TRUE)] <- 1
      ecdf_values[data_ecdf$price <= min(design_vars$tooexpensive, na.rm = TRUE)] <- 0
      data_ecdf$ecdf_tooexpensive <- ecdf_values
      # Ensure bounds are strictly between 0 and 1
      data_ecdf$ecdf_tooexpensive[data_ecdf$ecdf_tooexpensive < 0] <- 0
      data_ecdf$ecdf_tooexpensive[data_ecdf$ecdf_tooexpensive > 1] <- 1
    } else {
      data_ecdf$ecdf_tooexpensive <- NA
    }
  } else {
    # No valid variables found
    data_ecdf$ecdf_cheap <- NA
    data_ecdf$ecdf_expensive <- NA
    data_ecdf$ecdf_tooexpensive <- NA
  }
  
  return(data_ecdf)
}

#' Apply interpolation to ECDF data
#' @noRd
apply_interpolation <- function(data_ecdf, interpolation_steps) {
  
  # Create interpolated price grid
  price_range <- range(data_ecdf$price, na.rm = TRUE)
  interpolated_prices <- seq(
    from = price_range[1],
    to = price_range[2],
    by = abs(interpolation_steps)
  )
  
  # Create new data frame with interpolated prices
  data_ecdf_smooth <- data.frame(price = interpolated_prices)
  
  # Merge with existing ECDF data
  data_ecdf_smooth <- merge(
    x = data_ecdf_smooth,
    y = data_ecdf,
    by = "price",
    all.x = TRUE
  )
  
  # Apply linear interpolation to each ECDF
  ecdf_columns <- c("ecdf_toocheap", "ecdf_cheap", "ecdf_expensive", "ecdf_tooexpensive")
  
  for (col in ecdf_columns) {
    if (col %in% names(data_ecdf)) {
      # FIXED: Check if we have sufficient non-NA values for interpolation
      non_na_values <- !is.na(data_ecdf[[col]])
      
      if (sum(non_na_values) >= 2) {
        # We have at least 2 non-NA values, proceed with interpolation
        data_ecdf_smooth[[col]] <- approx(
          x = data_ecdf$price[non_na_values],
          y = data_ecdf[[col]][non_na_values],
          xout = data_ecdf_smooth$price,
          method = "linear"
        )$y
      } else {
        # Not enough non-NA values for interpolation, use existing values
        warning(paste("Not enough non-NA values for interpolation of", col, 
                     ". Skipping interpolation for this variable."))
        # Use merge result as-is (will have NAs where interpolation would occur)
        if (!col %in% names(data_ecdf_smooth)) {
          data_ecdf_smooth[[col]] <- NA_real_
        }
      }
    }
  }
  
  return(data_ecdf_smooth)
}

#' Identify key price points from ECDF data
#' @param data_ecdf ECDF data frame
#' @param intersection_method Method for handling multiple intersections
#' @param acceptable_range Definition of acceptable price range
#' @noRd
identify_price_points <- function(data_ecdf, 
                                intersection_method = "min",
                                acceptable_range = "original") {
  
  if (acceptable_range == "original") {
    # Original van Westendorp definition
    pricerange_lower <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_not_cheap",
      var2 = "ecdf_toocheap",
      method = intersection_method
    )
    
    pricerange_upper <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_tooexpensive",
      var2 = "ecdf_not_expensive",
      method = intersection_method
    )
  } else {
    # Narrower definition
    pricerange_lower <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_expensive",
      var2 = "ecdf_toocheap",
      method = intersection_method
    )
    
    pricerange_upper <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_tooexpensive",
      var2 = "ecdf_cheap",
      method = intersection_method
    )
  }
  
  # Indifference Price Point (IDP)
  idp <- identify_intersection(
    data = data_ecdf,
    var1 = "ecdf_expensive",
    var2 = "ecdf_cheap",
    method = intersection_method
  )
  
  # Optimal Price Point (OPP)
  opp <- identify_intersection(
    data = data_ecdf,
    var1 = "ecdf_tooexpensive",
    var2 = "ecdf_toocheap",
    method = intersection_method
  )
  
  list(
    pricerange_lower = pricerange_lower,
    pricerange_upper = pricerange_upper,
    idp = idp,
    opp = opp
  )
}

#' Calculate Newton Miller Smith extension analysis
#' @param prepared_data Prepared PSM data structure
#' @param data_ecdf ECDF data frame
#' @param pi_calibrated_toocheap Calibrated probability for too cheap
#' @param pi_calibrated_tooexpensive Calibrated probability for too expensive
#' @noRd
calculate_nms_analysis <- function(prepared_data, data_ecdf,
                                 pi_calibrated_toocheap = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOCHEAP"),
                                 pi_calibrated_tooexpensive = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOEXPENSIVE")) {
  
  psmdata <- prepared_data$data
  weighted <- prepared_data$weighted
  survey_design <- prepared_data$survey_design
  
  # Set up price grid for NMS analysis
  nms_prices <- data_ecdf$price
  
  # Create matrix for purchase probabilities
  nms_matrix <- create_nms_matrix(psmdata, nms_prices, 
                                 pi_calibrated_toocheap, pi_calibrated_tooexpensive,
                                 weighted)
  
  # Interpolate purchase probabilities
  nms_matrix <- interpolate_nms_matrix(nms_matrix)
  
  # Calculate reach and revenue
  if (weighted && !is.null(survey_design)) {
    # FIXED: Ensure weights align with the data dimensions
    nms_weights <- weights(survey_design)
    
    # Check that weights match the number of rows in nms_matrix
    if (length(nms_weights) != nrow(nms_matrix)) {
      # If validation filtered the data, we need to adjust weights accordingly
      if (prepared_data$total_sample > nrow(psmdata) && 
          length(nms_weights) == prepared_data$total_sample) {
        # Find which cases were kept after validation
        original_indices <- as.numeric(rownames(psmdata))
        if (!is.null(original_indices) && all(!is.na(original_indices))) {
          nms_weights <- nms_weights[original_indices]
        } else {
          # Fallback: trim weights to match data
          warning("Weight adjustment needed - using first ", nrow(nms_matrix), " weights")
          nms_weights <- nms_weights[seq_len(nrow(nms_matrix))]
        }
      }
      
      # Final check - if still mismatched, use unweighted analysis
      if (length(nms_weights) != nrow(nms_matrix)) {
        warning("Unable to align survey weights with data. Using unweighted analysis for NMS.")
        reach_values <- apply(nms_matrix, 2, mean, na.rm = TRUE)
      } else {
        reach_values <- apply(nms_matrix, 2, stats::weighted.mean, w = nms_weights, na.rm = TRUE)
      }
    } else {
      reach_values <- apply(nms_matrix, 2, stats::weighted.mean, w = nms_weights, na.rm = TRUE)
    }
  } else {
    reach_values <- apply(nms_matrix, 2, mean, na.rm = TRUE)
  }
  
  # Create NMS results data frame
  data_nms <- data.frame(
    price = nms_prices,
    reach = reach_values,
    row.names = seq_along(nms_prices)
  )
  
  data_nms$revenue <- data_nms$price * data_nms$reach
  
  # Find optimal prices
  price_optimal_reach <- data_nms$price[which.max(data_nms$reach)]
  price_optimal_revenue <- data_nms$price[which.max(data_nms$revenue)]
  
  list(
    data_nms = data_nms,
    price_optimal_reach = price_optimal_reach,
    price_optimal_revenue = price_optimal_revenue
  )
}

#' Create NMS matrix with known purchase probabilities
#' @noRd
create_nms_matrix <- function(psmdata, nms_prices, pi_calibrated_toocheap, 
                             pi_calibrated_tooexpensive, weighted = FALSE) {
  
  # Initialize matrix
  nms_matrix <- matrix(
    nrow = nrow(psmdata), 
    ncol = length(nms_prices),
    dimnames = list(rownames(psmdata), as.character(nms_prices))
  )
  
  # Helper function to find price positions
  find_price_positions <- function(prices, weighted) {
    if (weighted) {
      rounding_digits <- get_psm_constant("ROUNDING_DIGITS")
      rounded_prices <- round(prices, digits = rounding_digits)
      positions <- sapply(as.character(rounded_prices), function(x) {
        pos <- which(colnames(nms_matrix) == x)
        if (length(pos) == 0) return(NA_integer_) else return(pos[1])
      })
    } else {
      positions <- sapply(as.character(prices), function(x) {
        pos <- which(colnames(nms_matrix) == x)
        if (length(pos) == 0) return(NA_integer_) else return(pos[1])
      })
    }
    return(positions)
  }
  
  # FIXED: Helper function to safely assign values to matrix
  safe_matrix_assign <- function(matrix, row_indices, col_positions, values) {
    # Only assign where column positions are not NA
    valid_positions <- !is.na(col_positions)
    if (any(valid_positions)) {
      valid_rows <- row_indices[valid_positions]
      valid_cols <- col_positions[valid_positions]
      
      # FIX: Handle single value case properly  
      if (length(values) == 1) {
        valid_values <- rep(values, length(valid_rows))
      } else {
        valid_values <- values[valid_positions]
      }
      
      matrix[cbind(valid_rows, valid_cols)] <- valid_values
    }
    return(matrix)
  }
  
  # Fill matrix with known values
  if (!all(is.na(psmdata$toocheap))) {
    pos_toocheap <- find_price_positions(psmdata$toocheap, weighted)
    nms_matrix <- safe_matrix_assign(nms_matrix, 
                                     seq_len(nrow(nms_matrix)), 
                                     pos_toocheap, 
                                     pi_calibrated_toocheap)
  }
  
  pos_tooexpensive <- find_price_positions(psmdata$tooexpensive, weighted)
  nms_matrix <- safe_matrix_assign(nms_matrix, 
                                   seq_len(nrow(nms_matrix)), 
                                   pos_tooexpensive, 
                                   pi_calibrated_tooexpensive)
  
  pos_cheap <- find_price_positions(psmdata$cheap, weighted)
  nms_matrix <- safe_matrix_assign(nms_matrix, 
                                   seq_len(nrow(nms_matrix)), 
                                   pos_cheap, 
                                   psmdata$pi_cheap_cal)
  
  pos_expensive <- find_price_positions(psmdata$expensive, weighted)
  nms_matrix <- safe_matrix_assign(nms_matrix, 
                                   seq_len(nrow(nms_matrix)), 
                                   pos_expensive, 
                                   psmdata$pi_expensive_cal)
  
  return(nms_matrix)
}

#' Construct PSM result object
#' 
#' @description Creates the final PSM result object with all analysis components.
#'   This function ensures consistent result structure across all analysis types.
#' 
#' @param prepared_data Prepared data structure from prepare_psm_data()
#' @param ecdf_data ECDF calculation results from calculate_ecdf_data()
#' @param price_points Identified price points from identify_price_points()
#' @param nms_results NMS analysis results (optional, from calculate_nms_analysis())
#' @param acceptable_range Acceptable range definition used
#' @param pi_scale Purchase intent scale (for NMS)
#' @param pi_calibrated Calibrated probabilities (for NMS)
#' 
#' @return Object of class "psm" with standardized structure
#' 
#' @keywords internal
construct_psm_result <- function(prepared_data, ecdf_data, price_points, nms_results,
                                acceptable_range, pi_scale, pi_calibrated) {
  
  # Base PSM result structure (identical to original)
  output_psm <- list(
    data_input = prepared_data$data,
    validated = length(prepared_data$invalid_cases) > 0 && prepared_data$invalid_cases < prepared_data$total_sample,
    invalid_cases = prepared_data$invalid_cases,
    total_sample = prepared_data$total_sample,
    data_vanwestendorp = ecdf_data,
    pricerange_lower = price_points$pricerange_lower,
    pricerange_upper = price_points$pricerange_upper,
    idp = price_points$idp,
    opp = price_points$opp,
    acceptable_range_definition = acceptable_range,
    weighted = prepared_data$weighted,
    nms = prepared_data$nms_requested
  )
  
  # Add survey design for weighted analysis
  if (prepared_data$weighted) {
    output_psm$survey_design <- prepared_data$survey_design
  }
  
  # Add NMS results if calculated
  if (prepared_data$nms_requested && !is.null(nms_results)) {
    output_psm$data_nms <- nms_results$data_nms
    output_psm$pi_scale <- data.frame(pi_scale = pi_scale, pi_calibrated = pi_calibrated)
    output_psm$price_optimal_reach <- nms_results$price_optimal_reach
    output_psm$price_optimal_revenue <- nms_results$price_optimal_revenue
  }
  
  # Set class and return
  class(output_psm) <- "psm"
  return(output_psm)
}
