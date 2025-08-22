#' @title Utility Functions for PSM Analysis
#' @description Helper functions for intersection identification and matrix interpolation
#' @keywords internal

#' Identify intersection point between two ECDF curves
#' @param data ECDF data frame
#' @param var1 First variable name
#' @param var2 Second variable name  
#' @param method Method for handling multiple intersections ("min", "max", "mean")
#' @return Intersection price point
#' @keywords internal
identify_intersection <- function(data, var1, var2, method = "min") {
  
  # Check if both variables exist and have valid data
  if (!var1 %in% names(data) || !var2 %in% names(data)) {
    return(NA_real_)
  }
  
  # Get the curves
  curve1 <- data[[var1]]
  curve2 <- data[[var2]]
  
  # Handle cases where one or both curves are all NA
  if (all(is.na(curve1)) || all(is.na(curve2))) {
    return(NA_real_)
  }
  
  # Find intersection points by looking for sign changes in the difference
  diff_curves <- curve1 - curve2
  
  # Remove NA values for intersection detection
  valid_indices <- !is.na(diff_curves)
  if (sum(valid_indices) < 2) {
    return(NA_real_)
  }
  
  valid_diff <- diff_curves[valid_indices]
  valid_prices <- data$price[valid_indices]
  
  # Find sign changes (intersections)
  sign_changes <- which(diff(sign(valid_diff)) != 0)
  
  if (length(sign_changes) == 0) {
    # No intersections found - return NA
    return(NA_real_)
  }
  
  # Calculate intersection prices using linear interpolation
  intersection_prices <- numeric(length(sign_changes))
  
  for (i in seq_along(sign_changes)) {
    idx <- sign_changes[i]
    
    # Linear interpolation between the two points
    x1 <- valid_prices[idx]
    x2 <- valid_prices[idx + 1]
    y1 <- valid_diff[idx]
    y2 <- valid_diff[idx + 1]
    
    # Find x where y = 0 (intersection)
    if (y2 != y1) {
      intersection_prices[i] <- x1 - y1 * (x2 - x1) / (y2 - y1)
    } else {
      intersection_prices[i] <- (x1 + x2) / 2
    }
  }
  
  # Apply method for handling multiple intersections
  if (length(intersection_prices) == 1) {
    return(intersection_prices[1])
  } else {
    switch(method,
           "min" = min(intersection_prices),
           "max" = max(intersection_prices),
           "mean" = mean(intersection_prices),
           min(intersection_prices)  # default to min
    )
  }
}

#' Interpolate NMS matrix to fill missing purchase probabilities
#' @param nms_matrix Matrix with known purchase probabilities
#' @return Matrix with interpolated values
#' @keywords internal
interpolate_nms_matrix <- function(nms_matrix) {
  
  # For each row (respondent), interpolate missing values
  for (i in seq_len(nrow(nms_matrix))) {
    row_data <- nms_matrix[i, ]
    
    # Find non-NA values
    non_na_indices <- which(!is.na(row_data))
    
    if (length(non_na_indices) >= 2) {
      # We have at least 2 known values, can interpolate
      prices <- as.numeric(colnames(nms_matrix))
      known_prices <- prices[non_na_indices]
      known_values <- row_data[non_na_indices]
      
      # Interpolate for all price points
      interpolated_values <- approx(
        x = known_prices,
        y = known_values,
        xout = prices,
        method = "linear",
        rule = 2  # Use nearest value for extrapolation
      )$y
      
      nms_matrix[i, ] <- interpolated_values
      
    } else if (length(non_na_indices) == 1) {
      # Only one known value - use constant extrapolation
      nms_matrix[i, ] <- row_data[non_na_indices[1]]
      
    } else {
      # No known values - use 0 as default
      nms_matrix[i, ] <- 0
    }
  }
  
  return(nms_matrix)
}
