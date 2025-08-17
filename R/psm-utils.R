# Utility Functions for the pricesensitivitymeter package that are *not* exported

#-------
# Internal Helper Function: Identify Intersection Point
# (with possibility to specify method in case there are multiple intersection points)

identify_intersection <- function(data, var1, var2, method) {
  first_intersection_pos <- which(data[, var1] >= data[, var2])[1]

  if (is.na(first_intersection_pos)) { # if no intersection: return NA
    return(NA)
  } else { # otherwise, run the actual function
    all_intersections_pos <- which(data[, var1] == data[first_intersection_pos, var1] &
                                     data[, var2] == data[first_intersection_pos, var2])

    all_intersections_prices <- data[all_intersections_pos, "price"]

    switch(method,
           min = {min(all_intersections_prices)},
           max = {max(all_intersections_prices)},
           mean = {mean(all_intersections_prices)},
           median = {median(all_intersections_prices)}
    )
  }
}

# Newton Miller Smith Extension: Interpolating the matrix with purchase probabilities

interpolate_nms_matrix <- function(nms_matrix) {

for (i in seq_len(nrow(nms_matrix))) {
  # Find all non-NA positions for this row
  non_na_positions <- which(!is.na(nms_matrix[i, ]))
  
  # Skip interpolation if we have fewer than 2 non-NA values
  if (length(non_na_positions) < 2) {
    next
  }
  
  # Initialize interpolated probabilities vector
  interpolate_prob <- NA
  
  # Try different interpolation strategies based on number of non-NA values
  if (length(non_na_positions) >= 4) {
    # Try linear interpolation between three pairs of values
    interpolate_prob <- try({
      pos1 <- non_na_positions[1]
      pos2 <- non_na_positions[2]
      pos3 <- non_na_positions[3]
      pos4 <- non_na_positions[4]
      
      # Check if positions are different enough for interpolation
      if (pos2 > pos1 && pos3 > pos2 && pos4 > pos3) {
        c(
          # first pair
          seq(from = nms_matrix[i, pos1], to = nms_matrix[i, pos2], 
              length.out = pos2 - pos1 + 1),
          # second pair (exclude first point to avoid duplication)
          seq(from = nms_matrix[i, pos2], to = nms_matrix[i, pos3], 
              length.out = pos3 - pos2 + 1)[-1],
          # third pair (exclude first point to avoid duplication)
          seq(from = nms_matrix[i, pos3], to = nms_matrix[i, pos4], 
              length.out = pos4 - pos3 + 1)[-1]
        )
      } else {
        stop("Positions too close for 4-point interpolation")
      }
    }, silent = TRUE)
  }

  # If 4-point interpolation failed or we have fewer than 4 points, try 2-point interpolation
  if (inherits(interpolate_prob, "try-error") || length(non_na_positions) < 4) {
    # Use first and last non-NA positions for simple linear interpolation
    pos1 <- non_na_positions[1]
    pos_last <- non_na_positions[length(non_na_positions)]
    
    if (pos_last > pos1) {
      interpolate_prob <- seq(from = nms_matrix[i, pos1], 
                             to = nms_matrix[i, pos_last], 
                             length.out = pos_last - pos1 + 1)
    } else {
      # If positions are the same, just use the single value
      interpolate_prob <- nms_matrix[i, pos1]
    }
  }

  # Write vector with interpolated values to matrix
  if (!inherits(interpolate_prob, "try-error") && !any(is.na(interpolate_prob))) {
    start_pos <- min(non_na_positions)
    end_pos <- max(non_na_positions)
    if (length(interpolate_prob) == (end_pos - start_pos + 1)) {
      nms_matrix[i, start_pos:end_pos] <- interpolate_prob
    }
  }
}

  # Purchase probabilities outside of the individual's personal price range must be set to zero
  nms_matrix[is.na(nms_matrix)] <- 0

  return(nms_matrix)
}
