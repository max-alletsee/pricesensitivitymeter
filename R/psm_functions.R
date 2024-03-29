#---------------------
# Implementing van Westendorp's PSM in R
#---------------------

psm_analysis <- function(toocheap, cheap, expensive, tooexpensive, data = NA,
                         validate = TRUE,
                         interpolate = FALSE, interpolation_steps = 0.01,
                         intersection_method = "min",
                         acceptable_range = "original",
                         pi_cheap = NA, pi_expensive = NA,
                         pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
                         pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0) {

  #---
  # 1) Input Check: Price Sensitivity Meter data
  #---

  # input check 1a: validate is required and must be boolean
  if (any(is.na(validate)) | !is.logical(validate) | length(validate) != 1) {
    stop("validate requires one logical value")
  }

  # input check 1b: interpolation is required and must be boolean
  if (any(is.na(interpolate)) | !is.logical(interpolate) | length(interpolate) != 1) {
    stop("interpolate requires one logical value")
  }

  # input check 1c: intersection_method must be one of the pre-defined terms
  match.arg(intersection_method, c("min", "max", "mean", "median"))

  # input check 1d: if interpolate == TRUE, interpolation steps must be numeric vector of length 1
  if (interpolate & (length(interpolation_steps) != 1 | !is.numeric(interpolation_steps))) {
    stop("interpolatation_steps must be numeric value (vector of length 1)")
  }


  # input check 1e: acceptable_range must be one of the pre-defined terms
  match.arg(acceptable_range, c("original", "narrower"))

  # input check 2: if data is provided in a dataset, structure and format must be correct

  if (!is.data.frame(data) & !is.matrix(data) & !all(is.na(data))) {
    stop("If the data argument is used, it must provide a data frame (or matrix) object")
  }

  if (is.data.frame(data) | is.matrix(data)) {
    if (!is.character(toocheap) | !is.character(cheap) | !is.character(expensive) | !is.character(tooexpensive) |
      length(toocheap) != 1 | length(cheap) != 1 | length(expensive) != 1 | length(tooexpensive) != 1) {
      stop("If the data argument is used, all price arguments (toocheap, cheap, expensive, tooexpensive) must be character values that contain the name of the respective price variable in the data object")
    }

    # identify columns in data object that are supposed to contain the price variables
    if (length(toocheap) == 1 & length(cheap) == 1 & length(expensive) == 1 & length(tooexpensive) == 1) {
      col_toocheap <- match(toocheap, colnames(data))
      col_cheap <- match(cheap, colnames(data))
      col_expensive <- match(expensive, colnames(data))
      col_tooexpensive <- match(tooexpensive, colnames(data))
    }

    if (is.na(col_toocheap) | is.na(col_cheap) | is.na(col_expensive) | is.na(col_tooexpensive)) {
      stop("Could not find all variable names of the price variables (toocheap, cheap, expensive, tooexpensive) in the data object")
    }

    if (ifelse(!is.numeric(data[, toocheap]), !all(is.na(data[, toocheap])), FALSE) | !is.numeric(data[, cheap]) |
      !is.numeric(data[, expensive]) | !is.numeric(data[, tooexpensive])) {
      stop("All price variables (toocheap, cheap, expensive, tooexpensive) must contain only numeric values\n(toocheap is also tolerated if all values are NA)")
    }

    # if input structure of data is valid, create internal dataframe for PSM function
    psmdata <- data.frame(
      toocheap = data[, toocheap],
      cheap = data[, cheap],
      expensive = data[, expensive],
      tooexpensive = data[, tooexpensive]
    )
  }

  # input check 3: if no dataset is provided, all price variables must be numeric vectors that have the same length
  if (!is.data.frame(data) & !is.matrix(data) & all(is.na(data))) {
    if (!is.vector(toocheap) | !is.vector(cheap) | !is.vector(expensive) | !is.vector(tooexpensive) |
      ifelse(!all(is.na(toocheap)), !is.numeric(toocheap), FALSE) | !is.numeric(cheap) | !is.numeric(expensive) |
      !is.numeric(tooexpensive)) {
      stop("If the data argument is not provided, all price variables (toocheap, cheap, expensive, tooexpensive) must be numeric vectors\n(toocheap is also tolerated if all values are NA)")
    }

    if (!(length(toocheap) == length(cheap)) |
      !(length(toocheap) == length(expensive)) |
      !(length(toocheap) == length(tooexpensive))) {
      stop("All price arguments must have the same length")
    }

    # if input structure of data is valid, create internal dataframe for PSM function
    psmdata <- data.frame(toocheap, cheap, expensive, tooexpensive)
  }

  #---
  # 2) Input check: Newton Miller Smith Extension data
  #---

  nms <- !all(is.na(pi_cheap)) & !all(is.na(pi_expensive))

  # input check 4: both purchase intent variables must have the same length
  if (length(pi_cheap) != length(pi_expensive)) {
    stop("If purchase intent data is provided, this data must have the same number of respondents for the cheap and for the expensive price")
  }

  # input check 5a: NMS extension data if no data frame is provided
  if (all(is.na(data)) & isTRUE(nms)) {

    # purchase intent data must have same length as PSM data
    if (length(cheap) != length(pi_cheap)) {
      stop("Unequal number of respondents for PSM data and for purchase intent variables")
    }

    # purchase intent input must be vectors
    if (!is.vector(pi_cheap) | !is.vector(pi_expensive)) {
      stop("If the data argument is not provided, both purchase intent variables (pi_cheap, pi_expensive) must be vectors")
    }

    psmdata$pi_cheap <- pi_cheap
    psmdata$pi_expensive <- pi_expensive
  }

  # input check 5b: nms extension data if data frame is provided - check for matching variable names
  if (!all(is.na(data)) & isTRUE(nms)) {
    col_pi_cheap <- match(pi_cheap, colnames(data))
    col_pi_expensive <- match(pi_expensive, colnames(data))

    if (is.na(col_pi_cheap) | is.na(col_pi_expensive)) {
      stop("Could not find all variable names of the purchase intent variables (pi_cheap, pi_expensive) in the data object")
    }

    psmdata$pi_cheap <- data[, col_pi_cheap]
    psmdata$pi_expensive <- data[, col_pi_expensive]
  }

  # input check 6: for each value on the purchase intent scale, there must be a corresponding calibration value
  stopifnot(length(pi_scale) == length(pi_calibrated))

  # input check 7: purchase intent data must only contain values from the pre-defined scale
  if (isTRUE(nms)) {
    # check that purchase intent data and scale have the same class (special handling for integer vs. numeric vs. double)
    if (!identical(x = class(psmdata$pi_cheap), y = class(pi_scale)) &
      !(is.numeric(psmdata$pi_cheap) & is.numeric(pi_scale))) {
      stop("pi_cheap and pi_scale must both be numeric")
    }

    if (!identical(x = class(psmdata$pi_expensive), y = class(pi_scale)) &
      !(is.numeric(psmdata$pi_expensive) & is.numeric(pi_scale))) {
      stop("pi_expensive and pi_scale must both be numeric")
    }

    # check that all purchase intent data only includes values from the pre-defined scale
    if (!all(unique(psmdata$pi_cheap) %in% unique(pi_scale))) {
      stop("pi_cheap contains values which are not defined in the pi_scale variable")
    }

    if (!all(unique(psmdata$pi_expensive) %in% unique(pi_scale))) {
      stop("pi_expensive contains values which are not defined in the pi_scale variable")
    }

    # input check 8: calibration values must be numeric
    if (any(!is.numeric(pi_calibrated))) {
      stop("All calibrated purchase intent values must be numeric")
    }

    # input check 9: calibration values must be between 0 and 1 - only warning if this is not the case...
    if (any(is.nan(pi_calibrated))) {
      stop("Some of the purchase intent calibration values are not a number (NaN)")
    }

    if (any(is.infinite(pi_calibrated))) {
      stop("Some of the purchase intent calibration values are infinite (-Inf, Inf).")
    }

    if (any(pi_calibrated < 0)) {
      warning("Some of the purchase intent calibration values are smaller than 0. It seems that this is not a probability between 0 and 1. The interpretation of the reach/revenue values is not recommended.")
    }

    if (any(pi_calibrated > 1)) {
      warning("Some of the purchase intent calibration values are larger than 1. It seems that this is not a probability between 0 and 1. The interpretation of the reach/revenue values is not recommended.")
    }
  }


  #-----
  # 3) Validation of response patterns answers and optional cleaning of data set
  #-----

  # validation: "too cheap < cheap < expensive < too expensive" for each case. if not, drop from the data
  # consider special case of data without "too cheap" values for all respondents
  if (all(is.na(psmdata$toocheap))) { # if "too cheap" is missing: ignore for validation
    psmdata$valid <- psmdata$tooexpensive > psmdata$expensive & psmdata$expensive > psmdata$cheap
    # set to invalid if any NAs
    psmdata$valid[which(is.na(psmdata$tooexpensive) | is.na(psmdata$expensive) | is.na(psmdata$cheap))] <- FALSE
  } else { # if "too cheap" is included: consider in validation
    psmdata$valid <- psmdata$tooexpensive > psmdata$expensive & psmdata$expensive > psmdata$cheap & psmdata$cheap > psmdata$toocheap
    # set to invalid if any NAs
    psmdata$valid[which(is.na(psmdata$tooexpensive) | is.na(psmdata$expensive) | is.na(psmdata$cheap) | is.na(psmdata$toocheap))] <- FALSE
  }


  if (any(psmdata$valid == FALSE) & !isTRUE(validate)) {
    warning("Some respondents' price structures might not be consistent (i.e. different from too cheap < cheap < expensive < too expensive). Consider running this function with the additional option 'validate == TRUE' to analyse only the subset of respondents with consistent price structure.")
  }

  # save values for return function later
  invalid_cases <- nrow(psmdata) - sum(psmdata$valid)
  total_sample <- nrow(psmdata)

  if (total_sample == invalid_cases) {
    stop("All respondents have intransitive preference structures (i.e. different from too cheap < cheap < expensive < too expensive).")
  }

  if (isTRUE(validate) & !isTRUE(nms)) {
    psmdata <- subset(psmdata, psmdata$valid, select = c("toocheap", "cheap", "expensive", "tooexpensive"))
  }

  if (isTRUE(validate) & isTRUE(nms)) {
    psmdata <- subset(psmdata, psmdata$valid, select = c("toocheap", "cheap", "expensive", "tooexpensive", "pi_cheap", "pi_expensive"))
  }

  #-----
  # 4) Creating the empirical cumulative density per price
  #-----

  # new data set: 1st variable shows all prices, other variables show the respective cumulative density
  data_ecdf <- data.frame(price = sort(unique(c(psmdata$toocheap, psmdata$cheap, psmdata$expensive, psmdata$tooexpensive))))

  # empirical cumulative density for "too cheap" (ignore if no "too cheap" values provided)
  if (!all(is.na(psmdata$toocheap))) { # if there are values: first as a function
    ecdf_psm <- ecdf(psmdata$toocheap)
    # ... apply the function to all prices (1 - f(x) because the function is reversed in the original paper)
    data_ecdf$ecdf_toocheap <- 1 - ecdf_psm(data_ecdf$price)
  } else { # if no "too cheap" values provided: set to NA
    data_ecdf$ecdf_toocheap <- NA
  }

  # same for "cheap", "expensive", and "too expensive"
  # "cheap" is also reversed in the original paper, "expensive" and "too expensive" are not
  ecdf_psm <- ecdf(psmdata$cheap)
  data_ecdf$ecdf_cheap <- 1 - ecdf_psm(data_ecdf$price)

  ecdf_psm <- ecdf(psmdata$expensive)
  data_ecdf$ecdf_expensive <- ecdf_psm(data_ecdf$price)

  ecdf_psm <- ecdf(psmdata$tooexpensive)
  data_ecdf$ecdf_tooexpensive <- ecdf_psm(data_ecdf$price)

  # if interpolation is enabled: create bigger dataframe that contains all the actual price information plus price steps according to the interpolation_steps parameter
  if (isTRUE(interpolate)) {
    data_ecdf_smooth <- data.frame(price = seq(
      from = min(data_ecdf$price),
      to = max(data_ecdf$price),
      by = abs(interpolation_steps)
    ))

    # merge with existing dataframe incl. information on empirical cumulative density functions
    data_ecdf_smooth <- merge(
      x = data_ecdf_smooth,
      y = data_ecdf,
      by = "price",
      all.x = TRUE
    )

    # linear interpolation with the approx function for all empirical cumulative density functions
    data_ecdf_smooth$ecdf_toocheap <- try(approx(
      x = data_ecdf$price,
      y = data_ecdf$ecdf_toocheap,
      xout = data_ecdf_smooth$price,
      method = "linear"
    )$y)

    data_ecdf_smooth$ecdf_cheap <- approx(
      x = data_ecdf$price,
      y = data_ecdf$ecdf_cheap,
      xout = data_ecdf_smooth$price,
      method = "linear"
    )$y

    data_ecdf_smooth$ecdf_expensive <- approx(
      x = data_ecdf$price,
      y = data_ecdf$ecdf_expensive,
      xout = data_ecdf_smooth$price,
      method = "linear"
    )$y

    data_ecdf_smooth$ecdf_tooexpensive <- approx(
      x = data_ecdf$price,
      y = data_ecdf$ecdf_tooexpensive,
      xout = data_ecdf_smooth$price,
      method = "linear"
    )$y

    # replacing the old data_ecdf with its new smoothed version
    data_ecdf <- data_ecdf_smooth
  }

  # "not cheap" and "not expensive" for identifying the acceptable price range
  data_ecdf$ecdf_not_cheap <- 1 - data_ecdf$ecdf_cheap
  data_ecdf$ecdf_not_expensive <- 1 - data_ecdf$ecdf_expensive


  #-----
  # 5) Identifying the price points
  #-----

  if (acceptable_range=="original") {

    # price range, lower bound: intersection of "too cheap" and "not cheap"
    pricerange_lower <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_not_cheap",
      var2 = "ecdf_toocheap",
      method = intersection_method
    )

    # price range, upper bound: intersection of "not expensive" and "too expensive"
    pricerange_upper <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_tooexpensive",
      var2 = "ecdf_not_expensive",
      method = intersection_method
    )
  } else {
    # price range, lower bound: intersection of "too cheap" and "expensive"
    pricerange_lower <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_expensive",
      var2 = "ecdf_toocheap",
      method = intersection_method
    )

    # price range, upper bound: intersection of "cheap" and "too expensive"
    pricerange_upper <- identify_intersection(
      data = data_ecdf,
      var1 = "ecdf_tooexpensive",
      var2 = "ecdf_cheap",
      method = intersection_method
    )
  }

  # indifference price point IDP: intersection of "expensive" and "cheap"
  # interpretation: a) median price paid by consumer or b) price of the product of an important market leader
  idp <- identify_intersection(
    data = data_ecdf,
    var1 = "ecdf_expensive",
    var2 = "ecdf_cheap",
    method = intersection_method
  )

  # optimal price point OPP: intersection of "too expensive" and "too cheap"
  # interpretation: resistance against the price of a product is very low
  opp <- identify_intersection(
    data = data_ecdf,
    var1 = "ecdf_tooexpensive",
    var2 = "ecdf_toocheap",
    method = intersection_method
  )

  #-----
  # 6) Newton/Miller/Smith extension
  #-----

  # big if clause: run this whole section only if there is actually purchase intent data (which is required for the nms extension)
  if (!all(is.na(pi_cheap)) & !all(is.na(pi_expensive))) {
    # assign each respondent the calibrated probability of purchase
    psmdata$pi_cheap_cal <- NA
    psmdata$pi_expensive_cal <- NA

    for (i in seq_len(length(pi_scale))) {
      psmdata$pi_cheap_cal[which(psmdata$pi_cheap == pi_scale[i])] <- pi_calibrated[i]
      psmdata$pi_expensive_cal[which(psmdata$pi_expensive == pi_scale[i])] <- pi_calibrated[i]
    }


    # set up respondent-level data for the price steps
    nms_prices <- data_ecdf$price


    # create a matrix: each row is one respondent, each column is one (unique) price
    nms_matrix <- matrix(
      nrow = nrow(psmdata), ncol = length(nms_prices),
      dimnames = list(rownames(psmdata), nms_prices)
    )

    # fill matrix with known values:
    # 1) purchase probability of 0 for "too cheap" and "too expensive"
    # 2) weighted purchase probability for "cheap" and "expensive"

    pos_toocheap <- sapply(as.character(psmdata$toocheap), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(seq_len(nrow(nms_matrix)), as.numeric(pos_toocheap))] <- pi_calibrated_toocheap

    pos_tooexpensive <- sapply(as.character(psmdata$tooexpensive), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(seq_len(nrow(nms_matrix)), as.numeric(pos_tooexpensive))] <- pi_calibrated_tooexpensive

    pos_cheap <- sapply(as.character(psmdata$cheap), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(seq_len(nrow(nms_matrix)), as.numeric(pos_cheap))] <- psmdata$pi_cheap_cal

    pos_expensive <- sapply(as.character(psmdata$expensive), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(seq_len(nrow(nms_matrix)), as.numeric(pos_expensive))] <- psmdata$pi_expensive_cal

    # gradual interpolation of purchase probabilities

    nms_matrix <- interpolate_nms_matrix(nms_matrix)

    # analysis of reach and revenue (mean reach for each price)

    data_nms <- data.frame(
      price = nms_prices,
      reach = apply(nms_matrix, 2, mean),
      row.names = seq_len(length(nms_prices))
    )

    data_nms$revenue <- data_nms$price * data_nms$reach

    price_optimal_reach <- data_nms$price[which.max(data_nms$reach)]
    price_optimal_revenue <- data_nms$price[which.max(data_nms$revenue)]
  }

  #-----
  # 7) Construct the object to be returned
  #-----

  output_psm <- list(
    data_input = psmdata,
    validated = validate,
    invalid_cases = invalid_cases,
    total_sample = total_sample,
    data_vanwestendorp = data_ecdf,
    pricerange_lower = pricerange_lower,
    pricerange_upper = pricerange_upper,
    idp = idp,
    opp = opp,
    acceptable_range_definition = acceptable_range,
    weighted = FALSE,
    nms = nms
  )

  # if nms analysis was run: amend additional nms outputs
  if (isTRUE(nms)) {
    output_psm$data_nms <- data_nms
    output_psm$pi_scale <- data.frame(pi_scale, pi_calibrated)
    output_psm$price_optimal_reach <- price_optimal_reach
    output_psm$price_optimal_revenue <- price_optimal_revenue
  }

  class(output_psm) <- "psm"

  return(output_psm)
}
