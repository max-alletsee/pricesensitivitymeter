#---------------------
# Implementing van Westendorp's PSM in R
#---------------------

psm_analysis <- function(toocheap, cheap, expensive, tooexpensive, data = NA,
                         validate = TRUE, interpolate = FALSE,
                         pi_cheap = NA, pi_expensive = NA,
                         pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0)) {

  #---
  # 1) Input Check: Price Sensitivity Meter data
  #---

  # input check 1a: validate is required and must be boolean
  if(any(is.na(validate)) | !is.logical(validate) | length(validate) != 1) {
    stop("validate requires one logical value")
  }

  # input check 1b: interpolation is required and must be boolean
  if(any(is.na(interpolate)) | !is.logical(interpolate) | length(interpolate) != 1) {
    stop("interpolate requires one logical value")
  }

  # input check 2: if data is provided in a dataset, structure and format must be correct

  if(!is.data.frame(data) & !is.matrix(data) & !all(is.na(data))) {
    stop("If the data argument is used, it must provide a data frame (or matrix) object")

    if(!is.character(toocheap) | !is.character(cheap) | !is.character(expensive) | !is.character(tooexpensive) |
       length(toocheap) != 1 | length(cheap) != 1 | length(expensive) != 1 | length(tooexpensive) != 1) {
      stop("If the data argument is used, all price arguments (toocheap, cheap, expensive, tooexpensive) must be character values that contain the name of the respective price variable in the data object")
    }
  }


  if(is.data.frame(data) | is.matrix(data)) {
    # identify columns in data object that are supposed to contain the price variables
    if(length(toocheap) == 1 & length(cheap) == 1 & length(expensive) == 1 & length(tooexpensive) == 1) {
    col_toocheap <- match(toocheap, colnames(data))
    col_cheap <- match(cheap, colnames(data))
    col_expensive <- match(expensive, colnames(data))
    col_tooexpensive <- match(tooexpensive, colnames(data))
    }

    if(is.na(col_toocheap) | is.na(col_cheap) | is.na(col_expensive) | is.na(col_tooexpensive)) {
      stop("Could not find all variable names of the price variables (toocheap, cheap, expensive, tooexpensive) in the data object")
    }

    if(ifelse(!is.numeric(data[, col_toocheap]), !all(is.na(data[, col_toocheap])), FALSE) | !is.numeric(data[, col_cheap]) |
       !is.numeric(data[, col_expensive]) | !is.numeric(data[, col_tooexpensive])) {
      stop("All price variables (toocheap, cheap, expensive, tooexpensive) must contain only numeric values\n(toocheap is also tolerated if all values are NA)")
    }

    # if input structure of data is valid, create internal dataframe for PSM function
    psmdata <- data.frame(toocheap = data[, col_toocheap],
                          cheap = data[, col_cheap],
                          expensive = data[, col_expensive],
                          tooexpensive = data[, col_tooexpensive])
  }

  # input check 3: if no dataset is provided, all price variables must be numeric vectors that have the same length
  if(!is.data.frame(data) & !is.matrix(data) & all(is.na(data))) {
    if(!is.vector(toocheap) | !is.vector(cheap) | !is.vector(expensive)  | !is.vector(tooexpensive) |
       ifelse(!all(is.na(toocheap)), !is.numeric(toocheap), FALSE) | !is.numeric(cheap) | !is.numeric(expensive) |
       !is.numeric(tooexpensive)) {
      stop("If the data argument is not provided, all price variables (toocheap, cheap, expensive, tooexpensive) must be numeric vectors\n(toocheap is also tolerated if all values are NA)")
    }

    if(!(length(toocheap) == length(cheap)) |
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

  NMS <- !all(is.na(pi_cheap)) & !all(is.na(pi_expensive))

  # input check 4: both purchase intent variables must have the same length
  if(length(pi_cheap) != length(pi_expensive)) {
    stop("If purchase intent data is provided, this data must have the same number of respondents for the cheap and for the expensive price")
  }

  # input check 5a: NMS extension data if no data frame is provided
  if(all(is.na(data)) & isTRUE(NMS)) {

    # purchase intent data must have same length as PSM data
    if(length(cheap) != length(pi_cheap)) {
      stop("Unequal number of respondents for PSM data and for purchase intent variables")
    }

    # purchase intent input must be vectors
    if(!is.vector(pi_cheap) | !is.vector(pi_expensive)) {
      stop("If the data argument is not provided, both purchase intent variables (pi_cheap, pi_expensive) must be vectors")
    }

    psmdata$pi_cheap <- pi_cheap
    psmdata$pi_expensive <- pi_expensive
  }

  # input check 5b: NMS extension data if data frame is provided - check for matching variable names
  if(!all(is.na(data)) & isTRUE(NMS)) {
    col_pi_cheap <- match(pi_cheap, colnames(data))
    col_pi_expensive <- match(pi_expensive, colnames(data))

    if(is.na(col_pi_cheap) | is.na(col_pi_expensive)) {
      stop("Could not find all variable names of the purchase intent variables (pi_cheap, pi_expensive) in the data object")
    }

    psmdata$pi_cheap <- data[, col_pi_cheap]
    psmdata$pi_expensive <- data[, col_pi_expensive]

  }

  # input check 6: for each value on the purchase intent scale, there must be a corresponding calibration value
  stopifnot(length(pi_scale) == length(pi_calibrated))

  # input check 7: purchase intent data must only contain values from the pre-defined scale
  if(isTRUE(NMS)) {
    # check that purchase intent data and scale have the same class (special handling for integer vs. numeric vs. double)
    if(!identical(x = class(psmdata$pi_cheap), y = class(pi_scale)) &
       !(is.numeric(psmdata$pi_cheap) & is.numeric(pi_scale))) {
      stop("pi_cheap and pi_scale must have the same class")
    }

    if(!identical(x = class(psmdata$pi_expensive), y = class(pi_scale)) &
       !(is.numeric(psmdata$pi_expensive) & is.numeric(pi_scale))) {
      stop("pi_expensive and pi_scale must have the same class")
    }

    # check that all purchase intent data only includes values from the pre-defined scale
    if(!all(unique(psmdata$pi_cheap) %in% unique(pi_scale))) {
      stop("pi_cheap contains values which are not defined in the pi_scale variable")
    }

    if(!all(unique(psmdata$pi_expensive) %in% unique(pi_scale))) {
      stop("pi_expensive contains values which are not defined in the pi_scale variable")
    }

    # input check 8: calibration values must be numeric
    if(any(!is.numeric(pi_calibrated))) {
      stop("All calibrated purchase intent values must be numeric")
    }

    # input check 9: calibration values must be between 0 and 1 - only warning if this is not the case...
    if(any(pi_calibrated < 0)) {
      warning("Some of the purchase intent calibration values are smaller than 0. It seems that this is not a probability between 0 and 1. The interpretation of the trial/revenue values is not recommended.")
    }

    if(any(pi_calibrated > 1)) {
      warning("Some of the purchase intent calibration values are larger than 1. It seems that this is not a probability between 0 and 1. The interpretation of the trial/revenue values is not recommended.")
    }

    if(any(is.nan(pi_calibrated))) {
      stop("Some of the purchase intent calibration values are not a number (NaN)")
    }

    if(any(is.infinite(pi_calibrated))) {
      stop("Some of the purchase intent calibration values are infinite (-Inf, Inf).")
    }
  }


  #-----
  # 3) Validation of response patterns answers and optional cleaning of data set
  #-----

  # validation: "too cheap < cheap < expensive < too expensive" for each case. if not, drop from the data
  # consider special case of data without "too cheap" values for all respondents
  if(all(is.na(psmdata$toocheap))) { # if "too cheap" is missing: ignore for validation
    psmdata$valid <- psmdata$tooexpensive > psmdata$expensive & psmdata$expensive > psmdata$cheap
    # set to invalid if any NAs
    psmdata$valid[which(is.na(psmdata$tooexpensive) | is.na(psmdata$expensive) | is.na(psmdata$cheap))] <- FALSE
  } else { # if "too cheap" is included: consider in validation
    psmdata$valid <- psmdata$tooexpensive > psmdata$expensive & psmdata$expensive > psmdata$cheap & psmdata$cheap > psmdata$toocheap
    # set to invalid if any NAs
    psmdata$valid[which(is.na(psmdata$tooexpensive) | is.na(psmdata$expensive) | is.na(psmdata$cheap) | is.na(psmdata$toocheap))] <- FALSE
  }


  if(any(psmdata$valid == FALSE) & !isTRUE(validate)) {
    warning("Some respondents' price structures might not be consistent (i.e. different from too cheap < cheap < expensive < too expensive). Consider running this function with the additional option 'validate == TRUE' to analyse only the subset of respondents with consistent price structure.")
  }

  # save values for return function later
  invalid_cases <- nrow(psmdata) - sum(psmdata$valid)
  total_sample <- nrow(psmdata)

  if(total_sample == invalid_cases) {
    stop("All respondents have intransitive preference structures (i.e. different from too cheap < cheap < expensive < too expensive).")
  }

  if (isTRUE(validate) & !isTRUE(NMS)) {
    psmdata <- subset(psmdata, psmdata$valid, select = c("toocheap", "cheap", "expensive", "tooexpensive"))
  }

  if (isTRUE(validate) & isTRUE(NMS)) {
    psmdata <- subset(psmdata, psmdata$valid, select = c("toocheap", "cheap", "expensive", "tooexpensive", "pi_cheap", "pi_expensive"))
  }

  #-----
  # 4) Creating the empirical cumulative density per price
  #-----

  # new data set: 1st variable shows all prices, other variables show the respective cumulative density
  data_ecdf <- data.frame(price = sort(unique(c(psmdata$toocheap, psmdata$cheap, psmdata$expensive, psmdata$tooexpensive))))

  # empirical cumulative density for "too cheap" (ignore if no "too cheap" values provided)
  if(!all(is.na(psmdata$toocheap))) { # if there are values: first as a function
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

  # if interpolation is enabled: create bigger dataframe that contains all the actual price information plus fixed price steps of 0.01 between those values
  if(isTRUE(interpolate)) {
    data_ecdf_smooth <- data.frame(price = seq(from = min(data_ecdf$price),
                                               to = max(data_ecdf$price),
                                               by = 0.01))

    # merge with existing dataframe incl. information on empirical cumulative density functions
    data_ecdf_smooth <- merge(x = data_ecdf_smooth,
                              y = data_ecdf,
                              by = "price",
                              all.x = TRUE)

    # linear interpolation with the approx function for all empirical cumulative density functions
    data_ecdf_smooth$ecdf_toocheap <- approx(x = data_ecdf$price,
                                             y = data_ecdf$ecdf_toocheap,
                                             xout = data_ecdf_smooth$price,
                                             method = "linear")$y

    data_ecdf_smooth$ecdf_cheap <- approx(x = data_ecdf$price,
                                          y = data_ecdf$ecdf_cheap,
                                          xout = data_ecdf_smooth$price,
                                          method = "linear")$y

    data_ecdf_smooth$ecdf_expensive <- approx(x = data_ecdf$price,
                                              y = data_ecdf$ecdf_expensive,
                                              xout = data_ecdf_smooth$price,
                                              method = "linear")$y

    data_ecdf_smooth$ecdf_tooexpensive <- approx(x = data_ecdf$price,
                                                 y = data_ecdf$ecdf_tooexpensive,
                                                 xout = data_ecdf_smooth$price,
                                                 method = "linear")$y

    # replacing the old data_ecdf with its new smoothed version
    data_ecdf <- data_ecdf_smooth
  }

# "not cheap" and "not expensive" for identifying the acceptable price range
  data_ecdf$ecdf_not_cheap <-  1 - data_ecdf$ecdf_cheap
  data_ecdf$ecdf_not_expensive <-  1 - data_ecdf$ecdf_expensive


  #-----
  # 5) Identifying the price points
  #-----

  # price range, lower bound: intersection of "too cheap" and "not cheap"
  # first value where the CDF of the "not cheap" curve is at least as large as the CDF of the "too cheap" curve
  pricerange_lower <- data_ecdf$price[which(data_ecdf$ecdf_not_cheap >= data_ecdf$ecdf_toocheap)[1]]

  # price range, upper bound: intersection of "not expensive" and "too expensive"
  # first value where the CDF of the "too expensive" curve is at least as large as the the CDF of the "not expensive" curve
  pricerange_upper <- data_ecdf$price[which(data_ecdf$ecdf_tooexpensive >= data_ecdf$ecdf_not_expensive)[1]]

  # indifference price point IDP: intersection of "expensive" and "cheap"
  # equal number of people experience product as "cheap" and "expensive"
  # interpretation: a) median price paid by consumer or b) price of the product of an important market leader
  idp <- data_ecdf$price[which(data_ecdf$ecdf_expensive >= data_ecdf$ecdf_cheap)[1]]

  # optimal price point OPP: intersection of "too expensive" and "too cheap"
  # equal number of people regard the product as "too cheap" and "too expensive"
  # interpretation: resistance against the price of a product is very low
  opp <- data_ecdf$price[which(data_ecdf$ecdf_tooexpensive >= data_ecdf$ecdf_toocheap)[1]]

  #-----
  # 6) Newton/Miller/Smith extension
  #-----

  # big if clause: run this whole section only if there is actually purchase intent data (which is required for the NMS extension)
  if(!all(is.na(pi_cheap)) & !all(is.na(pi_expensive))) {
    # assign each respondent the calibrated probability of purchase
    psmdata$pi_cheap_cal <- NA
    psmdata$pi_expensive_cal <- NA

    for (i in 1:length(pi_scale)) {
      psmdata$pi_cheap_cal[which(psmdata$pi_cheap == pi_scale[i])] <- pi_calibrated[i]
      psmdata$pi_expensive_cal[which(psmdata$pi_expensive == pi_scale[i])] <- pi_calibrated[i]
    }


    # set up respondent-level data for the price steps
    nms_prices <- data_ecdf$price


    # create a matrix: each row is one respondent, each column is one (unique) price
    nms_matrix <- matrix(nrow = nrow(psmdata), ncol = length(nms_prices),
                         dimnames = list(rownames(psmdata), nms_prices))

    # fill matrix with known values:
    # 1) purchase probability of 0 for "too cheap" and "too expensive"
    # 2) weighted purchase probability for "cheap" and "expensive"

    pos_toocheap <- sapply(as.character(psmdata$toocheap), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_toocheap))] <- 0

    pos_tooexpensive <- sapply(as.character(psmdata$tooexpensive), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_tooexpensive))] <- 0

    pos_cheap <- sapply(as.character(psmdata$cheap), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_cheap))] <- psmdata$pi_cheap_cal

    pos_expensive <- sapply(as.character(psmdata$expensive), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_expensive))] <- psmdata$pi_expensive_cal

    table(nms_matrix[1,])

    # gradual interpolation of purchase probabilities

    if(all(is.na(psmdata$toocheap))) {# if no data for "too cheap": interpolation between two pairs of values
      for (i in 1:nrow(nms_matrix)) {
        # linear interpolation between first pair of values (usually: "cheap" and "expensive")
        nms_matrix[i, min(which(!is.na(nms_matrix[i, ]))):max(which(!is.na(nms_matrix[i, ])))] <- c(
          seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[1]],
                  to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
                  length.out = which(!is.na(nms_matrix[i, ]))[2] - which(!is.na(nms_matrix[i, ]))[1] + 1),
          # linear interpolation between second pair of values (usually: "expensive" to "too expensive")
          seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
                  to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[3]],
                  length.out = which(!is.na(nms_matrix[i, ]))[3] - which(!is.na(nms_matrix[i, ]))[2] + 1)[-1])
          # linear interpolation between third pair of values (usually: "expensive" to "too expensive")
      }
    } else {# if data for "too cheap": interpolation between three pairs of values
      for (i in 1:nrow(nms_matrix)) {
        # linear interpolation between first pair of values (usually: "too cheap" and "cheap")
        nms_matrix[i, min(which(!is.na(nms_matrix[i, ]))):max(which(!is.na(nms_matrix[i, ])))] <- c(
          seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[1]],
                  to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
                  length.out = which(!is.na(nms_matrix[i, ]))[2] - which(!is.na(nms_matrix[i, ]))[1] + 1),
          # linear interpolation between second pair of values (usually: "cheap" to "expensive")
          seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
                  to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[3]],
                  length.out = which(!is.na(nms_matrix[i, ]))[3] - which(!is.na(nms_matrix[i, ]))[2] + 1)[-1],
          # linear interpolation between third pair of values (usually: "expensive" to "too expensive")
          seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[3]],
                  to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[4]],
                  length.out = which(!is.na(nms_matrix[i, ]))[4] - which(!is.na(nms_matrix[i, ]))[3] + 1)[-1]
        )
      }
    }

    # purchase probabilities outside of the individual's personal price range must be set to zero
    nms_matrix[is.na(nms_matrix)] <- 0

    # analysis of trial and revenue (mean trial for each price)

    data_nms <- data.frame(price = nms_prices,
                           trial = apply(nms_matrix, 2, mean),
                           row.names = 1:length(nms_prices))

    data_nms$revenue <- data_nms$price * data_nms$trial

    price_optimal_trial <- data_nms$price[which.max(data_nms$trial)]
    price_optimal_revenue <- data_nms$price[which.max(data_nms$revenue)]
  }

  #-----
  # 7) Construct the object to be returned
  #-----

  # NMS <- !all(is.na(pi_cheap)) & !all(is.na(pi_expensive))

  output_psm <- list(data_input = psmdata,
                     validated = validate,
                     invalid_cases = invalid_cases,
                     total_sample = total_sample,
                     data_vanwestendorp = data_ecdf,
                     pricerange_lower = pricerange_lower,
                     pricerange_upper = pricerange_upper,
                     idp = idp,
                     opp = opp,
                     weighted = FALSE,
                     NMS = NMS)

  # if NMS analysis was run: amend additional NMS outputs
  if(NMS == TRUE) {
    output_psm$data_nms <- data_nms
    output_psm$pi_scale <- data.frame(pi_scale, pi_calibrated)
    output_psm$price_optimal_trial <- price_optimal_trial
    output_psm$price_optimal_revenue <- price_optimal_revenue
  }

  class(output_psm) <- "psm"

  return(output_psm)
}
