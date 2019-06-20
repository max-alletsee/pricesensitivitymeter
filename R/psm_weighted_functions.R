#---------------------
# Implementing van Westendorp's PSM in R
# ... with the possibility of having weights
#---------------------

psm_analysis_weighted <- function(toocheap, cheap, expensive, tooexpensive, design,
                                  validate = TRUE, interpolate = FALSE,
                                  intersection_method = "min",
                                  pi_cheap = NA, pi_expensive = NA,
                                  pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0)) {

  #---
  # 1) Input Check: Price Sensitivity Meter data
  #---

  # check if survey package could be loaded
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("The \"survey\" package is needed for the psm_analysis_weighted() function. Please install it. If you want to use unweighted data, please use the function psm_analysis() instead.")
  }

  # input check 1a: validate is required and must be boolean
  if(any(is.na(validate)) | !is.logical(validate) | length(validate) != 1) {
    stop("validate requires one logical value")
  }

  # input check 1b: interpolation is required and must be boolean
  if(any(is.na(interpolate)) | !is.logical(interpolate) | length(interpolate) != 1) {
    stop("interpolate requires one logical value")
  }

  # input check 1c: intersection_method must have length 1 and one of the pre-defined terms
  if(length(intersection_method) != 1) {
    stop("intersection_method must have length 1")
  }

  if(!intersection_method %in% c("min", "max", "mean", "median")) {
    stop("intersection_method must be one of the pre-defined values: min, max, mean, median")
  }

  # input check 2: design must be provided as an object of class "survey.design" (which is the default export of the svydesign function in the survey package)
  if(!inherits(design, "survey.design")) {
    stop("The design argument must be an object of class \"survey.design\". This is exported by the svydesign() function in the survey package. Please specify your survey design with the svydesign() function before running the weighted price sensitivity meter analysis.")
  }

  if(!is.character(toocheap) | !is.character(cheap) | !is.character(expensive) | !is.character(tooexpensive) |
     length(toocheap) != 1 | length(cheap) != 1 | length(expensive) != 1 | length(tooexpensive) != 1) {
    stop("All price arguments (toocheap, cheap, expensive, tooexpensive) must be character values that contain the name of the respective price variable in the data/design object")
    }

  # identify columns in design object that are supposed to contain the price variables
    col_toocheap <- match(toocheap, colnames(design$variables))
    col_cheap <- match(cheap, colnames(design$variables))
    col_expensive <- match(expensive, colnames(design$variables))
    col_tooexpensive <- match(tooexpensive, colnames(design$variables))

  if(is.na(col_toocheap) | is.na(col_cheap) | is.na(col_expensive) | is.na(col_tooexpensive)) {
    stop("Could not find all variable names of the price variables (toocheap, cheap, expensive, tooexpensive) in the design object")
  }

  if(ifelse(!is.numeric(design$variables[, col_toocheap]), !all(is.na(design$variables[, col_toocheap])), FALSE) | !is.numeric(design$variables[, col_cheap]) | !is.numeric(design$variables[, col_expensive]) | !is.numeric(design$variables[, col_tooexpensive])) {
    stop("All price variables (toocheap, cheap, expensive, tooexpensive) must contain only numeric values\n(toocheap is also tolerated if all values are NA)")
  }

    # if all checks are okay, copy into a new object and rename the variables to the standard names
    psm_data_w <- design
    colnames(psm_data_w$variables)[col_toocheap] <- "toocheap"
    colnames(psm_data_w$variables)[col_cheap] <- "cheap"
    colnames(psm_data_w$variables)[col_expensive] <- "expensive"
    colnames(psm_data_w$variables)[col_tooexpensive] <- "tooexpensive"


    #---
    # 2) Input Check: Newton Miller Smith extension
    #---

    NMS <- !all(is.na(pi_cheap)) & !all(is.na(pi_expensive))

    # NMS - check for matching variable names
    if(isTRUE(NMS)) {
      col_pi_cheap <- match(pi_cheap, colnames(design$variables))
      col_pi_expensive <- match(pi_expensive, colnames(design$variables))

      if(is.na(col_pi_cheap) | is.na(col_pi_expensive)) {
        stop("Could not find all variable names of the purchase intent variables (pi_cheap, pi_expensive) in the design object")
      }

      psm_data_w$variables$pi_cheap <- design$variables[, col_pi_cheap]
      psm_data_w$variables$pi_expensive <- design$variables[, col_pi_expensive]
    }

    # NMS - for each value on the purchase intent scale, there must be a corresponding calibration value
    stopifnot(length(pi_scale) == length(pi_calibrated))

    # NMS - purchase intent data must only contain values from the pre-defined scale
    if(isTRUE(NMS)) {
      # check that purchase intent data and scale have the same class (special handling for integer vs. numeric vs. double)
      if(!identical(x = class(psm_data_w$variables$pi_cheap), y = class(pi_scale)) & # for pi_cheap
         !(is.numeric(psm_data_w$variables$pi_cheap) & is.numeric(pi_scale)) & # for pi_cheap
         !identical(x = class(psm_data_w$variables$pi_expensive), y = class(pi_scale)) & # for pi_expensive
         !(is.numeric(psm_data_w$variables$pi_expensive) & is.numeric(pi_scale))) { # for pi_expensive
        stop("pi_cheap, pi_expensive and pi_scale must all be numeric")
      }

      # check that all purchase intent data only includes values from the pre-defined scale
      if(!all(unique(psm_data_w$variables$pi_cheap) %in% unique(pi_scale))) {
        stop("pi_cheap contains values which are not defined in the pi_scale variable")
      }

      if(!all(unique(psm_data_w$variables$pi_expensive) %in% unique(pi_scale))) {
        stop("pi_expensive contains values which are not defined in the pi_scale variable")
      }

      # NMS -  calibration values must be numeric
      if(any(!is.numeric(pi_calibrated))) {
        stop("All calibrated purchase intent values must be numeric")
      }

      # NMS -  calibration values must be between 0 and 1 - only warning if this is not the case...

      if(any(is.nan(pi_calibrated))) {
        stop("Some of the purchase intent calibration values are not a number (NaN)")
      }

      if(any(is.infinite(pi_calibrated))) {
        stop("Some of the purchase intent calibration values are infinite (-Inf, Inf).")
      }


      if(any(pi_calibrated < 0)) {
        warning("Some of the purchase intent calibration values are smaller than 0. It seems that this is not a probability between 0 and 1. The interpretation of the trial/revenue values is not recommended.")
      }

      if(any(pi_calibrated > 1)) {
        warning("Some of the purchase intent calibration values are larger than 1. It seems that this is not a probability between 0 and 1. The interpretation of the trial/revenue values is not recommended.")
      }
    }

    #-----
    # 3) Validation of response patterns answers and optional cleaning of data set
    #-----

    # validation: "too cheap < cheap < expensive < too expensive" for each case. if not, drop from the data
    # consider special case of data without "too cheap" values for all respondents
    if(all(is.na(psm_data_w$variables$toocheap))) { # if "too cheap" is missing: ignore for validation
      psm_data_w$variables$valid <- psm_data_w$variables$tooexpensive > psm_data_w$variables$expensive & psm_data_w$variables$expensive > psm_data_w$variables$cheap
      # set to invalid if any NAs
      psm_data_w$variables$valid[which(is.na(psm_data_w$variables$tooexpensive) | is.na(psm_data_w$variables$expensive) | is.na(psm_data_w$variables$cheap))] <- FALSE
    } else { # if "too cheap" is included: consider in validation
      psm_data_w$variables$valid <- psm_data_w$variables$tooexpensive > psm_data_w$variables$expensive & psm_data_w$variables$expensive > psm_data_w$variables$cheap & psm_data_w$variables$cheap > psm_data_w$variables$toocheap
      # set to invalid if any NAs
      psm_data_w$variables$valid[which(is.na(psm_data_w$variables$tooexpensive) | is.na(psm_data_w$variables$expensive) | is.na(psm_data_w$variables$cheap) | is.na(psm_data_w$variables$toocheap))] <- FALSE
    }


    if(any(psm_data_w$variables$valid == FALSE) & !isTRUE(validate)) {
      warning("Some respondents' price structures might not be consistent (i.e. different from too cheap < cheap < expensive < too expensive). Consider running this function with the additional option 'validate == TRUE' to analyse only the subset of respondents with consistent price structure.")
    }

    # save values for return function later
    invalid_cases <- nrow(psm_data_w$variables) - sum(psm_data_w$variables$valid)
    total_sample <- nrow(psm_data_w$variables)

    if(total_sample == invalid_cases) {
      stop("All respondents have intransitive preference structures (i.e. different from too cheap < cheap < expensive < too expensive).")
    }

  #-----
  # 4) Creating the empirical cumulative density per price
  #-----

  # new data set: 1st variable shows all prices, other variables show the respective cumulative density
    data_ecdf <- data.frame(price = sort(unique(c(round(psm_data_w$variables$toocheap, digits = 2),
                                                  round(psm_data_w$variables$cheap, digits = 2),
                                                  round(psm_data_w$variables$expensive, digits = 2),
                                                  round(psm_data_w$variables$tooexpensive, digits = 2)))))

  # empirical cumulative density for "too cheap" (ignore if no "too cheap" values provided)
  if(!all(is.na(psm_data_w$variables$toocheap))) { # if there are values: first as a function
    ecdf_psm <- survey::svycdf(formula = ~ toocheap,
                               design = psm_data_w)

    # ... apply the function to all prices (1 - f(x) because the function is reversed in the original paper)
    data_ecdf$ecdf_toocheap <- 1 - ecdf_psm$toocheap(data_ecdf$price)

  } else { # if no "too cheap" values provided: set to NA
    data_ecdf$ecdf_toocheap <- NA
  }

  # same for "cheap", "expensive", and "too expensive"
  # "cheap" is also reversed in the original paper, "expensive" and "too expensive" are not
  ecdf_psm <- survey::svycdf(formula = ~ cheap + expensive + tooexpensive,
                             design = psm_data_w)

  data_ecdf$ecdf_cheap <- 1 - ecdf_psm$cheap(data_ecdf$price)
  data_ecdf$ecdf_expensive <- ecdf_psm$expensive(data_ecdf$price)
  data_ecdf$ecdf_tooexpensive <- ecdf_psm$tooexpensive(data_ecdf$price)

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
    data_ecdf_smooth$ecdf_toocheap <- try(approx(x = data_ecdf$price,
                                             y = data_ecdf$ecdf_toocheap,
                                             xout = data_ecdf_smooth$price,
                                             method = "linear")$y)

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
  pricerange_lower <- identify_intersection(data = data_ecdf,
                                            var1 = "ecdf_not_cheap",
                                            var2 = "ecdf_toocheap",
                                            method = intersection_method)

  # price range, upper bound: intersection of "not expensive" and "too expensive"
  pricerange_upper <- identify_intersection(data = data_ecdf,
                                            var1 = "ecdf_tooexpensive",
                                            var2 = "ecdf_not_expensive",
                                            method = intersection_method)

  # indifference price point IDP: intersection of "expensive" and "cheap"
  # interpretation: a) median price paid by consumer or b) price of the product of an important market leader
  idp <- identify_intersection(data = data_ecdf,
                               var1 = "ecdf_expensive",
                               var2 = "ecdf_cheap",
                               method = intersection_method)

  # optimal price point OPP: intersection of "too expensive" and "too cheap"
  # interpretation: resistance against the price of a product is very low
  opp <- identify_intersection(data = data_ecdf,
                               var1 = "ecdf_tooexpensive",
                               var2 = "ecdf_toocheap",
                               method = intersection_method)

  #-----
  # 6) Newton Miller Smith Extension with weighted data
  #-----


  if(isTRUE(NMS)) {
    # assign each respondent the calibrated probability of purchase
    psm_data_w$variables$pi_cheap_cal <- NA
    psm_data_w$variables$pi_expensive_cal <- NA

    for (i in 1:length(pi_scale)) {
      psm_data_w$variables$pi_cheap_cal[which(psm_data_w$variables$pi_cheap == pi_scale[i])] <- pi_calibrated[i]
      psm_data_w$variables$pi_expensive_cal[which(psm_data_w$variables$pi_expensive == pi_scale[i])] <- pi_calibrated[i]
    }


    # set up respondent-level data for the price steps
    nms_prices <- data_ecdf$price


    # create a matrix: each row is one respondent, each column is one (unique) price
    nms_matrix <- matrix(nrow = nrow(psm_data_w$variables), ncol = length(nms_prices),
                         dimnames = list(rownames(psm_data_w$variables), nms_prices))

    # fill matrix with known values:
    # 1) purchase probability of 0 for "too cheap" and "too expensive"
    # 2) weighted purchase probability for "cheap" and "expensive"

    pos_toocheap <- sapply(as.character(round(psm_data_w$variables$toocheap, digits = 2)), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_toocheap))] <- 0

    pos_tooexpensive <- sapply(as.character(round(psm_data_w$variables$tooexpensive, digits = 2)), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_tooexpensive))] <- 0

    pos_cheap <- sapply(as.character(round(psm_data_w$variables$cheap, digits = 2)), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_cheap))] <- psm_data_w$variables$pi_cheap_cal

    pos_expensive <- sapply(as.character(round(psm_data_w$variables$expensive, digits = 2)), FUN = function(x) which(colnames(nms_matrix) == x))
    nms_matrix[cbind(1:nrow(nms_matrix), as.numeric(pos_expensive))] <- psm_data_w$variables$pi_expensive_cal


    # gradual interpolation of purchase probabilities
    for (i in 1:nrow(nms_matrix)) {
      interpolate_prob <- NA

      # try linear interpolation between three pairs of values
      interpolate_prob <- try(c(
        # linear interpolation between first pair of values (usually: "too cheap" and "cheap")
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
                length.out = which(!is.na(nms_matrix[i, ]))[4] - which(!is.na(nms_matrix[i, ]))[3] + 1)[-1]),
        silent = TRUE)

      # if try() function throws a silent error, perform interpolation between two pairs of values instead
      if(inherits(interpolate_prob, "try-error")) {
        # linear interpolation between first pair of values (usually: "too cheap"/"cheap" OR "cheap"/"expensive")
        interpolate_prob <- c(
          seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[1]],
                  to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
                  length.out = which(!is.na(nms_matrix[i, ]))[2] - which(!is.na(nms_matrix[i, ]))[1] + 1),
          # linear interpolation between second pair of values (usually: "cheap"/"expensive" OR "expensive"/"too expensive")
          seq.int(from = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[2]],
                  to = nms_matrix[i, which(!is.na(nms_matrix[i, ]))[3]],
                  length.out = which(!is.na(nms_matrix[i, ]))[3] - which(!is.na(nms_matrix[i, ]))[2] + 1)[-1])
      }

      # write vector with interpolated values to matrix
      nms_matrix[i, min(which(!is.na(nms_matrix[i, ]))):max(which(!is.na(nms_matrix[i, ])))] <- interpolate_prob
    }

    # purchase probabilities outside of the individual's personal price range must be set to zero
    nms_matrix[is.na(nms_matrix)] <- 0

    # extract weights from survey design
    nms_weights <- weights(psm_data_w)

    # analysis of trial and revenue (mean trial for each price)
    # ... via weighted.mean() from base R
    data_nms <- data.frame(price = nms_prices,
                           trial = apply(nms_matrix, 2, stats::weighted.mean, w = nms_weights, na.rm = TRUE),
                           row.names = 1:length(nms_prices))

    data_nms$revenue <- data_nms$price * data_nms$trial

    price_optimal_trial <- data_nms$price[which.max(data_nms$trial)]
    price_optimal_revenue <- data_nms$price[which.max(data_nms$revenue)]
  }

  #-----
  # 7) Construct the object to be returned
  #-----

  output_psm <- list(data_input = psm_data_w$variables,
                     validated = validate,
                     invalid_cases = invalid_cases,
                     total_sample = total_sample,
                     data_vanwestendorp = data_ecdf,
                     pricerange_lower = pricerange_lower,
                     pricerange_upper = pricerange_upper,
                     idp = idp,
                     opp = opp,
                     weighted = TRUE,
                     survey_design = psm_data_w,
                     NMS = NMS)

  # if NMS analysis was run: amend additional NMS outputs
  if(isTRUE(NMS)) {
    output_psm$data_nms <- data_nms
    output_psm$pi_scale <- data.frame(pi_scale, pi_calibrated)
    output_psm$price_optimal_trial <- price_optimal_trial
    output_psm$price_optimal_revenue <- price_optimal_revenue
  }

  class(output_psm) <- "psm"

  return(output_psm)
}
