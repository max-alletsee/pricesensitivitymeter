#---------------------
# Implementing van Westendorp's PSM in R
# ... with the possibility of having weights
#---------------------

psm_analysis_weighted <- function(toocheap, cheap, expensive, tooexpensive, design,
                                  validate = TRUE, interpolate = FALSE) {

  #---
  # 1) Input Check: Price Sensitivity Meter data
  #---

  # check if survey package could be loaded
  if (!requireNamespace("survey", quietly = TRUE)) {
    stop("The \"survey\" package is needed for the psm_analysis_weighted() function to work. Please install it. If you want to use unweighted data, please use the function psm_analysis() instead.")
  }

  # input check 1a: validate is required and must be boolean
  if(any(is.na(validate)) | !is.logical(validate) | length(validate) != 1) {
    stop("validate requires one logical value")
  }

  # input check 1b: interpolation is required and must be boolean
  if(any(is.na(interpolate)) | !is.logical(interpolate) | length(interpolate) != 1) {
    stop("interpolate requires one logical value")
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

    #-----
    # 2) Validation of response patterns answers and optional cleaning of data set
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
  data_ecdf <- data.frame(price = sort(unique(c(psm_data_w$variables$toocheap,
                                                psm_data_w$variables$cheap,
                                                psm_data_w$variables$expensive,
                                                psm_data_w$variables$tooexpensive))))

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
  # 6) Construct the object to be returned
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
                     NMS = FALSE)

  class(output_psm) <- "psm"

  return(output_psm)
}
