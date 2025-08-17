context("PSM Data Processing Functions")

#----
# Helper function to create test data
#----

create_test_data <- function(n = 10, seed = 123) {
  set.seed(seed)
  data.frame(
    toocheap = rnorm(n, 5, 1),
    cheap = rnorm(n, 8, 1),
    expensive = rnorm(n, 12, 1),
    tooexpensive = rnorm(n, 15, 1),
    pi_cheap = sample(1:5, n, replace = TRUE),
    pi_expensive = sample(1:5, n, replace = TRUE),
    weights = runif(n, 0.5, 2)
  )
}

#----
# Tests for prepare_psm_data
#----

test_that("prepare_psm_data handles vector inputs correctly", {
  result <- prepare_psm_data(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  expect_true(is.list(result))
  expect_false(result$weighted)
  expect_false(result$nms_requested)
  expect_equal(nrow(result$data), 2)
  expect_true(all(c("toocheap", "cheap", "expensive", "tooexpensive") %in% names(result$data)))
  expect_null(result$survey_design)
})

test_that("prepare_psm_data handles data frame inputs correctly", {
  test_data <- data.frame(
    tc = c(1, 2, 3), ch = c(2, 3, 4), ex = c(3, 4, 5), te = c(4, 5, 6)
  )
  
  result <- prepare_psm_data(
    toocheap = "tc", cheap = "ch", expensive = "ex", tooexpensive = "te",
    data = test_data
  )
  
  expect_false(result$weighted)
  expect_false(result$nms_requested)
  expect_equal(nrow(result$data), 3)
  expect_equal(result$data$toocheap, c(1, 2, 3))
  expect_equal(result$data$cheap, c(2, 3, 4))
})

test_that("prepare_psm_data handles NMS data correctly", {
  result <- prepare_psm_data(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5),
    pi_cheap = c(3, 4), pi_expensive = c(2, 3)
  )
  
  expect_true(result$nms_requested)
  expect_true("pi_cheap" %in% names(result$data))
  expect_true("pi_expensive" %in% names(result$data))
  expect_true("pi_cheap_cal" %in% names(result$data))
  expect_true("pi_expensive_cal" %in% names(result$data))
})

test_that("prepare_psm_data handles survey design correctly", {
  skip_if_not_installed("survey")
  
  test_data <- data.frame(
    tc = c(1, 2, 3), ch = c(2, 3, 4), ex = c(3, 4, 5), te = c(4, 5, 6),
    weights = c(1, 1, 1)
  )
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = test_data)
  
  result <- prepare_psm_data(
    toocheap = "tc", cheap = "ch", expensive = "ex", tooexpensive = "te",
    design = design
  )
  
  expect_true(result$weighted)
  expect_false(result$nms_requested)
  expect_equal(nrow(result$data), 3)
  expect_true(inherits(result$survey_design, "survey.design"))
})

test_that("prepare_psm_data validates price preferences correctly", {
  # Valid preferences
  result_valid <- prepare_psm_data(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5),
    validate = TRUE
  )
  expect_equal(result_valid$invalid_cases, 0)
  expect_equal(result_valid$total_sample, 2)
  
  # Invalid preferences
  result_invalid <- prepare_psm_data(
    toocheap = c(3, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5),
    validate = FALSE
  )
  expect_gt(result_invalid$invalid_cases, 0)
})

#----
# Tests for extract_data_from_design
#----

test_that("extract_data_from_design works correctly", {
  skip_if_not_installed("survey")
  
  test_data <- data.frame(
    tc = c(1, 2, 3), ch = c(2, 3, 4), ex = c(3, 4, 5), te = c(4, 5, 6),
    weights = c(1, 1, 1)
  )
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = test_data)
  
  result <- extract_data_from_design(design, "tc", "ch", "ex", "te")
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(c("toocheap", "cheap", "expensive", "tooexpensive") %in% names(result)))
  expect_equal(result$toocheap, c(1, 2, 3))
  expect_equal(result$cheap, c(2, 3, 4))
})

#----
# Tests for extract_data_from_dataframe
#----

test_that("extract_data_from_dataframe works correctly", {
  test_data <- data.frame(
    tc = c(1, 2, 3), ch = c(2, 3, 4), ex = c(3, 4, 5), te = c(4, 5, 6)
  )
  
  result <- extract_data_from_dataframe(test_data, "tc", "ch", "ex", "te")
  
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 3)
  expect_true(all(c("toocheap", "cheap", "expensive", "tooexpensive") %in% names(result)))
  expect_equal(result$toocheap, c(1, 2, 3))
  expect_equal(result$cheap, c(2, 3, 4))
})

#----
# Tests for add_nms_data
#----

test_that("add_nms_data works with vectors", {
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- add_nms_data(
    psmdata, pi_cheap = c(3, 4), pi_expensive = c(2, 3),
    pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0)
  )
  
  expect_true("pi_cheap" %in% names(result))
  expect_true("pi_expensive" %in% names(result))
  expect_true("pi_cheap_cal" %in% names(result))
  expect_true("pi_expensive_cal" %in% names(result))
  expect_equal(result$pi_cheap, c(3, 4))
  expect_equal(result$pi_expensive, c(2, 3))
})

test_that("add_nms_data works with data frame", {
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  data <- data.frame(
    pi_ch = c(3, 4), pi_ex = c(2, 3)
  )
  
  result <- add_nms_data(
    psmdata, pi_cheap = "pi_ch", pi_expensive = "pi_ex",
    pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
    data = data
  )
  
  expect_equal(result$pi_cheap, c(3, 4))
  expect_equal(result$pi_expensive, c(2, 3))
})

test_that("add_nms_data works with survey design", {
  skip_if_not_installed("survey")
  
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  test_data <- data.frame(
    tc = c(1, 2), ch = c(2, 3), ex = c(3, 4), te = c(4, 5),
    pi_ch = c(3, 4), pi_ex = c(2, 3), weights = c(1, 1)
  )
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = test_data)
  
  result <- add_nms_data(
    psmdata, pi_cheap = "pi_ch", pi_expensive = "pi_ex",
    pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
    design = design
  )
  
  expect_equal(result$pi_cheap, c(3, 4))
  expect_equal(result$pi_expensive, c(2, 3))
})

#----
# Tests for calibrate_purchase_intent
#----

test_that("calibrate_purchase_intent works correctly", {
  pi_data <- c(5, 4, 3, 2, 1, 5, 3, 1)
  pi_scale <- 5:1
  pi_calibrated <- c(0.7, 0.5, 0.3, 0.1, 0)
  
  result <- calibrate_purchase_intent(pi_data, pi_scale, pi_calibrated)
  
  expect_equal(length(result), length(pi_data))
  expect_equal(result, c(0.7, 0.5, 0.3, 0.1, 0, 0.7, 0.3, 0))
  expect_true(all(result >= 0 & result <= 1))
})

test_that("calibrate_purchase_intent handles missing values", {
  pi_data <- c(5, 4, 6, 2, 1)  # 6 is not in scale
  pi_scale <- 5:1
  pi_calibrated <- c(0.7, 0.5, 0.3, 0.1, 0)
  
  result <- calibrate_purchase_intent(pi_data, pi_scale, pi_calibrated)
  
  expect_equal(length(result), length(pi_data))
  expect_true(is.na(result[3]))  # Value 6 not in scale should be NA
  expect_equal(result[c(1,2,4,5)], c(0.7, 0.5, 0.1, 0))
})

#----
# Tests for validate_price_preferences
#----

test_that("validate_price_preferences works with valid data", {
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- validate_price_preferences(psmdata, validate = TRUE)
  
  expect_equal(result$invalid_cases, 0)
  expect_equal(result$total_sample, 2)
  expect_equal(nrow(result$data), 2)
  expect_true(all(result$data$valid))
})

test_that("validate_price_preferences detects invalid preferences", {
  psmdata <- data.frame(
    toocheap = c(3, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- validate_price_preferences(psmdata, validate = FALSE)
  
  expect_gt(result$invalid_cases, 0)
  expect_equal(result$total_sample, 2)
  expect_false(result$data$valid[1])  # First case should be invalid
})

test_that("validate_price_preferences filters invalid cases when validate=TRUE", {
  psmdata <- data.frame(
    toocheap = c(3, 2), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- validate_price_preferences(psmdata, validate = TRUE)
  
  expect_gt(result$invalid_cases, 0)
  expect_lt(nrow(result$data), result$total_sample)
})

test_that("validate_price_preferences handles missing toocheap", {
  psmdata <- data.frame(
    toocheap = c(NA, NA), cheap = c(2, 3), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- validate_price_preferences(psmdata, validate = TRUE)
  
  expect_equal(result$invalid_cases, 0)
  expect_equal(nrow(result$data), 2)
})

test_that("validate_price_preferences handles NA values correctly", {
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, NA), 
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- validate_price_preferences(psmdata, validate = FALSE)
  
  expect_equal(result$invalid_cases, 1)  # Second case has NA
  expect_false(result$data$valid[2])
})

#----
# Tests for calculate_ecdf_data
#----

test_that("calculate_ecdf_data produces correct structure", {
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- calculate_ecdf_data(psmdata)
  
  expect_true(is.data.frame(result))
  expect_true("price" %in% names(result))
  expect_true(all(c("ecdf_cheap", "ecdf_expensive", "ecdf_tooexpensive") %in% names(result)))
  expect_true(all(result$ecdf_cheap >= 0 & result$ecdf_cheap <= 1, na.rm = TRUE))
  expect_true(all(result$ecdf_expensive >= 0 & result$ecdf_expensive <= 1, na.rm = TRUE))
  expect_true("ecdf_not_cheap" %in% names(result))
  expect_true("ecdf_not_expensive" %in% names(result))
})

test_that("calculate_ecdf_data handles interpolation", {
  psmdata <- data.frame(
    toocheap = c(1, 3), cheap = c(2, 4),
    expensive = c(3, 5), tooexpensive = c(4, 6)
  )
  
  result_no_interp <- calculate_ecdf_data(psmdata, interpolate = FALSE)
  result_interp <- calculate_ecdf_data(psmdata, interpolate = TRUE, interpolation_steps = 0.5)
  
  expect_gt(nrow(result_interp), nrow(result_no_interp))
  expect_true(all(diff(result_interp$price) <= 0.5))
})

test_that("calculate_ecdf_data handles weighted analysis", {
  skip_if_not_installed("survey")
  
  test_data <- data.frame(
    tc = c(1, 2, 3), ch = c(2, 3, 4), ex = c(3, 4, 5), te = c(4, 5, 6),
    weights = c(1, 2, 1)
  )
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = test_data)
  
  # Prepare data for weighted analysis
  psmdata <- data.frame(
    toocheap = c(1, 2, 3), cheap = c(2, 3, 4),
    expensive = c(3, 4, 5), tooexpensive = c(4, 5, 6)
  )
  
  result <- calculate_ecdf_data(psmdata, weighted = TRUE, survey_design = design)
  
  expect_true(is.data.frame(result))
  expect_true("price" %in% names(result))
  expect_true(all(c("ecdf_cheap", "ecdf_expensive", "ecdf_tooexpensive") %in% names(result)))
})

test_that("calculate_ecdf_data handles missing toocheap", {
  psmdata <- data.frame(
    toocheap = c(NA, NA), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- calculate_ecdf_data(psmdata)
  
  expect_true(all(is.na(result$ecdf_toocheap)))
  expect_false(any(is.na(result$ecdf_cheap)))
})

#----
# Tests for calculate_unweighted_ecdfs
#----

test_that("calculate_unweighted_ecdfs works correctly", {
  data_ecdf <- data.frame(price = 1:5)
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5)
  )
  
  result <- calculate_unweighted_ecdfs(data_ecdf, psmdata)
  
  expect_true(all(c("ecdf_toocheap", "ecdf_cheap", "ecdf_expensive", "ecdf_tooexpensive") %in% names(result)))
  expect_true(all(result$ecdf_cheap >= 0 & result$ecdf_cheap <= 1))
  expect_true(all(result$ecdf_expensive >= 0 & result$ecdf_expensive <= 1))
})

#----
# Tests for apply_interpolation
#----

test_that("apply_interpolation works correctly", {
  data_ecdf <- data.frame(
    price = c(1, 3, 5),
    ecdf_toocheap = c(1, 0.5, 0),
    ecdf_cheap = c(1, 0.5, 0),
    ecdf_expensive = c(0, 0.5, 1),
    ecdf_tooexpensive = c(0, 0.5, 1)
  )
  
  result <- apply_interpolation(data_ecdf, interpolation_steps = 1)
  
  expect_gt(nrow(result), nrow(data_ecdf))
  expect_equal(min(result$price), 1)
  expect_equal(max(result$price), 5)
  expect_true(all(diff(result$price) <= 1))
})

#----
# Tests for identify_price_points
#----

test_that("identify_price_points works correctly", {
  # Create simple ECDF data for testing
  data_ecdf <- data.frame(
    price = 1:5,
    ecdf_toocheap = c(1, 0.8, 0.6, 0.4, 0.2),
    ecdf_cheap = c(1, 0.8, 0.6, 0.4, 0.2),
    ecdf_expensive = c(0.2, 0.4, 0.6, 0.8, 1),
    ecdf_tooexpensive = c(0.2, 0.4, 0.6, 0.8, 1),
    ecdf_not_cheap = c(0, 0.2, 0.4, 0.6, 0.8),
    ecdf_not_expensive = c(0.8, 0.6, 0.4, 0.2, 0)
  )
  
  result <- identify_price_points(data_ecdf)
  
  expect_true(is.list(result))
  expect_true(all(c("pricerange_lower", "pricerange_upper", "idp", "opp") %in% names(result)))
  expect_true(is.numeric(result$idp))
  expect_true(is.numeric(result$opp))
})

test_that("identify_price_points handles different acceptable_range definitions", {
  data_ecdf <- data.frame(
    price = 1:5,
    ecdf_toocheap = c(1, 0.8, 0.6, 0.4, 0.2),
    ecdf_cheap = c(1, 0.8, 0.6, 0.4, 0.2),
    ecdf_expensive = c(0.2, 0.4, 0.6, 0.8, 1),
    ecdf_tooexpensive = c(0.2, 0.4, 0.6, 0.8, 1),
    ecdf_not_cheap = c(0, 0.2, 0.4, 0.6, 0.8),
    ecdf_not_expensive = c(0.8, 0.6, 0.4, 0.2, 0)
  )
  
  result_original <- identify_price_points(data_ecdf, acceptable_range = "original")
  result_narrower <- identify_price_points(data_ecdf, acceptable_range = "narrower")
  
  expect_true(is.numeric(result_original$pricerange_lower))
  expect_true(is.numeric(result_narrower$pricerange_lower))
  # Results may differ between methods
})

#----
# Tests for calculate_nms_analysis
#----

test_that("calculate_nms_analysis works correctly", {
  # Create prepared data structure
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5),
    pi_cheap_cal = c(0.7, 0.5), pi_expensive_cal = c(0.3, 0.1)
  )
  
  prepared_data <- list(
    data = psmdata,
    weighted = FALSE,
    survey_design = NULL,
    nms_requested = TRUE
  )
  
  data_ecdf <- data.frame(price = 1:5)
  
  result <- calculate_nms_analysis(prepared_data, data_ecdf)
  
  expect_true(is.list(result))
  expect_true(all(c("data_nms", "price_optimal_reach", "price_optimal_revenue") %in% names(result)))
  expect_true(is.data.frame(result$data_nms))
  expect_true(all(c("price", "reach", "revenue") %in% names(result$data_nms)))
  expect_true(is.numeric(result$price_optimal_reach))
  expect_true(is.numeric(result$price_optimal_revenue))
})

test_that("calculate_nms_analysis handles weighted data", {
  skip_if_not_installed("survey")
  
  test_data <- data.frame(
    tc = c(1, 2), ch = c(2, 3), ex = c(3, 4), te = c(4, 5),
    pi_ch = c(3, 4), pi_ex = c(2, 3), weights = c(1, 2)
  )
  design <- survey::svydesign(ids = ~1, weights = ~weights, data = test_data)
  
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5),
    pi_cheap_cal = c(0.7, 0.5), pi_expensive_cal = c(0.3, 0.1)
  )
  
  prepared_data <- list(
    data = psmdata,
    weighted = TRUE,
    survey_design = design,
    nms_requested = TRUE
  )
  
  data_ecdf <- data.frame(price = 1:5)
  
  result <- calculate_nms_analysis(prepared_data, data_ecdf)
  
  expect_true(is.list(result))
  expect_true(is.data.frame(result$data_nms))
})

#----
# Tests for create_nms_matrix
#----

test_that("create_nms_matrix works correctly", {
  psmdata <- data.frame(
    toocheap = c(1, 2), cheap = c(2, 3),
    expensive = c(3, 4), tooexpensive = c(4, 5),
    pi_cheap_cal = c(0.7, 0.5), pi_expensive_cal = c(0.3, 0.1)
  )
  
  nms_prices <- 1:5
  
  result <- create_nms_matrix(psmdata, nms_prices, 
                             pi_calibrated_toocheap = 0, 
                             pi_calibrated_tooexpensive = 0)
  
  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(psmdata))
  expect_equal(ncol(result), length(nms_prices))
  expect_equal(colnames(result), as.character(nms_prices))
})

test_that("create_nms_matrix handles weighted data", {
  psmdata <- data.frame(
    toocheap = c(1.11, 2.22), cheap = c(2.33, 3.44),
    expensive = c(3.55, 4.66), tooexpensive = c(4.77, 5.88),
    pi_cheap_cal = c(0.7, 0.5), pi_expensive_cal = c(0.3, 0.1)
  )
  
  nms_prices <- seq(1, 6, by = 0.5)
  
  result <- create_nms_matrix(psmdata, nms_prices, 
                             pi_calibrated_toocheap = 0, 
                             pi_calibrated_tooexpensive = 0,
                             weighted = TRUE)
  
  expect_true(is.matrix(result))
  expect_equal(nrow(result), nrow(psmdata))
  expect_equal(ncol(result), length(nms_prices))
})

#----
# Tests for edge cases and error conditions
#----

test_that("functions handle empty data gracefully", {
  # Empty data should cause appropriate errors
  expect_error(
    prepare_psm_data(
      toocheap = numeric(0), cheap = numeric(0), 
      expensive = numeric(0), tooexpensive = numeric(0)
    )
  )
})

test_that("functions handle single observation", {
  result <- prepare_psm_data(
    toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4
  )
  
  expect_equal(nrow(result$data), 1)
  expect_equal(result$invalid_cases, 0)
})

test_that("functions handle all invalid preferences", {
  expect_error(
    prepare_psm_data(
      toocheap = c(4, 4), cheap = c(3, 3), 
      expensive = c(2, 2), tooexpensive = c(1, 1),
      validate = TRUE
    ),
    "All respondents have intransitive preference structures"
  )
})

test_that("functions issue warnings for invalid preferences when validate=FALSE", {
  expect_warning(
    prepare_psm_data(
      toocheap = c(4, 2), cheap = c(3, 3), 
      expensive = c(2, 4), tooexpensive = c(1, 5),
      validate = FALSE
    ),
    "Some respondents have inconsistent price structures"
  )
})
