context("Data Output Checks")

#----
# Setting up test dataframes: with and without NMS
#----

data.psm.test <- data.frame(
  tch = round(rnorm(n = 20, mean = 5, sd = 0.5), digits = 2),
  ch = round(rnorm(n = 20, mean = 8.5, sd = 0.5), digits = 2),
  ex = round(rnorm(n = 20, mean = 13, sd = 0.75), digits = 2),
  tex = round(rnorm(n = 20, mean = 17, sd = 1), digits = 2),
  pi_cheap = sample(x = c(1:5), size = 20, replace = TRUE, prob = c(0.0, 0.1, 0.2, 0.3, 0.5)),
  pi_expensive = sample(x = c(1:5), size = 20, replace = TRUE, prob = c(0.0, 0.1, 0.2, 0.3, 0.5))
  )

psm.result1 <- psm_analysis(toocheap = "tch",
                            cheap = "ch",
                            expensive = "ex",
                            tooexpensive = "tex",
                            data = data.psm.test)

psm.result2 <- psm_analysis(toocheap = "tch",
                            cheap = "ch",
                            expensive = "ex",
                            tooexpensive = "tex",
                            pi_cheap = "pi_cheap",
                            pi_expensive = "pi_expensive",
                            data = data.psm.test)

#----
# Overall Features of the Output Object
#----

test_that("Data Output: Length of Output Object", {
  expect_length(psm.result1, 11)
  expect_length(psm.result2, 15)
})

test_that("Data Output: Class of Output Object", {
  expect_equal(class(psm.result1), "psm")
  expect_equal(class(psm.result2), "psm")
})

#----
# Internal Structure of the Output Object
#----

test_that("Data Output: Matrices have rows and columns", {
  expect_gt(nrow(psm.result1$data_input), 0)
  expect_gt(nrow(psm.result1$data_vanwestendorp), 0)
  expect_gt(nrow(psm.result2$data_input), 0)
  expect_gt(nrow(psm.result2$data_vanwestendorp), 0)
  expect_gt(nrow(psm.result2$data_nms), 0)

  expect_equal(ncol(psm.result1$data_input), 4)
  expect_equal(ncol(psm.result1$data_vanwestendorp), 7)

  expect_equal(ncol(psm.result2$data_input), 8)
  expect_equal(ncol(psm.result2$data_vanwestendorp), 7)
  expect_equal(ncol(psm.result2$data_nms), 3)
})

test_that("Data Output: Numeric Data in Matrices", {
  expect_true(unique(sapply(psm.result1$data_input, is.numeric)))
  expect_true(unique(sapply(psm.result1$data_vanwestendorp, is.numeric)))

  expect_true(unique(sapply(psm.result2$data_input, is.numeric)))
  expect_true(unique(sapply(psm.result2$data_vanwestendorp, is.numeric)))
  expect_true(unique(sapply(psm.result2$data_nms, is.numeric)))
})


test_that("Data Output: Rest of Output Object Structure", {
  # Standard PSM
  expect_true(is.logical(psm.result1$validated))
  expect_length(psm.result1$validated, 1)

  expect_true(is.numeric(psm.result1$invalid_cases))
  expect_false(is.nan(psm.result1$invalid_cases))
  expect_length(psm.result1$invalid_cases, 1)

  expect_true(is.numeric(psm.result1$total_sample))
  expect_false(is.nan(psm.result1$total_sample))
  expect_length(psm.result1$total_sample, 1)

  expect_true(is.numeric(psm.result1$pricerange_lower))
  expect_false(is.nan(psm.result1$pricerange_lower))
  expect_length(psm.result1$pricerange_lower, 1)

  expect_true(is.numeric(psm.result1$pricerange_upper))
  expect_false(is.nan(psm.result1$pricerange_upper))
  expect_length(psm.result1$pricerange_upper, 1)

  expect_true(is.numeric(psm.result1$idp))
  expect_false(is.nan(psm.result1$idp))
  expect_length(psm.result1$idp, 1)

  expect_true(is.numeric(psm.result1$opp))
  expect_false(is.nan(psm.result1$opp))
  expect_length(psm.result1$opp, 1)

  expect_true(is.logical(psm.result1$NMS))
  expect_false(is.nan(psm.result1$NMS))
  expect_length(psm.result1$NMS, 1)

  # PSM with NMS
  expect_true(is.logical(psm.result2$validated))
  expect_length(psm.result2$validated, 1)

  expect_true(is.numeric(psm.result2$invalid_cases))
  expect_false(is.nan(psm.result2$invalid_cases))
  expect_length(psm.result2$invalid_cases, 1)

  expect_true(is.numeric(psm.result2$total_sample))
  expect_false(is.nan(psm.result2$total_sample))
  expect_length(psm.result2$total_sample, 1)

  expect_true(is.numeric(psm.result2$pricerange_lower))
  expect_false(is.nan(psm.result2$pricerange_lower))
  expect_length(psm.result2$pricerange_lower, 1)

  expect_true(is.numeric(psm.result2$pricerange_upper))
  expect_false(is.nan(psm.result2$pricerange_upper))
  expect_length(psm.result2$pricerange_upper, 1)

  expect_true(is.numeric(psm.result2$idp))
  expect_false(is.nan(psm.result2$idp))
  expect_length(psm.result2$idp, 1)

  expect_true(is.numeric(psm.result2$opp))
  expect_false(is.nan(psm.result2$opp))
  expect_length(psm.result2$opp, 1)

  expect_true(is.logical(psm.result2$NMS))
  expect_false(is.nan(psm.result2$NMS))
  expect_length(psm.result2$NMS, 1)

  expect_true(is.numeric(psm.result2$pi_scale$pi_calibrated))
  expect_false(unique(is.nan(psm.result2$pi_scale$pi_calibrated)))

  expect_true(is.numeric(psm.result2$price_optimal_trial))
  expect_false(is.nan(psm.result2$price_optimal_trial))
  expect_length(psm.result2$price_optimal_trial, 1)

  expect_true(is.numeric(psm.result2$price_optimal_revenue))
  expect_false(is.nan(psm.result2$price_optimal_revenue))
  expect_length(psm.result2$price_optimal_revenue, 1)
})

#----
# Expecting Specific Values
#----

test_that("Data Output: NMS correctly (not) included in output", {
  expect_false(psm.result1$NMS)
  expect_true(psm.result2$NMS)
})

test_that("Data Output: All prices included in the empirical cumulative density function data", {
  expect_true(unique(psm.result1$data_input$toocheap %in% psm.result1$data_vanwestendorp$price))
  expect_true(unique(psm.result1$data_input$cheap %in% psm.result1$data_vanwestendorp$price))
  expect_true(unique(psm.result1$data_input$expensive %in% psm.result1$data_vanwestendorp$price))
  expect_true(unique(psm.result1$data_input$tooexpensive %in% psm.result1$data_vanwestendorp$price))
})

test_that("Data Output: All prices included in the NMS data", {
  expect_equal(min(psm.result2$data_vanwestendorp$price), min(psm.result2$data_nms$price))
  expect_equal(max(psm.result2$data_vanwestendorp$price), max(psm.result2$data_nms$price))
})

#----
# Plausible Values
#----

test_that("Data Output - Plausibility: Number of total cases must be greater than the number of invalid cases", {
  expect_gt(psm.result1$total_sample, psm.result1$invalid_cases)
  expect_gt(psm.result2$total_sample, psm.result2$invalid_cases)
})

test_that("Data Output - Plausibility: Price estimations must be within range of provided prices", {
  expect_gte(psm.result1$pricerange_lower, min(psm.result1$data_vanwestendorp$price))
  expect_lte(psm.result1$pricerange_lower, max(psm.result1$data_vanwestendorp$price))

  expect_gte(psm.result1$pricerange_upper, min(psm.result1$data_vanwestendorp$price))
  expect_lte(psm.result1$pricerange_upper, max(psm.result1$data_vanwestendorp$price))

  expect_gte(psm.result1$idp, min(psm.result1$data_vanwestendorp$price))
  expect_lte(psm.result1$idp, max(psm.result1$data_vanwestendorp$price))

  expect_gte(psm.result1$opp, min(psm.result1$data_vanwestendorp$price))
  expect_lte(psm.result1$opp, max(psm.result1$data_vanwestendorp$price))

  expect_gte(psm.result2$price_optimal_trial, min(psm.result2$data_vanwestendorp$price))
  expect_lte(psm.result2$price_optimal_trial, max(psm.result2$data_vanwestendorp$price))

  expect_gte(psm.result2$price_optimal_revenue, min(psm.result2$data_vanwestendorp$price))
  expect_lte(psm.result2$price_optimal_revenue, max(psm.result2$data_vanwestendorp$price))
})

test_that("Data Output - Plausibility: Hierarchy between lower and upper limit of price range", {
  expect_gte(psm.result1$pricerange_upper, psm.result1$pricerange_lower)
})

test_that("Data Output - Plausibility: Hierarchy between IDP, OPP and limits of price range", {
  expect_gte(psm.result1$idp, psm.result1$pricerange_lower)
  expect_lte(psm.result1$idp, psm.result1$pricerange_upper)

  expect_gte(psm.result1$opp, psm.result1$pricerange_lower)
  expect_lte(psm.result1$opp, psm.result1$pricerange_upper)
})

test_that("Data Output - Plausibility: Consistent price esimations across models", {
  expect_equal(psm.result1$pricerange_lower, psm.result2$pricerange_lower)
  expect_equal(psm.result1$pricerange_upper, psm.result2$pricerange_upper)
  expect_equal(psm.result1$idp, psm.result2$idp)
  expect_equal(psm.result1$opp, psm.result2$opp)
})

# clean up workspace after test
rm(data.psm.test, psm.result1, psm.result2)


