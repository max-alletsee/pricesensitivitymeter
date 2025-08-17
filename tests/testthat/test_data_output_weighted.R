context("Data Output Checks: Weighted Data")

#---
# Setting up some common objects that will be used during the tests
#---

library(survey)

input_data <- data.frame(tch = round(rnorm(n = 250, mean = 8, sd = 0.5), digits = 2),
                         ch = round(rnorm(n = 250, mean = 13, sd = 0.5), digits = 2),
                         ex = round(rnorm(n = 250, mean = 15, sd = 1), digits = 2),
                         tex = round(rnorm(n = 250, mean = 20, sd = 0.5), digits = 2),
                         pi_cheap = sample(x = c(1:5), size = 250,
                                           replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
                         pi_expensive = sample(x = c(1:5), size = 250,
                                               replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1)),
                         gender = sample(x = c("male", "female"),
                                         size = 250,
                                         replace = TRUE,
                                         prob = c(2 / 3, 1 / 3)))


input_data$tch[input_data$gender == "female"] <- input_data$tch[input_data$gender == "female"] * 2.5
input_data$ch[input_data$gender == "female"] <- input_data$ch[input_data$gender == "female"] * 2.5
input_data$ex[input_data$gender == "female"] <- input_data$ex[input_data$gender == "female"] * 2.5
input_data$tex[input_data$gender == "female"] <- input_data$tex[input_data$gender == "female"] * 2.5

input_data$gender_pop <- 5000

# 1) Creating an object that will then show the effect of correcting for misbalancing
input_design_1 <- survey::svydesign(ids = ~ 1, # no clusters
                                  probs = NULL, # hence no cluster samling probabilities,
                                  strata = input_data$gender, # stratified by gender
                                  fpc = input_data$gender_pop, # strata size in the population
                                  data = input_data) # data object used as input

psm_results_w1 <- psm_analysis_weighted(toocheap = "tch",
                                        cheap = "ch",
                                        expensive = "ex",
                                        tooexpensive = "tex",
                                        design = input_design_1)


# 2) Creating an object that will show no difference compared to an unweighted analysis

input_data_2 <- input_data

# for survey design object: occurence of each gender in the target population
# would usually be information from sampling frame, differs here only for demonstration purposes
# here: scaling up based on actual sample information (hypothetical population of 250 * 4 = 10k)
input_data_2$gender_pop <- NA
input_data_2$gender_pop[input_data_2$gender == "female"] <- sum(input_data_2$gender == "female") * 40
input_data_2$gender_pop[input_data_2$gender == "male"] <- sum(input_data_2$gender == "male") * 40

# creating the survey design object for post-stratification based on gender
input_design_2 <- survey::svydesign(ids = ~ 1, # no clusters
                                    probs = NULL, # hence no cluster sampling probabilities,
                                    strata = input_data_2$gender, # stratified by gender
                                    fpc = input_data_2$gender_pop, # strata size in the population
                                    data = input_data_2) # data object used as input

psm_results_w2 <- psm_analysis_weighted(toocheap = "tch",
                                        cheap = "ch",
                                        expensive = "ex",
                                        tooexpensive = "tex",
                                        design = input_design_2)

psm_results_unw <- psm_analysis(toocheap = "tch",
                                cheap = "ch",
                                expensive = "ex",
                                tooexpensive = "tex",
                                data = input_data_2)

# 3) Creating an object with NMS extension
psm_results_w3 <- psm_analysis_weighted(toocheap = "tch",
                                        cheap = "ch",
                                        expensive = "ex",
                                        tooexpensive = "tex",
                                        pi_cheap = "pi_cheap",
                                        pi_expensive = "pi_expensive",
                                        design = input_design_1)



#----
# Overall Features of the Output Object
#----

test_that("Data Output: Length of Output Object", {
  expect_length(psm_results_w1, 13)
  expect_length(psm_results_w2, 13)
})

test_that("Data Output: Class of Output Object", {
  expect_equal(class(psm_results_w1), "psm")
  expect_equal(class(psm_results_w2), "psm")
})

#----
# Internal Structure of the Output Object
#----

test_that("Data Output: Matrices have rows and columns", {
  expect_gt(nrow(psm_results_w1$data_input), 0)
  expect_gt(nrow(psm_results_w1$data_vanwestendorp), 0)
  expect_gt(nrow(psm_results_w2$data_input), 0)
  expect_gt(nrow(psm_results_w2$data_vanwestendorp), 0)
  expect_equal(ncol(psm_results_w1$data_input), 4)
  expect_equal(ncol(psm_results_w1$data_vanwestendorp), 7)

  expect_equal(ncol(psm_results_w3$data_input), 8)
  expect_equal(ncol(psm_results_w3$data_vanwestendorp), 7)
  expect_equal(ncol(psm_results_w3$data_nms), 3)
})

test_that("Data Output: Numeric Data in Matrices", {
  expect_true(unique(apply(psm_results_w1$data_input[, c("toocheap", "cheap", "expensive", "tooexpensive")], 2, is.numeric)))
  expect_true(unique(apply(psm_results_w1$data_vanwestendorp, 2, is.numeric)))

  # No expectation for psm_results_w3$data_input possible, as some cluster/strata names may be factors or character variables
  expect_true(unique(sapply(psm_results_w3$data_vanwestendorp, is.numeric)))
  expect_true(unique(sapply(psm_results_w3$data_nms, is.numeric)))
})



test_that("Data Output: Output Object Structure (Analysis without NMS)", {
  expect_true(is.logical(psm_results_w1$validated))
  expect_length(psm_results_w1$validated, 1)

  expect_true(is.logical(psm_results_w1$weighted))
  expect_length(psm_results_w1$weighted, 1)
  expect_true(psm_results_w1$weighted)

  expect_true(is.numeric(psm_results_w1$invalid_cases))
  expect_false(is.nan(psm_results_w1$invalid_cases))
  expect_length(psm_results_w1$invalid_cases, 1)

  expect_true(is.numeric(psm_results_w1$total_sample))
  expect_false(is.nan(psm_results_w1$total_sample))
  expect_length(psm_results_w1$total_sample, 1)

  expect_true(is.numeric(psm_results_w1$pricerange_lower))
  expect_false(is.nan(psm_results_w1$pricerange_lower))
  expect_length(psm_results_w1$pricerange_lower, 1)

  expect_true(is.numeric(psm_results_w1$pricerange_upper))
  expect_false(is.nan(psm_results_w1$pricerange_upper))
  expect_length(psm_results_w1$pricerange_upper, 1)

  expect_true(is.numeric(psm_results_w1$idp))
  expect_false(is.nan(psm_results_w1$idp))
  expect_length(psm_results_w1$idp, 1)

  expect_true(is.numeric(psm_results_w1$opp))
  expect_false(is.nan(psm_results_w1$opp))
  expect_length(psm_results_w1$opp, 1)

  expect_true(is.logical(psm_results_w1$nms))
  expect_false(is.nan(psm_results_w1$nms))
  expect_length(psm_results_w1$nms, 1)
})


test_that("Data Output: Output Object Structure (Analysis with NMS)", {
expect_true(is.logical(psm_results_w3$validated))
expect_length(psm_results_w3$validated, 1)

expect_true(is.numeric(psm_results_w3$invalid_cases))
expect_false(is.nan(psm_results_w3$invalid_cases))
expect_length(psm_results_w3$invalid_cases, 1)

expect_true(is.numeric(psm_results_w3$total_sample))
expect_false(is.nan(psm_results_w3$total_sample))
expect_length(psm_results_w3$total_sample, 1)

expect_true(is.numeric(psm_results_w3$pricerange_lower))
expect_false(is.nan(psm_results_w3$pricerange_lower))
expect_length(psm_results_w3$pricerange_lower, 1)

expect_true(is.numeric(psm_results_w3$pricerange_upper))
expect_false(is.nan(psm_results_w3$pricerange_upper))
expect_length(psm_results_w3$pricerange_upper, 1)

expect_true(is.numeric(psm_results_w3$idp))
expect_false(is.nan(psm_results_w3$idp))
expect_length(psm_results_w3$idp, 1)

expect_true(is.numeric(psm_results_w3$opp))
expect_false(is.nan(psm_results_w3$opp))
expect_length(psm_results_w3$opp, 1)

expect_true(is.logical(psm_results_w3$nms))
expect_false(is.nan(psm_results_w3$nms))
expect_length(psm_results_w3$nms, 1)

expect_true(is.numeric(psm_results_w3$pi_scale$pi_calibrated))
expect_false(unique(is.nan(psm_results_w3$pi_scale$pi_calibrated)))

expect_true(is.numeric(psm_results_w3$price_optimal_reach))
expect_false(is.nan(psm_results_w3$price_optimal_reach))
expect_length(psm_results_w3$price_optimal_reach, 1)

expect_true(is.numeric(psm_results_w3$price_optimal_revenue))
expect_false(is.nan(psm_results_w3$price_optimal_revenue))
expect_length(psm_results_w3$price_optimal_revenue, 1)
})


#----
# Expecting Specific Values
#----

test_that("Data Output: NMS correctly (not) included in output", {
  expect_false(psm_results_w1$nms)
  expect_true(psm_results_w3$nms)
})

test_that("Data Output: All prices included in the empirical cumulative density function data", {
  expect_true(unique(round(psm_results_w1$data_input$toocheap, digits = 2) %in% psm_results_w1$data_vanwestendorp$price))
  expect_true(unique(round(psm_results_w1$data_input$cheap, digits = 2) %in% psm_results_w1$data_vanwestendorp$price))
  expect_true(unique(round(psm_results_w1$data_input$expensive, digits = 2) %in% psm_results_w1$data_vanwestendorp$price))
  expect_true(unique(round(psm_results_w1$data_input$tooexpensive, digits = 2) %in% psm_results_w1$data_vanwestendorp$price))
})

test_that("Data Output: All prices included in the NMS data", {
  expect_equal(min(psm_results_w3$data_vanwestendorp$price), min(psm_results_w3$data_nms$price))
  expect_equal(max(psm_results_w3$data_vanwestendorp$price), max(psm_results_w3$data_nms$price))
})


#----
# Plausible Values
#----

test_that("Data Output - Plausibility: Number of total cases must be greater than the number of invalid cases", {
  expect_gt(psm_results_w1$total_sample, psm_results_w1$invalid_cases)
  expect_gt(psm_results_w2$total_sample, psm_results_w2$invalid_cases)
})

test_that("Data Output - Plausibility: Price estimations must be within range of provided prices", {
  expect_gte(psm_results_w1$pricerange_lower, min(psm_results_w1$data_vanwestendorp$price))
  expect_lte(psm_results_w1$pricerange_lower, max(psm_results_w1$data_vanwestendorp$price))

  expect_gte(psm_results_w1$pricerange_upper, min(psm_results_w1$data_vanwestendorp$price))
  expect_lte(psm_results_w1$pricerange_upper, max(psm_results_w1$data_vanwestendorp$price))

  expect_gte(psm_results_w1$idp, min(psm_results_w1$data_vanwestendorp$price))
  expect_lte(psm_results_w1$idp, max(psm_results_w1$data_vanwestendorp$price))

  expect_gte(psm_results_w1$opp, min(psm_results_w1$data_vanwestendorp$price))
  expect_lte(psm_results_w1$opp, max(psm_results_w1$data_vanwestendorp$price))

  expect_gte(psm_results_w3$price_optimal_reach, min(psm_results_w3$data_vanwestendorp$price))
  expect_lte(psm_results_w3$price_optimal_reach, max(psm_results_w3$data_vanwestendorp$price))

  expect_gte(psm_results_w3$price_optimal_revenue, min(psm_results_w3$data_vanwestendorp$price))
  expect_lte(psm_results_w3$price_optimal_revenue, max(psm_results_w3$data_vanwestendorp$price))
})

test_that("Data Output - Plausibility: Hierarchy between lower and upper limit of price range", {
  expect_gte(psm_results_w1$pricerange_upper, psm_results_w1$pricerange_lower)
})

test_that("Data Output - Plausibility: Hierarchy between IDP, OPP and limits of price range", {
  expect_gte(psm_results_w1$idp, psm_results_w1$pricerange_lower)
  expect_lte(psm_results_w1$idp, psm_results_w1$pricerange_upper)

  expect_gte(psm_results_w1$opp, psm_results_w1$pricerange_lower)
  expect_lte(psm_results_w1$opp, psm_results_w1$pricerange_upper)
})

test_that("Data Output - Plausibility: Different weighting should result in different results", {
  expect_false(psm_results_w1$pricerange_lower == psm_results_w2$pricerange_lower &&
    psm_results_w1$pricerange_upper == psm_results_w2$pricerange_upper &&
    psm_results_w1$idp == psm_results_w2$idp &&
    psm_results_w1$opp == psm_results_w2$opp)
})


test_that("Data Output - Plausibility: If data is misbalanced, weighted analysis should result in different results than unweighted analysis", {
  expect_false(psm_results_w1$pricerange_lower == psm_results_unw$pricerange_lower &&
                 psm_results_w1$pricerange_upper == psm_results_unw$pricerange_upper &&
                 psm_results_w1$idp == psm_results_unw$idp &&
                 psm_results_w1$opp == psm_results_unw$opp)
})

test_that("Data Output - Plausibility: If data is not misbalanced, weighted analysis and unweighted analysis should result in the same results", {
  # Internal Note: Checking for absence of invalid cases is necessary as the survey package re-weights directly *after* removing the invalid cases, meaning that all weights will not be equal anymore. This would lead to cases where the point estimates are not equal anymore, which is driven by the adjustment of weights after removing the invalid cases.
  if(psm_results_w2$invalid_cases == 0) {
    expect_lte(abs(psm_results_w2$pricerange_lower - psm_results_unw$pricerange_lower), 0.05)
    expect_lte(abs(psm_results_w2$pricerange_upper - psm_results_unw$pricerange_upper), 0.05)
    expect_lte(abs(psm_results_w2$idp - psm_results_unw$idp), 0.05)
    expect_lte(abs(psm_results_w2$opp - psm_results_unw$opp), 0.05)
  }
})

test_that("Data Output - Plausibility: If data is misbalanced, weighting should pull the results in the direction of the underrepresented group", {
  # In the example "input_data"/"input_design1", women are underrepresented and have a higher price tolerance.
  # When using a weighted PSM analysis, the overall price points should be higher as we correct for the under-representation of women
  expect_gt(psm_results_w1$pricerange_lower, psm_results_unw$pricerange_lower)
  expect_gt(psm_results_w1$pricerange_upper, psm_results_unw$pricerange_upper)
  expect_gt(psm_results_w1$idp, psm_results_unw$idp)
  expect_gt(psm_results_w1$opp, psm_results_unw$opp)
})

#----
# Structure of Output Object
#----

test_that("Ensuring that cases with invalid cases are removed", {
  expect_equal(nrow(psm_results_w1$data_input), psm_results_w1$total_sample - psm_results_w1$invalid_cases)
})

# clean up workspace after test
rm(input_data, input_data_2, input_design_1, input_design_2, psm_results_w1, psm_results_w2, psm_results_unw, psm_results_w3)
