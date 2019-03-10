context("Data Output Checks: Weighted Data")

#---
# Setting up some common objects that will be used during the tests
#---

library(survey)

input_data <- data.frame(tch = round(rnorm(n = 250, mean = 8, sd = 0.5), digits = 2),
                         ch = round(rnorm(n = 250, mean = 12, sd = 0.5), digits = 2),
                         ex = round(rnorm(n = 250, mean = 13, sd = 0.5), digits = 2),
                         tex = round(rnorm(n = 250, mean = 15, sd = 0.5), digits = 2),
                         gender = sample(x = c("male", "female"),
                                         size = 250,
                                         replace = TRUE,
                                         prob = c(2/3, 1/3)))

input_data$tch[input_data$gender == "female"] <- input_data$tch[input_data$gender == "female"] * 1.5
input_data$ch[input_data$gender == "female"] <- input_data$ch[input_data$gender == "female"] * 1.5
input_data$ex[input_data$gender == "female"] <- input_data$ex[input_data$gender == "female"] * 1.5
input_data$tex[input_data$gender == "female"] <- input_data$tex[input_data$gender == "female"] * 1.5

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


#----
# Overall Features of the Output Object
#----

test_that("Data Output: Length of Output Object", {
  expect_length(psm_results_w1, 12)
  expect_length(psm_results_w2, 12)
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

  expect_equal(ncol(psm_results_w1$data_input), 7)
  expect_equal(ncol(psm_results_w1$data_vanwestendorp), 7)

  # Placeholder here: Expectation for objects with NSM analysis
})

test_that("Data Output: Numeric Data in Matrices", {
  expect_true(unique(sapply(psm_results_w1$data_input[, c("toocheap", "cheap", "expensive", "tooexpensive")], is.numeric)))
  expect_true(unique(sapply(psm_results_w1$data_vanwestendorp[, c("toocheap", "cheap", "expensive", "tooexpensive")], is.numeric)))

  # Placeholder here: Expectation for objects with NSM analysis
})



test_that("Data Output: Output Object Structure (Analysis without NSM)", {
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

  expect_true(is.logical(psm_results_w1$NMS))
  expect_false(is.nan(psm_results_w1$NMS))
  expect_length(psm_results_w1$NMS, 1)

  # Placeholder here: Expectation for objects with NSM analysis
})

#----
# Expecting Specific Values
#----

test_that("Data Output: NMS correctly (not) included in output", {
  expect_false(psm_results_w1$NMS)
})

test_that("Data Output: All prices included in the empirical cumulative density function data", {
  expect_true(unique(psm_results_w1$data_input$toocheap %in% psm_results_w1$data_vanwestendorp$price))
  expect_true(unique(psm_results_w1$data_input$cheap %in% psm_results_w1$data_vanwestendorp$price))
  expect_true(unique(psm_results_w1$data_input$expensive %in% psm_results_w1$data_vanwestendorp$price))
  expect_true(unique(psm_results_w1$data_input$tooexpensive %in% psm_results_w1$data_vanwestendorp$price))
})

# Placeholder here: Expectation for objects with NSM analysis - All prices included in the NSM data


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

  # Placeholder here: Expectation for objects with NSM analysis
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

# TODO - FIXME: CURRENTLY DOES NOT WORK, CONTINUE TO INSPECT THE INITIAL OBJECT
# test_that("Data Output - Plausibility: If data is not misbalanced, weighted analysis and unweighted analysis should result in the same results", {
#   expect_equal(psm_results_w2$pricerange_lower, psm_results_unw$pricerange_lower)
#   expect_equal(psm_results_w2$pricerange_upper, psm_results_unw$pricerange_upper)
#   expect_equal(psm_results_w2$idp, psm_results_unw$idp)
#   expect_equal(psm_results_w2$opp, psm_results_unw$opp)
# })



## TODO: commonalities/differences between weighted and unweighted model
## TODO: higher price points for underrepresented groups / lower price points for overrepresented groups

# clean up workspace after test
rm(input_data, input_data_2, input_design_1, input_design_2, psm_results_w1, psm_results_w2, psm_results_unw)


