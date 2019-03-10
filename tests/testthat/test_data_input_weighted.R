context("Data Input Checks: Weighted Data")

#---
# Setting up some common objects that will be used during the tests
#---

library(survey)

input_data <- data.frame(tch = round(rnorm(n = 250, mean = 8, sd = 0.5), digits = 2),
                         ch = round(rnorm(n = 250, mean = 12, sd = 0.5), digits = 2),
                         ex = round(rnorm(n = 250, mean = 13, sd = 0.5), digits = 2),
                         tex = round(rnorm(n = 250, mean = 15, sd = 0.5), digits = 2),
                         tch_empty = NA,
                         ch_wrong = as.factor(round(rnorm(n = 250, mean = 12, sd = 0.5), digits = 2)),
                         ex_wrong = as.character(round(rnorm(n = 250, mean = 13, sd = 0.5), digits = 2)),
                         tex_wrong = rep(TRUE, times = 250),
                         gender = sample(x = c("male", "female"),
                                         size = 250,
                                         replace = TRUE,
                                         prob = c(2/3, 1/3)))

input_data$tch[input_data$gender == "female"] <- input_data$tch[input_data$gender == "female"] * 1.5
input_data$ch[input_data$gender == "female"] <- input_data$ch[input_data$gender == "female"] * 1.5
input_data$ex[input_data$gender == "female"] <- input_data$ex[input_data$gender == "female"] * 1.5
input_data$tex[input_data$gender == "female"] <- input_data$tex[input_data$gender == "female"] * 1.5

input_data$gender_pop <- 5000

input_design <- survey::svydesign(ids = ~ 1, # no clusters
                                  probs = NULL, # hence no cluster samling probabilities,
                                  strata = input_data$gender, # stratified by gender
                                  fpc = input_data$gender_pop, # strata size in the population
                                  data = input_data) # data object used as input


#----
# Detecting invalid input data: input object must be an object of class "survey.design"
#----

test_that("Data Input - Weighted Analysis: Structure and Variable Names", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                                     design = input_data))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "2", expensive = "ex", tooexpensive = "tex",
                                     design = input_design))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = ex, tooexpensive = "tex",
                                     design = input_design))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = c("tex",
                                     "tex"), design = input_design))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex",
                                     design = input_design))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                                      design = input_design))

})


#----
# Variable Format inside data frame
#----

test_that("Data Input - Weighted Analysis: data frame variable format", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch_wrong", expensive = "ex", tooexpensive = "tex", design = input_design))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex_wrong", tooexpensive = "tex", design = input_design))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex_wrong", design = input_design))
})


#----
# Detecting invalid input data: "validate" must be a logical vector
#----

test_that("Data Input - Weighted Analysis: validate must be logical vector of length 1", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, validate = "yes"))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, validate = 2))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, validate = c(TRUE, TRUE)))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, validate = TRUE))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, validate = FALSE))
})

#----
# Detecting invalid input data: "interpolate" must be a logical vector
#----
test_that("Data Input - Weighted Analysis: interpolate must be logical vector of length 1", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, interpolate = "yes"))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, interpolate = 2))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, interpolate = c(TRUE, TRUE)))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, interpolate = TRUE))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, interpolate = FALSE))
})

#---
# Some Placeholders for a Potential Future Implementation of Weighted NMS:
# NMS Input: Variables in a Data Frame
# General NMS Options (length of PI scale and calibration scale, match between answers and defined pattern, numeric calibration values, warning if calibration out of bounds)
#---


# #----
# # Validation of response patterns
# #----
#
# random.row <- sample(x = nrow(data.psm.test), size = 1)
#
# data.psm.test$ch[random.row] <- data.psm.test$ex[random.row] + 0.5
#
# test_that("(In)Transitive Preference Structures", {
#   expect_warning(psm_analysis(data = data.psm.test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", validate = FALSE))
#   expect_error(psm_analysis(toocheap = 2, cheap = 1, expensive = 3, tooexpensive = 4))
# }
# )


#----
# Not specifying any "too cheap" price should be handled by the function
#----

test_that("Running analysis while too cheap price is missing",
  expect_silent(psm_analysis_weighted(toocheap = "tch_empty", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design)))


# clean up workspace after test
rm(input_data, input_design)
