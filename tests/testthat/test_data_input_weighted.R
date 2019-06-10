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
                         pi_cheap = sample(x = c(1:5), size = 250,
                                           replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
                         pi_expensive = sample(x = c(1:5), size = 250,
                                               replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1)),
                         gender = sample(x = c("male", "female"),
                                         size = 250,
                                         replace = TRUE,
                                         prob = c(2/3, 1/3)))

# manipulating one row to ensure one case with intransitive price preferences
input_data$ch_invalid <- input_data$ch
random_row <- sample(x = nrow(input_data), size = 1)
input_data$ch_invalid[random_row] <- input_data$ex[random_row] + 0.5


# input_data$tch[input_data$gender == "female"] <- input_data$tch[input_data$gender == "female"] * 1.5
# input_data$ch[input_data$gender == "female"] <- input_data$ch[input_data$gender == "female"] * 1.5
# input_data$ex[input_data$gender == "female"] <- input_data$ex[input_data$gender == "female"] * 1.5
# input_data$tex[input_data$gender == "female"] <- input_data$tex[input_data$gender == "female"] * 1.5

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
# Ensure that survey package is available
#----

test_that("Survey Package is installed", {
  expect_true("survey" %in% rownames(installed.packages()))
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

#----
# Detecting invalid input data: "intersection_method" must be one of the pre-defined values
#----

test_that("Data Input: intersection_method must be one of the pre-defined values", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, intersection_method =  c("min", "max")))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, intersection_method = 1))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, intersection_method = TRUE))

  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, intersection_method = "min"))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, intersection_method = "max"))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, intersection_method = "mean"))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, intersection_method = "median"))
}
)

#---
# General NMS Options (length of PI scale and calibration scale, match between answers and defined pattern, numeric calibration values, warning if calibration out of bounds)
#---

test_that("Data Input - Weighted NMS: length of PI scale and calibration scale", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 1:6, pi_calibrated = seq(0, 1, length.out = 5)))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 1:5, pi_calibrated = seq(0, 1, length.out = 6)))
  expect_silent(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 1:5, pi_calibrated = seq(0, 1, length.out = 5)))
}
)

test_that("Data Input: NMS - match between answers and defined pattern", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 2:5, pi_calibrated = seq(0, 1, length.out = 4)))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = c(1.1,2:5), pi_calibrated = seq(0, 1, length.out = 5)))
}
)

test_that("Data Input: NMS - numeric calibration values", {
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = letters[1:5]))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = factor(seq(0, 1, length.out = 5))))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = rep(NA, 5)))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = c(TRUE, TRUE, TRUE, FALSE, FALSE)))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = c(NaN, NaN, NaN, NaN, NaN)))
}
)

test_that("Data Input: NMS - warning if calibration values out of bounds", {
  expect_warning(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = 5:1))
  expect_warning(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = -5:-1))
  expect_warning(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = seq(-0.25, 0.5, length.out = 5)))
  expect_error(psm_analysis_weighted(toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", pi_scale = 5:1, pi_calibrated = c(Inf, 0, 0, 0, -Inf)))
}
)


#----
# Validation of response patterns
#----


test_that("(In)Transitive Preference Structures- Weighted Analysis", {
  expect_warning(psm_analysis_weighted(toocheap = "tch", cheap = "ch_invalid", expensive = "ex", tooexpensive = "tex", design = input_design, validate = FALSE))
})


#----
# Not specifying any "too cheap" price should be handled by the function
#----

test_that("Weighted Analysis: Running analysis while too cheap price is missing",
  expect_silent(psm_analysis_weighted(toocheap = "tch_empty", cheap = "ch", expensive = "ex", tooexpensive = "tex", design = input_design)))


# clean up workspace after test
rm(input_data, input_design, random_row)
