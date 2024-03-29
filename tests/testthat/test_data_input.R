context("Data Input Checks")

#----
# Detecting invalid input data: data as single vectors
#----

test_that("Data Input: vectors", {
          expect_error(psm_analysis(toocheap = "a", cheap = 2, expensive = 3, tooexpensive = 4))
          expect_error(psm_analysis(toocheap = 1, cheap = factor(2), expensive = 3, tooexpensive = 4))
          expect_error(psm_analysis(toocheap = 1, cheap = 2, tooexpensive = 4))
          expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = NA))
          expect_error(psm_analysis(toocheap = matrix(1), cheap = 2, expensive = 3, tooexpensive = 4))
          expect_error(psm_analysis(toocheap = 1, cheap = FALSE, expensive = 3, tooexpensive = 4))
          expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = rep.int(3, times = 2), tooexpensive = 4))
          expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4))
}
)

#----
# Detecting invalid input data: data as data frame
#----

data_psm_test <- data.frame(tch = round(rnorm(n = 20, mean = 5, sd = 0.5), digits = 2),
                            ch = round(rnorm(n = 20, mean = 8.5, sd = 0.5), digits = 2),
                            ex = round(rnorm(n = 20, mean = 13, sd = 0.75), digits = 2),
                            tex = round(rnorm(n = 20, mean = 17, sd = 1), digits = 2))

test_that("Data Input: data frame input structure", {
  expect_silent(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex"))
  expect_silent(psm_analysis(data = as.matrix(data_psm_test), toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex"))

  expect_error(psm_analysis(data = "data_psm_test", toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = round(rnorm(n = 20, mean = 5, sd = 0.5), digits = 2),
                            cheap = round(rnorm(n = 20, mean = 8.5, sd = 0.5), digits = 2),
                            expensive = round(rnorm(n = 20, mean = 13, sd = 0.75), digits = 2),
                            tooexpensive = round(rnorm(n = 20, mean = 17, sd = 1), digits = 2)))
  expect_error(psm_analysis(data = data_psm_test, toocheap = tch, cheap = "ch", expensive = "ex", tooexpensive = "tex"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "2", expensive = "ex", tooexpensive = "tex"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = c("tex", "tex")))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "", expensive = "ex", tooexpensive = "tex"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", expensive = "ex", tooexpensive = "tex"))

}
)

data_psm_test2 <- data_psm_test
data_psm_test2$ch <- as.factor(data_psm_test2$ch)

data_psm_test3 <- data_psm_test
data_psm_test3$ex <- as.character(data_psm_test3$ex)

data_psm_test4 <- data_psm_test
data_psm_test4$tex <- rep(TRUE, times = nrow(data_psm_test4))

test_that("Data Input: data frame variable format", {
  expect_error(psm_analysis(data = data_psm_test2, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex"))
  expect_error(psm_analysis(data = data_psm_test3, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex"))
  expect_error(psm_analysis(data = data_psm_test4, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex"))
}
)

#----
# Detecting invalid input data: "validate" must be a logical vector
#----
test_that("Data Input: validate must be logical vector of length 1", {
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, validate = "yes"))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, validate = c(TRUE, TRUE)))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, validate = TRUE))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, validate = FALSE))
}
)

#----
# Detecting invalid input data: "interpolate" must be a logical vector
#----
test_that("Data Input: interpolate must be logical vector of length 1", {
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = "yes"))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = c(TRUE, TRUE)))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = TRUE))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = FALSE))
}
)

#----
# Detecting invalid input data: "interpolation_steps" must be valid if interpolate == TRUE
# (but can be off when interpolate == FALSE)
#----
test_that("Data Input: interpolatation_steps must be numeric vector of length 1", {
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = TRUE, interpolation_steps = c(0, 1)))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = TRUE, interpolation_steps = "default"))

  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = TRUE, interpolation_steps = 1))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = FALSE, interpolation_steps = c(0, 1)))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, interpolate = FALSE, interpolation_steps = "default"))
}
)

#----
# Detecting invalid input data: "intersection_method" must be one of the pre-defined values
#----

test_that("Data Input: intersection_method must be one of the pre-defined values", {
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, intersection_method =  c("min", "max")))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, intersection_method = 1))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, intersection_method = TRUE))

  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, intersection_method = "min"))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, intersection_method = "max"))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, intersection_method = "mean"))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, intersection_method = "median"))
}
)

#----
# Detecting invalid input data: NMS using vectors
#----

test_that("Data Input: NMS using vectors", {
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, pi_cheap = 1, pi_expensive = factor(2)))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, pi_cheap = 1, pi_expensive = matrix(2)))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, pi_cheap = TRUE, pi_expensive = 2))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, pi_cheap = 1, pi_expensive = rep.int(2, 2)))
  expect_error(psm_analysis(toocheap = c(1, 1), cheap = c(2, 2), expensive = c(3, 3), tooexpensive = c(4, 4), pi_cheap = 1, pi_expensive = rep.int(2, 2)))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, pi_cheap = -1, pi_expensive = 2))
  expect_error(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, pi_cheap = 1, pi_expensive = -2))
  expect_error(psm_analysis(toocheap = c(5, 6), cheap = c(8, 9), expensive = c(12, 14), tooexpensive = c(16, 18), pi_cheap = 3, pi_expensive = 2))
  expect_silent(psm_analysis(toocheap = 1, cheap = 2, expensive = 3, tooexpensive = 4, pi_cheap = 3, pi_expensive = 2))
}
)


#----
# Detecting invalid input data: NMS using data frames
#----

data_psm_test$pi_cheap <- sample(x = c(1:5), size = nrow(data_psm_test),
                                 replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3))

data_psm_test$pi_cheap2 <- sample(x = letters[1:5], size = nrow(data_psm_test),
                                  replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3))

data_psm_test$pi_cheap3 <- NA

data_psm_test$pi_expensive <- sample(x = c(1:5), size = nrow(data_psm_test),
                                     replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1))

data_psm_test$pi_expensive2 <- as.factor(data_psm_test$pi_expensive)

data_psm_test$pi_expensive3 <- sample(x = c(TRUE, FALSE), size = nrow(data_psm_test),
                                      replace = TRUE, prob = c(0.5, 0.5))

test_that("Data Input: NMS using dataframe", {
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "foo", pi_expensive = "bar"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap2", pi_expensive = "pi_expensive"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive2"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap3", pi_expensive = "pi_expensive"))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive3"))
  expect_silent(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive"))
}
)


#----
# Detecting invalid input data: General NMS Options
#----


test_that("Data Input: NMS - length of PI scale and calibration scale", {
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 1:6, pi_calibrated = seq(0, 1, length.out = 5)))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 1:5, pi_calibrated = seq(0, 1, length.out = 6)))
  expect_silent(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 1:6, pi_calibrated = seq(0, 1, length.out = 6)))
}
)

test_that("Data Input: NMS - match between answers and defined pattern", {
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 2:5, pi_calibrated = seq(0, 1, length.out = 4)))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = c(1.1, 2:5), pi_calibrated = seq(0, 1, length.out = 5)))
}
)

test_that("Data Input: NMS - numeric calibration values", {
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 5:1, pi_calibrated = letters[1:5]))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 5:1, pi_calibrated = factor(seq(0, 1, length.out = 5))))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 5:1, pi_calibrated = rep(NA, 5)))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 5:1, pi_calibrated = c(TRUE, TRUE, TRUE, FALSE, FALSE)))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 5:1, pi_calibrated = c(NaN, NaN, NaN, NaN, NaN)))
}
)

test_that("Data Input: NMS - warning if calibration values out of bounds", {
  expect_warning(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                              pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                              pi_scale = 5:1, pi_calibrated = 5:1))
  expect_warning(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                              pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                              pi_scale = 5:1, pi_calibrated = -5:-1))
  expect_warning(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                              pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                              pi_scale = 5:1, pi_calibrated = seq(-0.25, 0.5, length.out = 5)))
  expect_error(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex",
                            pi_cheap = "pi_cheap", pi_expensive = "pi_expensive",
                            pi_scale = 5:1, pi_calibrated = c(Inf, 0, 0, 0, -Inf)))
}
)


#----
# Validation of response patterns
#----

random_row <- sample(x = nrow(data_psm_test), size = 1)

data_psm_test$ch[random_row] <- data_psm_test$ex[random_row] + 0.5

test_that("(In)Transitive Preference Structures", {
  expect_warning(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex", validate = FALSE))
  expect_error(psm_analysis(toocheap = 2, cheap = 1, expensive = 3, tooexpensive = 4))
}
)

#----
# Not specifying any "too cheap" price should be handled by the function
#----

data_psm_test$tch <- NA

test_that("Running analysis while too cheap price is missing", {
  expect_silent(psm_analysis(toocheap = NA, cheap = 1, expensive = 3, tooexpensive = 4))
  expect_silent(psm_analysis(data = data_psm_test, toocheap = "tch", cheap = "ch", expensive = "ex", tooexpensive = "tex"))
}
)




# clean up workspace after test
rm(data_psm_test, data_psm_test2, data_psm_test3, data_psm_test4, random_row)
