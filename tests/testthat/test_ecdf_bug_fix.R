context("ECDF Bug Fix Tests")

test_that("ECDF functions reach proper maximum values of 1.0", {
  # Create test data that specifically triggers the bug
  # toocheap has lower max than other variables
  set.seed(123)
  tch <- round(rnorm(n = 100, mean = 8.5, sd = 1), digits = 2)
  ch <- round(rnorm(n = 100, mean = 10, sd = 1), digits = 2)
  ex <- round(rnorm(n = 100, mean = 12, sd = 0.75), digits = 2)
  tex <- round(rnorm(n = 100, mean = 15, sd = 1), digits = 2)  # Higher mean to trigger bug
  
  data_psm_test <- data.frame(tch, ch, ex, tex)
  
  # Run PSM analysis
  result <- psm_analysis(
    toocheap = "tch",
    cheap = "ch", 
    expensive = "ex",
    tooexpensive = "tex",
    data = data_psm_test
  )
  
  # All ECDF functions should reach exactly 1.0 at their maximum
  expect_equal(max(result$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), 1.0,
               info = "ecdf_toocheap should reach maximum of 1.0")
  expect_equal(max(result$data_vanwestendorp$ecdf_cheap), 1.0,
               info = "ecdf_cheap should reach maximum of 1.0")
  expect_equal(max(result$data_vanwestendorp$ecdf_expensive), 1.0,
               info = "ecdf_expensive should reach maximum of 1.0")
  expect_equal(max(result$data_vanwestendorp$ecdf_tooexpensive), 1.0,
               info = "ecdf_tooexpensive should reach maximum of 1.0")
})

test_that("ECDF functions reach proper minimum values of 0.0", {
  # Create test data
  set.seed(456)
  tch <- round(rnorm(n = 100, mean = 5, sd = 1), digits = 2)   # Lower mean
  ch <- round(rnorm(n = 100, mean = 8, sd = 1), digits = 2)
  ex <- round(rnorm(n = 100, mean = 12, sd = 0.75), digits = 2)
  tex <- round(rnorm(n = 100, mean = 15, sd = 1), digits = 2)
  
  data_psm_test <- data.frame(tch, ch, ex, tex)
  
  # Run PSM analysis
  result <- psm_analysis(
    toocheap = "tch",
    cheap = "ch",
    expensive = "ex", 
    tooexpensive = "tex",
    data = data_psm_test
  )
  
  # All ECDF functions should reach exactly 0.0 at their minimum
  expect_equal(min(result$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), 0.0,
               info = "ecdf_toocheap should reach minimum of 0.0")
  expect_equal(min(result$data_vanwestendorp$ecdf_cheap), 0.0,
               info = "ecdf_cheap should reach minimum of 0.0")
  expect_equal(min(result$data_vanwestendorp$ecdf_expensive), 0.0,
               info = "ecdf_expensive should reach minimum of 0.0")
  expect_equal(min(result$data_vanwestendorp$ecdf_tooexpensive), 0.0,
               info = "ecdf_tooexpensive should reach minimum of 0.0")
})

test_that("ECDF bug reproduction with user's exact example", {
  # Reproduce the user's exact example
  set.seed(789)
  tch <- round(rnorm(n = 250, mean = 8.5, sd = 1), digits = 2)
  ch <- round(rnorm(n = 250, mean = 10, sd = 1), digits = 2)
  ex <- round(rnorm(n = 250, mean = 12, sd = 0.75), digits = 2)
  tex <- round(rnorm(n = 250, mean = 13, sd = 1), digits = 2)
  
  data_psm_demo <- data.frame(tch, ch, ex, tex)
  
  output_psm_demo <- psm_analysis(
    toocheap = "tch",
    cheap = "ch",
    expensive = "ex",
    tooexpensive = "tex",
    data = data_psm_demo
  )
  
  # This should pass after the fix
  expect_equal(max(output_psm_demo$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), 1.0,
               info = "User's example: ecdf_toocheap should reach maximum of 1.0")
})
