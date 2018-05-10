#---------------------
# Performance test of package
#---------------------

# code is not included in CRAN version in order to reduce workload for CRAN servers during package checking

library(microbenchmark)
library(pricesensitivitymeter)

# running 4 checks: regular version, with interpolation, with NMS, with interpolation+NMS
# on 2 levels: small dataset with 150 respondents, large dataset with 1000 respondents

# two different datasets sizes for testing: small dataset with n=150, large dataset with n=5000
data_small <- data.frame(toocheap = round(rnorm(n = 150, mean = 8, sd = 1.5), digits = 2),
                         cheap = round(rnorm(n = 150, mean = 15, sd = 3), digits = 2),
                         expensive = round(rnorm(n = 150, mean = 16, sd = 3), digits = 2),
                         tooexpensive = round(rnorm(n = 150, mean = 20, sd = 1), digits = 2),
                         pi_cheap = sample(x = c(1:5), size = 150, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
                         pi_expensive = sample(x = c(1:5), size = 150, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1)))

data_large <- data.frame(toocheap = round(rnorm(n = 5000, mean = 8, sd = 1.5), digits = 2),
                         cheap = round(rnorm(n = 5000, mean = 15, sd = 3), digits = 2),
                         expensive = round(rnorm(n = 5000, mean = 16, sd = 3), digits = 2),
                         tooexpensive = round(rnorm(n = 5000, mean = 20, sd = 1), digits = 2),
                         pi_cheap = sample(x = c(1:5), size = 5000, replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
                         pi_expensive = sample(x = c(1:5), size = 5000, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.1, 0.1)))

# running 4 different benchmarks:
# a) basic function: no NMS, no interpolation
# b) basic function with interpolation (no NMS)
# c) PSM with NMS, but no interpolation
# d) PSM with NMS and interpolation

###
# benchmarks for small dataset
benchmarks_small <- microbenchmark(psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_small, interpolate = FALSE),
                                   psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_small, interpolate = TRUE),
                                   psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_small, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", interpolate = FALSE),
                                   psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_small, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", interpolate = TRUE))

levels(benchmarks_small$expr) <- c("PSM (no NMS, no interpolation)",
                                   "PSM with interpolation (no NMS)",
                                   "PSM with NMS (no interpolation)",
                                   "PSM with NMS and interpolation")


###
# benchmarks for large dataset
benchmarks_large <- microbenchmark(psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_large, interpolate = FALSE),
                                   psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_large, interpolate = TRUE),
                                   psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_large, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", interpolate = FALSE),
                                   psm_analysis(toocheap = "toocheap", cheap = "cheap", expensive = "expensive", tooexpensive = "tooexpensive", data = data_large, pi_cheap = "pi_cheap", pi_expensive = "pi_expensive", interpolate = TRUE), times = 25)

levels(benchmarks_large$expr) <- c("PSM (no NMS, no interpolation)",
                                   "PSM with interpolation (no NMS)",
                                   "PSM with NMS (no interpolation)",
                                   "PSM with NMS and interpolation")

###
# writing all the results to a txt file

sink(file = "benchmarks/benchmark_results.txt", append = TRUE)
print("---------------------------------------")
print("pricensitivitymeter benchmarking results")
paste("package version", packageVersion("pricesensitivitymeter"))
cat("\n\n")

print("-------------")
print("Benchmarking results for small dataset")
summary(benchmarks_small)

print("-------------")
print("Benchmarking results for large dataset")
summary(benchmarks_large)

cat("\n")
Sys.time()
sessionInfo()
cat("\n\n")
sink()
