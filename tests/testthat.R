library(testthat)

# Source all R files since package may not be installed
r_files <- list.files("../../R", pattern = "\\.R$", full.names = TRUE)
for (file in r_files) {
  source(file, local = FALSE)
}

test_check("pricesensitivitymeter")
