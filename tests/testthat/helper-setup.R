# Helper file to set up test environment
# This file sources all R files to make functions available during testing

# Get the path to the R directory
pkg_root <- file.path("..", "..")
r_dir <- file.path(pkg_root, "R")

# Source all R files
if (dir.exists(r_dir)) {
  r_files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  for (file in r_files) {
    source(file, local = FALSE)
  }
}