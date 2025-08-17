# Test script to verify the ECDF bug fix
library(pricesensitivitymeter)

# Create test data that triggers the bug
set.seed(123)
tch <- round(rnorm(n = 250, mean = 8.5, sd = 1), digits = 2)
ch <- round(rnorm(n = 250, mean = 10, sd = 1), digits = 2)
ex <- round(rnorm(n = 250, mean = 12, sd = 0.75), digits = 2)
tex <- round(rnorm(n = 250, mean = 13, sd = 1), digits = 2)

data_psm_demo <- data.frame(tch, ch, ex, tex)

# Run the analysis
output_psm_demo <- psm_analysis(
  toocheap = "tch",
  cheap = "ch",
  expensive = "ex",
  tooexpensive = "tex",
  data = data_psm_demo
)

# Check the results
cat("Summary of ECDF results:\n")
print(summary(output_psm_demo$data_vanwestendorp))

cat("\nMaximum values for each ECDF:\n")
cat("ecdf_toocheap max:", max(output_psm_demo$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), "\n")
cat("ecdf_cheap max:", max(output_psm_demo$data_vanwestendorp$ecdf_cheap), "\n")
cat("ecdf_expensive max:", max(output_psm_demo$data_vanwestendorp$ecdf_expensive), "\n")
cat("ecdf_tooexpensive max:", max(output_psm_demo$data_vanwestendorp$ecdf_tooexpensive), "\n")

cat("\nMinimum values for each ECDF:\n")
cat("ecdf_toocheap min:", min(output_psm_demo$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE), "\n")
cat("ecdf_cheap min:", min(output_psm_demo$data_vanwestendorp$ecdf_cheap), "\n")
cat("ecdf_expensive min:", min(output_psm_demo$data_vanwestendorp$ecdf_expensive), "\n")
cat("ecdf_tooexpensive min:", min(output_psm_demo$data_vanwestendorp$ecdf_tooexpensive), "\n")

# Test if all ECDFs reach proper bounds
all_max_correct <- all(
  max(output_psm_demo$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE) == 1.0,
  max(output_psm_demo$data_vanwestendorp$ecdf_cheap) == 1.0,
  max(output_psm_demo$data_vanwestendorp$ecdf_expensive) == 1.0,
  max(output_psm_demo$data_vanwestendorp$ecdf_tooexpensive) == 1.0
)

all_min_correct <- all(
  min(output_psm_demo$data_vanwestendorp$ecdf_toocheap, na.rm = TRUE) == 0.0,
  min(output_psm_demo$data_vanwestendorp$ecdf_cheap) == 0.0,
  min(output_psm_demo$data_vanwestendorp$ecdf_expensive) == 0.0,
  min(output_psm_demo$data_vanwestendorp$ecdf_tooexpensive) == 0.0
)

cat("\nBug fix verification:\n")
cat("All ECDFs reach maximum of 1.0:", all_max_correct, "\n")
cat("All ECDFs reach minimum of 0.0:", all_min_correct, "\n")

if (all_max_correct && all_min_correct) {
  cat("\n✓ SUCCESS: Bug has been fixed! All ECDFs now reach proper bounds.\n")
} else {
  cat("\n✗ FAILURE: Bug still exists.\n")
}
