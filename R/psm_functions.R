#---------------------
# Implementing van Westendorp's PSM in R
#---------------------

psm_analysis <- function(toocheap, cheap, expensive, tooexpensive, data = NA,
                         validate = TRUE,
                         interpolate = FALSE, interpolation_steps = 0.01,
                         intersection_method = "min",
                         acceptable_range = "original",
                         pi_cheap = NA, pi_expensive = NA,
                         pi_scale = 5:1, pi_calibrated = c(0.7, 0.5, 0.3, 0.1, 0),
                         pi_calibrated_toocheap = 0, pi_calibrated_tooexpensive = 0) {

  # Call the refactored implementation with identical parameters
  # This maintains 100% backward compatibility while using the improved architecture
  psm_analysis_refactored(
    toocheap = toocheap,
    cheap = cheap,
    expensive = expensive,
    tooexpensive = tooexpensive,
    data = data,
    validate = validate,
    interpolate = interpolate,
    interpolation_steps = interpolation_steps,
    intersection_method = intersection_method,
    acceptable_range = acceptable_range,
    pi_cheap = pi_cheap,
    pi_expensive = pi_expensive,
    pi_scale = pi_scale,
    pi_calibrated = pi_calibrated,
    pi_calibrated_toocheap = pi_calibrated_toocheap,
    pi_calibrated_tooexpensive = pi_calibrated_tooexpensive
  )
}
