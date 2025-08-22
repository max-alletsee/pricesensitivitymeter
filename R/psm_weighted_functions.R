#---------------------
# Implementing van Westendorp's PSM in R
# ... with the possibility of having weights
#---------------------

psm_analysis_weighted <- function(toocheap, cheap, expensive, tooexpensive, design,
                                  validate = TRUE,
                                  interpolate = FALSE, interpolation_steps = get_psm_constant("DEFAULT_INTERPOLATION_STEPS"),
                                  intersection_method = "min",
                                  acceptable_range = "original",
                                  pi_cheap = NA, pi_expensive = NA,
                                  pi_scale = get_psm_constant("NMS_DEFAULTS.PI_SCALE"),
                                  pi_calibrated = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED"),
                                  pi_calibrated_toocheap = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOCHEAP"),
                                  pi_calibrated_tooexpensive = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOEXPENSIVE")) {

  # Step 1: Validate inputs (including survey design)
  validation_info <- validate_psm_inputs(
    toocheap = toocheap, cheap = cheap, expensive = expensive, tooexpensive = tooexpensive,
    data = NULL, validate = validate, interpolate = interpolate,
    interpolation_steps = interpolation_steps, intersection_method = intersection_method,
    acceptable_range = acceptable_range, pi_cheap = pi_cheap, pi_expensive = pi_expensive,
    pi_scale = pi_scale, pi_calibrated = pi_calibrated,
    pi_calibrated_toocheap = pi_calibrated_toocheap,
    pi_calibrated_tooexpensive = pi_calibrated_tooexpensive,
    design = design  # Key difference: pass survey design
  )
  
  # Step 2: Prepare data (automatically handles weighted case)
  prepared_data <- prepare_psm_data(
    toocheap = toocheap, cheap = cheap, expensive = expensive, tooexpensive = tooexpensive,
    design = design,  # Key difference: pass survey design instead of data
    pi_cheap = pi_cheap, pi_expensive = pi_expensive,
    pi_scale = pi_scale, pi_calibrated = pi_calibrated, validate = validate
  )
  
  # Steps 3-6: Identical to unweighted version!
  # The functions automatically handle weighted vs unweighted based on prepared_data structure
  
  ecdf_data <- calculate_ecdf_data(
    psmdata = prepared_data$data,
    weighted = prepared_data$weighted,
    survey_design = prepared_data$survey_design,
    interpolate = interpolate,
    interpolation_steps = interpolation_steps
  )
  
  price_points <- identify_price_points(
    data_ecdf = ecdf_data,
    intersection_method = intersection_method,
    acceptable_range = acceptable_range
  )
  
  nms_results <- NULL
  if (prepared_data$nms_requested) {
    nms_results <- calculate_nms_analysis(
      prepared_data = prepared_data,
      data_ecdf = ecdf_data,
      pi_calibrated_toocheap = pi_calibrated_toocheap,
      pi_calibrated_tooexpensive = pi_calibrated_tooexpensive
    )
  }
  
  construct_psm_result(
    prepared_data = prepared_data,
    ecdf_data = ecdf_data,
    price_points = price_points,
    nms_results = nms_results,
    acceptable_range = acceptable_range,
    pi_scale = pi_scale,
    pi_calibrated = pi_calibrated
  )
}
