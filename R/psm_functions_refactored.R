#' @title Refactored PSM Analysis Functions
#' @description Improved versions of the main PSM analysis functions using modular design
#' @keywords internal

#' Van Westendorp Price Sensitivity Meter Analysis (Refactored Version)
#' 
#' @description This is the refactored version of the main PSM analysis function.
#'   It uses the new modular architecture for better maintainability while
#'   maintaining identical functionality and output to the original function.
#' 
#' @param toocheap Price point "too cheap" (numeric vector or column name)
#' @param cheap Price point "cheap" (numeric vector or column name)
#' @param expensive Price point "expensive" (numeric vector or column name)
#' @param tooexpensive Price point "too expensive" (numeric vector or column name)
#' @param data Optional data frame containing price variables
#' @param validate Whether to validate and filter inconsistent price preferences
#' @param interpolate Whether to apply interpolation for smoother curves
#' @param interpolation_steps Step size for interpolation
#' @param intersection_method Method for handling multiple intersection points
#' @param acceptable_range Definition of acceptable price range
#' @param pi_cheap Purchase intent at cheap price (for NMS extension)
#' @param pi_expensive Purchase intent at expensive price (for NMS extension)
#' @param pi_scale Purchase intent scale
#' @param pi_calibrated Calibrated purchase probabilities
#' @param pi_calibrated_toocheap Calibrated probability for too cheap price
#' @param pi_calibrated_tooexpensive Calibrated probability for too expensive price
#' 
#' @return Object of class "psm" containing analysis results
#' 
#' @examples
#' # Basic analysis with vectors
#' result <- psm_analysis_refactored(
#'   toocheap = c(1, 1.5, 2),
#'   cheap = c(2, 2.5, 3), 
#'   expensive = c(4, 4.5, 5),
#'   tooexpensive = c(5, 6, 7)
#' )
#' 
#' # Analysis with data frame
#' data <- data.frame(
#'   tc = c(1, 1.5, 2),
#'   ch = c(2, 2.5, 3),
#'   ex = c(4, 4.5, 5), 
#'   te = c(5, 6, 7)
#' )
#' result <- psm_analysis_refactored(
#'   data = data,
#'   toocheap = "tc", cheap = "ch", 
#'   expensive = "ex", tooexpensive = "te"
#' )
#' 
#' @export
psm_analysis_refactored <- function(toocheap, cheap, expensive, tooexpensive,
                                   data = NA, validate = TRUE, interpolate = FALSE,
                                   interpolation_steps = get_psm_constant("DEFAULT_INTERPOLATION_STEPS"),
                                   intersection_method = "min", acceptable_range = "original",
                                   pi_cheap = NA, pi_expensive = NA,
                                   pi_scale = get_psm_constant("NMS_DEFAULTS.PI_SCALE"),
                                   pi_calibrated = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED"),
                                   pi_calibrated_toocheap = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOCHEAP"),
                                   pi_calibrated_tooexpensive = get_psm_constant("NMS_DEFAULTS.PI_CALIBRATED_TOOEXPENSIVE")) {
  
  # Step 1: Validate all inputs (centralized, consistent validation)
  validation_info <- validate_psm_inputs(
    toocheap = toocheap, cheap = cheap, expensive = expensive, tooexpensive = tooexpensive,
    data = data, validate = validate, interpolate = interpolate,
    interpolation_steps = interpolation_steps, intersection_method = intersection_method,
    acceptable_range = acceptable_range, pi_cheap = pi_cheap, pi_expensive = pi_expensive,
    pi_scale = pi_scale, pi_calibrated = pi_calibrated,
    pi_calibrated_toocheap = pi_calibrated_toocheap,
    pi_calibrated_tooexpensive = pi_calibrated_tooexpensive
  )
  
  # Step 2: Prepare and clean data (handles all data formats consistently)
  prepared_data <- prepare_psm_data(
    toocheap = toocheap, cheap = cheap, expensive = expensive, tooexpensive = tooexpensive,
    data = data, pi_cheap = pi_cheap, pi_expensive = pi_expensive,
    pi_scale = pi_scale, pi_calibrated = pi_calibrated, validate = validate
  )
  
  # Step 3: Calculate empirical cumulative distribution functions
  ecdf_data <- calculate_ecdf_data(
    psmdata = prepared_data$data,
    weighted = prepared_data$weighted,
    survey_design = prepared_data$survey_design,
    interpolate = interpolate,
    interpolation_steps = interpolation_steps
  )
  
  # Step 4: Identify key price points (IDP, OPP, price range)
  price_points <- identify_price_points(
    data_ecdf = ecdf_data,
    intersection_method = intersection_method,
    acceptable_range = acceptable_range
  )
  
  # Step 5: Calculate NMS extension if requested
  nms_results <- NULL
  if (prepared_data$nms_requested) {
    nms_results <- calculate_nms_analysis(
      prepared_data = prepared_data,
      data_ecdf = ecdf_data,
      pi_calibrated_toocheap = pi_calibrated_toocheap,
      pi_calibrated_tooexpensive = pi_calibrated_tooexpensive
    )
  }
  
  # Step 6: Construct final result object
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

#' Weighted PSM Analysis (Refactored Version)
#' 
#' @description Refactored version of the weighted PSM analysis function.
#'   Uses the same modular architecture as the unweighted version,
#'   demonstrating significant code reuse and consistency.
#' 
#' @param toocheap Column name for too cheap prices
#' @param cheap Column name for cheap prices
#' @param expensive Column name for expensive prices
#' @param tooexpensive Column name for too expensive prices
#' @param design Survey design object from svydesign()
#' @param validate Whether to validate and filter inconsistent price preferences
#' @param interpolate Whether to apply interpolation for smoother curves
#' @param interpolation_steps Step size for interpolation
#' @param intersection_method Method for handling multiple intersection points
#' @param acceptable_range Definition of acceptable price range
#' @param pi_cheap Purchase intent at cheap price (for NMS extension)
#' @param pi_expensive Purchase intent at expensive price (for NMS extension)
#' @param pi_scale Purchase intent scale
#' @param pi_calibrated Calibrated purchase probabilities
#' @param pi_calibrated_toocheap Calibrated probability for too cheap price
#' @param pi_calibrated_tooexpensive Calibrated probability for too expensive price
#' 
#' @return Object of class "psm" containing weighted analysis results
#' 
#' @examples
#' \dontrun{
#' # Create survey design
#' library(survey)
#' data <- data.frame(
#'   tc = c(1, 2, 3), ch = c(2, 3, 4), ex = c(3, 4, 5), te = c(4, 5, 6),
#'   weights = c(1.2, 0.8, 1.0)
#' )
#' design <- svydesign(ids = ~1, weights = ~weights, data = data)
#' 
#' # Weighted analysis
#' result <- psm_analysis_weighted_refactored(
#'   toocheap = "tc", cheap = "ch", expensive = "ex", tooexpensive = "te",
#'   design = design
#' )
#' }
#' 
#' @export
psm_analysis_weighted_refactored <- function(toocheap, cheap, expensive, tooexpensive, design,
                                           validate = TRUE, interpolate = FALSE,
                                           interpolation_steps = get_psm_constant("DEFAULT_INTERPOLATION_STEPS"),
                                           intersection_method = "min", acceptable_range = "original",
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

#' Construct PSM result object
#' 
#' @description Creates the final PSM result object with all analysis components.
#'   This function ensures consistent result structure across all analysis types.
#' 
#' @param prepared_data Prepared data structure from prepare_psm_data()
#' @param ecdf_data ECDF calculation results from calculate_ecdf_data()
#' @param price_points Identified price points from identify_price_points()
#' @param nms_results NMS analysis results (optional, from calculate_nms_analysis())
#' @param acceptable_range Acceptable range definition used
#' @param pi_scale Purchase intent scale (for NMS)
#' @param pi_calibrated Calibrated probabilities (for NMS)
#' 
#' @return Object of class "psm" with standardized structure
#' 
#' @keywords internal
construct_psm_result <- function(prepared_data, ecdf_data, price_points, nms_results,
                                acceptable_range, pi_scale, pi_calibrated) {
  
  # Base PSM result structure (identical to original)
  output_psm <- list(
    data_input = prepared_data$data,
    validated = length(prepared_data$invalid_cases) > 0 && prepared_data$invalid_cases < prepared_data$total_sample,
    invalid_cases = prepared_data$invalid_cases,
    total_sample = prepared_data$total_sample,
    data_vanwestendorp = ecdf_data,
    pricerange_lower = price_points$pricerange_lower,
    pricerange_upper = price_points$pricerange_upper,
    idp = price_points$idp,
    opp = price_points$opp,
    acceptable_range_definition = acceptable_range,
    weighted = prepared_data$weighted,
    nms = prepared_data$nms_requested
  )
  
  # Add survey design for weighted analysis
  if (prepared_data$weighted) {
    output_psm$survey_design <- prepared_data$survey_design
  }
  
  # Add NMS results if calculated
  if (prepared_data$nms_requested && !is.null(nms_results)) {
    output_psm$data_nms <- nms_results$data_nms
    output_psm$pi_scale <- data.frame(pi_scale = pi_scale, pi_calibrated = pi_calibrated)
    output_psm$price_optimal_reach <- nms_results$price_optimal_reach
    output_psm$price_optimal_revenue <- nms_results$price_optimal_revenue
  }
  
  # Set class and return
  class(output_psm) <- "psm"
  return(output_psm)
}

#' Refactored PSM Plot Function
#' 
#' @description Improved version of the PSM plotting function using constants
#'   for better maintainability and consistency.
#' 
#' @param psm_result PSM analysis result object
#' @param shade_pricerange Whether to shade the acceptable price range
#' @param line_toocheap Whether to show "too cheap" line
#' @param line_tooexpensive Whether to show "too expensive" line
#' @param line_notcheap Whether to show "not cheap" line
#' @param line_notexpensive Whether to show "not expensive" line
#' @param point_idp Whether to show IDP point
#' @param point_color_idp Color for IDP point
#' @param label_idp Whether to label IDP point
#' @param point_opp Whether to show OPP point
#' @param point_color_opp Color for OPP point
#' @param label_opp Whether to label OPP point
#' @param pricerange_color Color for price range shading
#' @param pricerange_alpha Alpha for price range shading
#' @param line_color Colors for lines (uses constants if not specified)
#' @param line_type Line types (uses constants if not specified)
#' 
#' @return ggplot2 object
#' 
#' @export
psm_plot_refactored <- function(psm_result,
                               shade_pricerange = TRUE,
                               line_toocheap = TRUE,
                               line_tooexpensive = TRUE,
                               line_notcheap = TRUE,
                               line_notexpensive = TRUE,
                               point_idp = TRUE,
                               point_color_idp = get_psm_constant("DEFAULT_COLORS.IDP"),
                               label_idp = TRUE,
                               point_opp = TRUE,
                               point_color_opp = get_psm_constant("DEFAULT_COLORS.OPP"),
                               label_opp = TRUE,
                               pricerange_color = get_psm_constant("DEFAULT_COLORS.PRICE_RANGE"),
                               pricerange_alpha = get_psm_constant("DEFAULT_PLOT_SETTINGS.PRICE_RANGE_ALPHA"),
                               line_color = get_psm_constant("DEFAULT_COLORS.LINE_COLORS"),
                               line_type = get_psm_constant("DEFAULT_LINE_TYPES")) {
  
  # Ensure that only objects that are result of psm_analysis are accepted as input
  stopifnot(class(psm_result) == "psm")
  
  # Get plot settings from constants
  line_size <- get_psm_constant("DEFAULT_PLOT_SETTINGS.LINE_SIZE")
  point_size_idp <- get_psm_constant("DEFAULT_PLOT_SETTINGS.POINT_SIZE_IDP")
  point_size_opp <- get_psm_constant("DEFAULT_PLOT_SETTINGS.POINT_SIZE_OPP")
  point_shape_idp <- get_psm_constant("DEFAULT_PLOT_SETTINGS.POINT_SHAPE_IDP")
  point_shape_opp <- get_psm_constant("DEFAULT_PLOT_SETTINGS.POINT_SHAPE_OPP")
  
  # Base of plot
  plot_object <- ggplot2::ggplot(data = psm_result$data_vanwestendorp, ggplot2::aes(x = .data$price))
  
  # Shaded area for acceptable pricerange
  if (isTRUE(shade_pricerange)) {
    plot_object <- plot_object + ggplot2::annotate(
      geom = "rect",
      xmin = psm_result$pricerange_lower,
      xmax = psm_result$pricerange_upper,
      ymin = 0, ymax = Inf,
      fill = pricerange_color, alpha = pricerange_alpha
    )
  }
  
  # Line for "too cheap"
  if (isTRUE(line_toocheap)) {
    plot_object <- plot_object + ggplot2::geom_line(ggplot2::aes(
      y = .data$ecdf_toocheap,
      colour = "too cheap",
      linetype = "too cheap"
    ),
    size = line_size
    )
  }
  
  # Line for "too expensive"
  if (isTRUE(line_tooexpensive)) {
    plot_object <- plot_object + ggplot2::geom_line(ggplot2::aes(
      y = .data$ecdf_tooexpensive,
      colour = "too expensive",
      linetype = "too expensive"
    ),
    size = line_size
    )
  }
  
  # Line for "not cheap"
  if (isTRUE(line_notcheap)) {
    plot_object <- plot_object + ggplot2::geom_line(ggplot2::aes(
      y = .data$ecdf_not_cheap,
      colour = "not cheap",
      linetype = "not cheap"
    ),
    size = line_size
    )
  }
  
  # Line for "not expensive"
  if (isTRUE(line_notexpensive)) {
    plot_object <- plot_object + ggplot2::geom_line(ggplot2::aes(
      y = .data$ecdf_not_expensive,
      colour = "not expensive",
      linetype = "not expensive"
    ),
    size = line_size
    )
  }
  
  # Point for Indifference Price Point (intersection of "cheap" and "expensive")
  if (isTRUE(point_idp)) {
    plot_object <- plot_object + ggplot2::annotate(
      geom = "point",
      x = psm_result$idp,
      y = psm_result$data_vanwestendorp$ecdf_not_cheap[psm_result$data_vanwestendorp$price == psm_result$idp],
      size = point_size_idp,
      shape = point_shape_idp,
      colour = point_color_idp
    )
  }
  
  # Point for Optimal Price Point (intersection of "too cheap" and "too expensive")
  if (isTRUE(point_opp)) {
    plot_object <- plot_object + ggplot2::annotate(
      geom = "point",
      x = psm_result$opp,
      y = psm_result$data_vanwestendorp$ecdf_toocheap[psm_result$data_vanwestendorp$price == psm_result$opp],
      size = point_size_opp,
      shape = point_shape_opp,
      colour = point_color_opp
    )
  }
  
  # Adding line color and line style
  plot_object <- plot_object + ggplot2::scale_colour_manual(name = "Legend", values = line_color)
  plot_object <- plot_object + ggplot2::scale_linetype_manual(name = "Legend", values = line_type)
  
  # Adding label for Indifference Price Point
  if (isTRUE(label_idp)) {
    plot_object <- plot_object + ggplot2::annotate(
      geom = "label",
      x = psm_result$idp,
      y = psm_result$data_vanwestendorp$ecdf_not_cheap[psm_result$data_vanwestendorp$price == psm_result$idp],
      label = paste("IDP: ", format(psm_result$idp, nsmall = 2)),
      fill = "white",
      alpha = 0.5
    )
  }
  
  # Adding label for Optimal Price Point
  if (isTRUE(label_opp)) {
    plot_object <- plot_object + ggplot2::annotate(
      geom = "label",
      x = psm_result$opp,
      y = psm_result$data_vanwestendorp$ecdf_toocheap[psm_result$data_vanwestendorp$price == psm_result$opp],
      label = paste("OPP: ", format(psm_result$opp, nsmall = 2)),
      fill = "white",
      alpha = 0.5
    )
  }
  
  # Return the resulting ggplot object
  return(plot_object)
}
