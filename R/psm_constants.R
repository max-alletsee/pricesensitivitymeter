#' @title Constants for Price Sensitivity Meter Analysis
#' @description Internal constants used throughout the pricesensitivitymeter package
#' @keywords internal
#' @noRd

# Package-wide constants
PSM_CONSTANTS <- list(
  # Numerical precision
  ROUNDING_DIGITS = 2L,
  DEFAULT_INTERPOLATION_STEPS = 0.01,
  
  # Validation constants
  VALID_INTERSECTION_METHODS = c("min", "max", "mean", "median"),
  VALID_ACCEPTABLE_RANGES = c("original", "narrower"),
  
  # Default plotting colors
  DEFAULT_COLORS = list(
    IDP = "#009E73",
    OPP = "#009E73",
    PRICE_RANGE = "grey50",
    LINE_COLORS = c(
      "too cheap" = "#009E73",
      "not cheap" = "#009E73", 
      "not expensive" = "#D55E00",
      "too expensive" = "#D55E00"
    )
  ),
  
  # Default plotting settings
  DEFAULT_PLOT_SETTINGS = list(
    PRICE_RANGE_ALPHA = 0.3,
    LINE_SIZE = 1,
    POINT_SIZE_IDP = 5,
    POINT_SIZE_OPP = 3,
    POINT_SHAPE_IDP = 18,
    POINT_SHAPE_OPP = 17
  ),
  
  # Default line types
  DEFAULT_LINE_TYPES = c(
    "too cheap" = "dotted",
    "not cheap" = "solid",
    "not expensive" = "solid", 
    "too expensive" = "dotted"
  ),
  
  # NMS Extension defaults
  NMS_DEFAULTS = list(
    PI_SCALE = 5:1,
    PI_CALIBRATED = c(0.7, 0.5, 0.3, 0.1, 0),
    PI_CALIBRATED_TOOCHEAP = 0,
    PI_CALIBRATED_TOOEXPENSIVE = 0
  )
)

# Helper function to get constants
get_psm_constant <- function(path) {
  # Handle empty string
  if (is.null(path) || length(path) == 0 || path == "") {
    stop("Constant not found: ", path)
  }
  
  keys <- strsplit(path, "\\.")[[1]]
  result <- PSM_CONSTANTS
  
  tryCatch({
    for (key in keys) {
      if (is.null(result) || !is.list(result) || !key %in% names(result)) {
        stop("Constant not found: ", path)
      }
      result <- result[[key]]
    }
    result
  }, error = function(e) {
    # Ensure we always return the "Constant not found" message
    if (!grepl("Constant not found", e$message)) {
      stop("Constant not found: ", path)
    } else {
      stop(e)
    }
  })
}
