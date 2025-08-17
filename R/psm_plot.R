#---------------------
# Plotting function for standard PSM plot
#---------------------

psm_plot <- function(psm_result,
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

  # Call the refactored plotting function with identical parameters
  # This maintains 100% backward compatibility while using the improved architecture
  psm_plot_refactored(
    psm_result = psm_result,
    shade_pricerange = shade_pricerange,
    line_toocheap = line_toocheap,
    line_tooexpensive = line_tooexpensive,
    line_notcheap = line_notcheap,
    line_notexpensive = line_notexpensive,
    point_idp = point_idp,
    point_color_idp = point_color_idp,
    label_idp = label_idp,
    point_opp = point_opp,
    point_color_opp = point_color_opp,
    label_opp = label_opp,
    pricerange_color = pricerange_color,
    pricerange_alpha = pricerange_alpha,
    line_color = line_color,
    line_type = line_type
  )
}
