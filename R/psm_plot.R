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
    linewidth = line_size
    )
  }
  
  # Line for "too expensive"
  if (isTRUE(line_tooexpensive)) {
    plot_object <- plot_object + ggplot2::geom_line(ggplot2::aes(
      y = .data$ecdf_tooexpensive,
      colour = "too expensive",
      linetype = "too expensive"
    ),
    linewidth = line_size
    )
  }
  
  # Line for "not cheap"
  if (isTRUE(line_notcheap)) {
    plot_object <- plot_object + ggplot2::geom_line(ggplot2::aes(
      y = .data$ecdf_not_cheap,
      colour = "not cheap",
      linetype = "not cheap"
    ),
    linewidth = line_size
    )
  }
  
  # Line for "not expensive"
  if (isTRUE(line_notexpensive)) {
    plot_object <- plot_object + ggplot2::geom_line(ggplot2::aes(
      y = .data$ecdf_not_expensive,
      colour = "not expensive",
      linetype = "not expensive"
    ),
    linewidth = line_size
    )
  }
  
  # Point for Indifference Price Point (intersection of "cheap" and "expensive")
  if (isTRUE(point_idp)) {
    # Use interpolation to find y-coordinate at exact IDP price
    y_idp <- approx(x = psm_result$data_vanwestendorp$price, 
                    y = psm_result$data_vanwestendorp$ecdf_not_cheap, 
                    xout = psm_result$idp)$y
    
    plot_object <- plot_object + ggplot2::annotate(
      geom = "point",
      x = psm_result$idp,
      y = y_idp,
      size = point_size_idp,
      shape = point_shape_idp,
      colour = point_color_idp
    )
  }
  
  # Point for Optimal Price Point (intersection of "too cheap" and "too expensive")
  if (isTRUE(point_opp)) {
    # Use interpolation to find y-coordinate at exact OPP price
    y_opp <- approx(x = psm_result$data_vanwestendorp$price, 
                    y = psm_result$data_vanwestendorp$ecdf_toocheap, 
                    xout = psm_result$opp)$y
    
    plot_object <- plot_object + ggplot2::annotate(
      geom = "point",
      x = psm_result$opp,
      y = y_opp,
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
    # Use interpolation to find y-coordinate at exact IDP price for label
    y_idp_label <- approx(x = psm_result$data_vanwestendorp$price, 
                          y = psm_result$data_vanwestendorp$ecdf_not_cheap, 
                          xout = psm_result$idp)$y
    
    plot_object <- plot_object + ggplot2::annotate(
      geom = "label",
      x = psm_result$idp,
      y = y_idp_label,
      label = paste("IDP: ", format(psm_result$idp, nsmall = 2)),
      fill = "white",
      alpha = 0.5
    )
  }
  
  # Adding label for Optimal Price Point
  if (isTRUE(label_opp)) {
    # Use interpolation to find y-coordinate at exact OPP price for label
    y_opp_label <- approx(x = psm_result$data_vanwestendorp$price, 
                          y = psm_result$data_vanwestendorp$ecdf_toocheap, 
                          xout = psm_result$opp)$y
    
    plot_object <- plot_object + ggplot2::annotate(
      geom = "label",
      x = psm_result$opp,
      y = y_opp_label,
      label = paste("OPP: ", format(psm_result$opp, nsmall = 2)),
      fill = "white",
      alpha = 0.5
    )
  }
  
  # Return the resulting ggplot object
  return(plot_object)
}
