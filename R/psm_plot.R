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
                     label_idp = TRUE,
                     point_opp = TRUE,
                     label_opp= TRUE,
                     pricerange_color = "grey50",
                     pricerange_alpha = 0.3,
                     line_color = c("too cheap" = "#009E73",
                                    "not cheap" = "#009E73",
                                    "not expensive" = "#D55E00",
                                    "too expensive" = "#D55E00"),
                     line_type = c("too cheap" = "dotted",
                                    "not cheap" = "solid",
                                    "not expensive" = "solid",
                                    "too expensive" = "dotted")
) {
  # ensure that only objects that are result of psm_analysis/psm_analysis_weighted are accepted as input
  stopifnot(class(psm_result) == "psm")

  # base of plot
  plot_object <- ggplot2::ggplot(data = psm_result$data_vanwestendorp, aes(x = .data$price))

  # shaded area for acceptable pricerange
  if(isTRUE(shade_pricerange)) {
    plot_object <- plot_object + ggplot2::annotate(geom = "rect",
                                          xmin = psm_result$pricerange_lower,
                                          xmax = psm_result$pricerange_upper,
                                          ymin = 0, ymax = Inf,
                                          fill = pricerange_color, alpha = pricerange_alpha)
  }

  # line for "too cheap"
  if(isTRUE(line_toocheap)) {
    plot_object <- plot_object + ggplot2::geom_line(aes(y = .data$ecdf_toocheap,
                                               colour = "too cheap",
                                               linetype = "too cheap"),
                                           size= 1)
  }

  # line for "too expensive"
  if(isTRUE(line_tooexpensive)) {
    plot_object <- plot_object + ggplot2::geom_line(aes(y = .data$ecdf_tooexpensive,
                                               colour = "too expensive",
                                               linetype = "too expensive"),
                                           size= 1)
  }

  # line for "not cheap"
  if(isTRUE(line_notcheap)) {
    plot_object <- plot_object + ggplot2::geom_line(aes(y = .data$ecdf_not_cheap,
                                               colour = "not cheap",
                                               linetype = "not cheap"),
                                           size= 1)
  }

  # line for "not expensive"
  if(isTRUE(line_notexpensive)) {
    plot_object <- plot_object + ggplot2::geom_line(aes(y = .data$ecdf_not_expensive,
                                               colour = "not expensive",
                                               linetype = "not expensive"),
                                           size= 1)
  }


  # point for Indifference Price Point (intersection of "cheap" and "expensive")
  if(isTRUE(point_idp)) {
    plot_object <- plot_object + ggplot2::annotate(geom = "point",
                                          x = psm_result$idp,
                                          y = psm_result$data_vanwestendorp$ecdf_not_cheap[psm_result$data_vanwestendorp$price == psm_result$idp],
                                          size = 5,
                                          shape = 18,
                                          colour = "#009E73")
  }

  # point for Optimal Price Point (intersection of "too cheap" and "too expensive")
  if(isTRUE(point_idp)) {
    plot_object <- plot_object + ggplot2::annotate(geom = "point",
                                          x = psm_result$opp,
                                          y = psm_result$data_vanwestendorp$ecdf_toocheap[psm_result$data_vanwestendorp$price == psm_result$opp],
                                          size = 3,
                                          shape = 17,
                                          colour = "#009E73")
  }


  # Adding line color and line style

  plot_object <- plot_object + ggplot2::scale_colour_manual(name = "Legend", values = line_color)
  plot_object <- plot_object + ggplot2::scale_linetype_manual(name="Legend", values = line_type)

  # Adding label for Indifference Price Point

  if(isTRUE(label_idp)) {
  plot_object <- plot_object + ggplot2::annotate(geom = "text",
                                        x = psm_result$idp + 1.5,
                                        y = psm_result$data_vanwestendorp$ecdf_not_cheap[psm_result$data_vanwestendorp$price == psm_result$idp],
                                        label = paste("IDP: ", psm_result$idp))

  }


  if(isTRUE(label_opp)) {
    plot_object <- plot_object + ggplot2::annotate(geom = "text",
                                          x = psm_result$opp + 1.5,
                                          y = psm_result$data_vanwestendorp$ecdf_toocheap[psm_result$data_vanwestendorp$price == psm_result$opp],
                                          label = paste("OPP: ", psm_result$opp))

  }

  # return the resulting ggplot object
  return(plot_object)
}
