#' Draw a load duration curve
#'
#' Creates a load duration curve visualization from the outputs of
#' \code{calc_ldc} and \code{summ_ldc} as a ggplot object.
#'
#' @param .tbl_calc data frame object created by \code{calc_ldc}
#' @param .tbl_summ data frame object created by \code{summ_ldc}
#' @param y_lab optional string for y-axis label name, will be appended with
#'   units automatically. default is NULL.
#' @param ldc_legend_name string, provides the name used for the allowable
#'   pollutant load line in the legend. required.
#' @param measurement_name string, provides the name used for measured load
#'   values in the legend. required.
#' @param measurement_shape aesthetic value passed to the layer plotting
#'   measured load values. defaults to \code{21}.
#' @param measurement_color aesthetic value passed to the layer plotting
#'   measured load values. defaults to \code{"dodgerblue"}.
#' @param measurement_alpha aesthetic value passed to the layer plotting
#'   measured load values. defaults to \code{1}.
#' @param summary_name string, provides the name used for summary statistic
#'   values in the legend. required.
#' @param summary_stat_shape aesthetic value passed to the layer plotting
#'   summary statistic values. defaults to \code{12}.
#' @param summary_stat_color aesthetic value passed to the layer plotting
#'   summary statistic values. defaults to \code{"red"}.
#' @param label_nudge_y numeric value to vertically nudge flow category labels.
#'   If a log10 transformed scale is being used, a log value is probably
#'   appropriate for example \code{log10(1000)}.
#' @param label_font_family string specifying font family to use in flow
#'   category labels.
#' @param label_font_size numeric value specifying font size to use in flow
#'   category labels.
#' @param label_break logical, add line breaks to flow category labels. Labels
#'   will break at spaces.
#'
#' @return ggplot object
#' @export
#' @import ggplot2 rlang
#' @examples
#' # Basic example using built in Tres Palacios data
#' library(dplyr)
#' library(units)
#' library(ggplot2)
#' # Format data
#' install_unit("cfu")
#' df <- as_tibble(tres_palacios) %>%
#'         ## filter data so this run quicker
#'         filter(!is.na(Indicator_Bacteria)) %>%
#'         ## flow must have units, here is is in cfs
#'         mutate(Flow = set_units(Flow, "ft^3/s")) %>%
#'         ## pollutant concentration must have units
#'         mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu/100mL"))
#' # Calculate LDC
#'
#' ## specify the allowable concentration
#' allowable_concentration <- 126
#' ## set the units
#' units(allowable_concentration) <- "cfu/100mL"
#' df_ldc <- calc_ldc(df,
#'                    Q = Flow,
#'                    C = Indicator_Bacteria,
#'                    allowable_concentration = allowable_concentration)
#'
#' # Summarize LDC
#' df_sum <- summ_ldc(df_ldc,
#'                    Q = Flow,
#'                    C = Indicator_Bacteria,
#'                    Exceedance = P_Exceedance,
#'                    groups = Flow_Category,
#'                    method = "geomean")
#'
#' # Create ggplot object
#' draw_ldc(df_ldc,
#'          df_sum,
#'          y_lab = expression(paste(italic("E. coli"))),
#'          label_nudge_y = log10(1000)) +
#'          scale_y_log10() +
#'          theme(legend.title = element_blank(),
#'                legend.direction = "vertical",
#'                legend.position = "bottom")
#'
#' ## cleanup
#' remove_unit("cfu")
#'
draw_ldc <- function(.tbl_calc,
                     .tbl_summ,
                     y_lab = NULL,
                     ldc_legend_name = "Allowable Load at State Water Quality Standard",
                     measurement_name = "Measurement Value",
                     measurement_shape = 21,
                     measurement_color = "dodgerblue",
                     measurement_alpha = 1,
                     summary_name = "Summarized Measured Load",
                     summary_stat_shape = 12,
                     summary_stat_color = "red",
                     label_nudge_y = 0,
                     label_font_family = "Arial",
                     label_font_size = 3,
                     label_break = TRUE) {

  ## make y label
  units.y <- as_units(units(.tbl_calc$Daily_Load))
  ylab <- units::make_unit_label("Daily_Load",
                                 units.y)
  if(!is.null(y_lab)) {
    ylab <- parse(text = gsub("Daily_Load",
                              paste0(y_lab),
                              ylab,
                              fixed = TRUE))
  }

  ## define values for scales
  shapes_values <- c(summary_stat_shape, measurement_shape)
  names(shapes_values)[1] <- summary_name
  names(shapes_values)[2] <- measurement_name

  colors_values <- c(summary_stat_color, measurement_color)
  names(colors_values)[1] <- summary_name
  names(colors_values)[2] <- measurement_name

  ## if flow category labels break, break them here
  if(isTRUE(label_break)) {
    .tbl_summ <- .tbl_summ %>%
      mutate(Flow_Category = gsub(" ", "\n", .data$Flow_Category))
  }

  ## create ggplot object
  ggplot(.tbl_calc) +
    ## add flow category breaks
    geom_vline(xintercept = attr(.tbl_calc, "breaks"), color = "#cccccc") +
    ## plot the allowable load (ldc) line
    geom_line(aes(.data$P_Exceedance,
                  as.numeric(.data$Allowable_Daily_Load),
                  linetype = ldc_legend_name)) +
    ## plot the measured loads
    geom_point(aes(.data$P_Exceedance,
                   as.numeric(.data$Daily_Load),
                   shape = measurement_name,
                   color = measurement_name),
               alpha = measurement_alpha) +
    ## plot the summary stats
    geom_point(data = .tbl_summ,
               aes(x = .data$Median_P,
                   y = as.numeric(.data$Median_Flow_Load),
                   shape = summary_name,
                   color = summary_name)) +
    ## add flow category labels
    geom_text(data = .tbl_summ,
              aes(x = .data$Median_P,
                  y = max(as.numeric(.data$Median_Flow_Load)),
                  label = .data$Flow_Category),
              nudge_y = label_nudge_y,
              size = label_font_size,
              hjust = 0.5) +
    ## add axis labels
    labs(y = ylab, x = "Proportion Days Load Exceeded") +
    ## specify scales
    scale_shape_manual(name = "values",
                       values = shapes_values) +
    scale_color_manual(name = "values",
                       values = colors_values) +
    ## some theme options
    theme(legend.title = element_blank())


}
