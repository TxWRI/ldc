#' Summarize load duration curve
#'
#' Calculates summary statistics for flow and pollutant concentrations for
#' desired flow categories. Estimates "average" pollutant load per category
#' based on average concentration times the median flow.
#' @param .tbl data frame, prefferably the output from \code{calc_ldc()}.
#' @param Q variable name in .tbl for discharge or flow. This must have unit
#'   set, typically "ft^3/s".
#' @param C variable name in .tbl for associated pollutant concentration at a
#'   given flow value. This must have a unit set, typically "mg/L" or
#'   "cfu/100mL".
#' @param Exceedance variable name in .tbl with flow/load exceedance
#'   probabilities.
#' @param groups variable name in .tbl with categorized flow names.
#' @param method string that describes the summary statistic used for the
#'   pollutant concentration. Must be one of \code{c('geomean', 'mean',
#'   'median')}.
#'
#' @return object of class tibble. Includes Flow Category grouping variable,
#'   median flow and exceedance values, geometric mean/mean/median pollutant
#'   concentration, and estimated average load based on median flow times the
#'   average pollutant concentration per flow category.
#' @export
#' @import rlang dplyr
#' @importFrom stats quantile
#' @examples
#' # Basic example using built in Tres Palacios data
#' library(dplyr)
#' library(units)
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
#' df_sum
#'
#' ## cleanup
#' remove_unit("cfu")
#'
summ_ldc <- function(.tbl,
                     Q,
                     C,
                     Exceedance,
                     groups,
                     method = "geomean") {

  ## check that the summary method is appropriate
  if(!method %in% c("geomean", "mean", "median")) {
    stop("method must be one of 'geomean', 'mean', or 'median'")
  }

  ## grab units for C
  foo <- units(.tbl[[substitute(C)]])

  ## get concentration denominator
  C_den <- foo$denominator

  ## if length is zero, then it is not a concentration
  if(length(C_den) == 0) {
    stop(paste0(as_name(enquo(C)),
                " does not have valid units, it is missing a denominator"))
  }

  ## daily volume units based on concentration
  fv_units <- as_units(paste0(C_den, "/day"))

  if(method == "geomean") {
    .tbl <- .tbl %>%
      group_by(!! enquo(groups)) %>%
      summarize(Median_Flow = quantile(!! enquo(Q), .5, type = 5, names = FALSE, na.rm = TRUE),
                Median_P = quantile(!! enquo(Exceedance), .5, type = 5, names = FALSE, na.rm = TRUE),
                Geomean_C = Gmean(!! enquo(C), na.rm = TRUE)) %>%
      mutate(Geomean_C = set_units(.data$Geomean_C, foo, mode = "standard"),
             Median_Daily_Flow_Volume = set_units(.data$Median_Flow,
                                                  fv_units,
                                                  mode = "standard"),
             Median_Flow_Load = .data$Geomean_C * .data$Median_Daily_Flow_Volume)
  }

  if(method == "mean") {
    .tbl <- .tbl %>%
      group_by(!! enquo(groups)) %>%
      summarize(Median_Flow = quantile(!! enquo(Q), .5, type = 5, names = FALSE, na.rm = TRUE),
                Median_P = quantile(!! enquo(Exceedance), .5, type = 5, names = FALSE, na.rm = TRUE),
                Mean_C = mean(!! enquo(C), na.rm = TRUE)) %>%
      mutate(Mean_C = set_units(.data$Mean_C, foo, mode = "standard"),
             Median_Daily_Flow_Volume = set_units(.data$Median_Flow,
                                                  fv_units,
                                                  mode = "standard"),
             Median_Flow_Load = .data$Mean_C * .data$Median_Daily_Flow_Volume)
  }

  if(method == "median") {
    .tbl <- .tbl %>%
      group_by(!! enquo(groups)) %>%
      summarize(Median_Flow = quantile(!! enquo(Q), .5, type = 5, names = FALSE, na.rm = TRUE),
                Median_P = quantile(!! enquo(Exceedance), .5, type = 5, names = FALSE, na.rm = TRUE),
                Median_C = median(!! enquo(C), na.rm = TRUE)) %>%
      mutate(Median_C = set_units(.data$Median_C, foo, mode = "standard"),
             Median_Daily_Flow_Volume = set_units(.data$Median_Flow,
                                                  fv_units,
                                                  mode = "standard"),
             Median_Flow_Load = .data$Median_C * .data$Median_Daily_Flow_Volume)
  }
  return(.tbl)


}
