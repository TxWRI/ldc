#' Summarize load duration curve
#'
#' @param .tbl data frame, prefferably the output from \code{calc_ldc()}.
#' @param Q variable name in .tbl for discharge or flow. This must have unit set, typically "ft^3/s".
#' @param C variable name in .tbl for associated pollutant concentration at a given flow value. This must have a unit set, typically "mg/L" or "cfu/100mL".
#' @param Exceedance variable name in .tbl with flow/load exceedance probabilities.
#' @param groups variable name in .tbl with categorized flow names.
#' @param method string that describes the summary statistic used for the pollutant concentration. Must be one of \code{c('geomean', 'mean', 'median')}.
#'
#' @return a data frame
#' @export
#' @import rlang dplyr
#' @importFrom stats quantile
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
