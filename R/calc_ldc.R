#' Calculate load duration curve
#'
#' Calculates the period of record load duration curve from a data frame that
#' includes mean daily flow and associated point measurements of pollutant
#' concentration.
#'
#' @param .tbl data frame with at least two columns Q (discharge or flow) and C
#'   (associated pollutant concentration).
#' @param Q variable name in .tbl for discharge or flow. This must have unit
#'   set, typically "ft^3/s".
#' @param C variable name in .tbl for associated pollutant concentration at a
#'   given flow value. This must have a unit set, typically "mg/L" or
#'   "cfu/100mL".
#' @param allowable_concentration an object of class \code{units} specifying the
#'   allowable pollutant concentration.
#' @param breaks a numeric vector of break points for flow categories. Must be
#'   of length of labels + 1. defaults to \code{c(1, 0.8, 0.4, 0)}.
#' @param labels labels for the categories specified by breaks.
#' @param estimator numeric, one of \code{c(5,6,7,8,9)}. \code{6} is the default
#'   method correponding to the Weibull plotting position. Further details are
#'   provided in \code{stats::quantile()}.
#'
#' @return object of class tibble. Includes variables in .tbl and
#'   Daily_Flow_Volume (discharge volume), Daily_Load (pollutant sample volume),
#'   P_Exceedance (exeedance probability), Flow_Category (as defined by breaks
#'   and labels).
#' @details The exceedance probability is calculated from the descending order
#'   of Daily Flows. By default, the Weibull plotting position is used:
#'   \deqn{p = P(Q > q_i) =  \frac{i}{n+1}}
#'   where \eqn{q_i, i = 1, 2, ... n}, is the i-th sorted streamflow value.
#' @import rlang dplyr units
#' @importFrom stats median
#' @export
#' @examples
#' # Basic example using built in Tres Palacios data
#' library(dplyr)
#' library(units)
#' # Format data
#' install_unit("cfu")
#' df <- as_tibble(tres_palacios) %>%
#'   ## filter data so this run quicker
#'   filter(!is.na(Indicator_Bacteria)) %>%
#'   ## flow must have units, here is is in cfs
#'   mutate(Flow = set_units(Flow, "ft^3/s")) %>%
#'   ## pollutant concentration must have units
#'   mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu/100mL"))
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
#' df_ldc
#'
#' ## cleanup
#' remove_unit("cfu")
calc_ldc <- function(.tbl,
                     Q = NULL,
                     C = NULL,
                     allowable_concentration = NULL,
                     breaks = c(1, 0.8, 0.4, 0),
                     labels = c("High Flows", "Medium Flows", "Low Flows"),
                     estimator = 6) {


  ## basic checks
  if(is.null(substitute(Q))) {
    stop(paste0("Please specify a col 'Q' from '", as_name(enquo(.tbl)),"'"))
  }

  if(is.null(substitute(C))) {
    stop(paste0("Please specify a col 'C' from '", as_name(enquo(.tbl)), "'"))
  }

  if(is.null(allowable_concentration)) {
    stop("'allowable_concentration' cannot be 'NULL'")
  }

  if(!(estimator %in% c(5,6,7,8,9))) {
    stop("'estimator' must be one of 'c(5,6,7,8,9)'")
  }

  ## check that Q and C have units
  if (class(.tbl[[substitute(Q)]]) != "units") {
    stop(paste0(as_name(enquo(Q)),
                " does not have units"))
  }
  if (class(.tbl[[substitute(C)]]) != "units") {
    stop(paste0(as_name(enquo(C)),
                " does not have units"))
  }
  if (class(allowable_concentration) != "units") {
    stop("'allowable_concentration' does not have units")
  }

  ## get concentration denominator
  C_den <- units(.tbl[[substitute(C)]])$denominator

  ## if length is zero, then it is not a concentration
  if(length(C_den) == 0) {
    stop(paste0(as_name(enquo(C)),
                " does not have valid units, it is missing a denominator"))
  }

  ## daily volume units based on concentration
  fv_units <- as_units(paste0(C_den, "/day"))

  .tbl %>%
    as_tibble(.tbl) %>%
    arrange(!! enquo(Q)) %>%
    mutate(
      ## daily flow in units of the concentration denominator
      Daily_Flow_Volume = set_units(!! enquo(Q),
                                         fv_units,
                                         mode = "standard"),
      ## daily measured load
      Daily_Load = .data$Daily_Flow_Volume * !! enquo(C),
      ## daily allowable load
      Allowable_Daily_Load = .data$Daily_Flow_Volume * allowable_concentration,
      ## proportion of days flow exceeded
      P_Exceedance = p_estimator(!! enquo(Q), estimator = estimator),
      ## bin the flow exceedance values
      Flow_Category = cut(.data$P_Exceedance, breaks = breaks, labels = labels,
                          right = FALSE)) -> .tbl

  attr(.tbl, "breaks") <- breaks
  return(.tbl)

}


#' Estimate exceedance probability
#'
#' @param Q vector of streamflow values
#' @param estimator numeric, values one of 5, 6, 7, 8, or 9. Type 6 corresponds to Weibull plotting position and is the default
#'
#' @return vector of streamflow percentiles
#' @noRd
#' @keywords internal
#' @importFrom stats ppoints
p_estimator <- function(Q,
                        estimator = 6) {

  if (estimator == 5) a = 0.5
  if (estimator == 6) a = 0 # weibull default
  if (estimator == 7) a = 1
  if (estimator == 8) a = 1/3
  if (estimator == 9) a = 3/8

  pp <- 1 - stats::ppoints(Q, a = a)

  return(pp)


}

wb_pp <- function(x) {

  r <- rank(-x,
            na.last = NA,
            ties.method = "first")
  n <- length(x)

  pp <- r/(n+1)

  return(pp)

}
