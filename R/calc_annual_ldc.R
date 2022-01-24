#' Calculate annualized load duration curve
#'
#' Calculates the median annual ldc with confidence intervals.
#'
#' @param .tbl data frame with at least three columns Q (discharge or flow), C
#'   (associated pollutant concentration), and Date.
#' @param Q variable name in .tbl for discharge or flow. This must be of class
#'   `units`, typically with a units value of "ft^3/s".
#' @param C variable name in .tbl for associated pollutant concentration at a
#'   given flow value. This must be of class `units`, typically with a units
#'   value of "mg/L" or "cfu/100mL".
#' @param Date variable name in .tbl for the event Date. This variable must be
#'   of class `Date`.
#' @param allowable_concentration an object of class \code{units} specifying the
#'   allowable pollutant concentration.
#' @param breaks a numeric vector of break points for flow categories. Must be
#'   of length of labels + 1. defaults to \code{c(1, 0.8, 0.4, 0)}.
#' @param labels labels for the categories specified by breaks.
#' @param conf_level numeric, confidence level (default is 0.9) of the median
#'   interval at given exceedance probability.
#' @param estimator one of \code{c(5,6,7,8,9,"hd")}. \code{6} is the default
#'   method correponding to the Weibull plotting position. Further details are
#'   provided in \code{\link[stats]{quantile}}. \code{"hd"} uses the Harrell-Davis
#'   Distribution-Free Quantile Estimator (see:
#'   \code{\link[Hmisc]{hdquantile}}).
#' @param n numeric, the length of generated probability points. Larger n may
#'   result in a slightly smoother curve at a cost of increased processing time.
#'   The probability points are used to generate the continuous sample quantiles
#'   types 5 to 9 (see \code{\link[stats]{quantile}}).
#'
#' @details The median annual ldc is calculated by computing the flow duration
#'   curve for each individual year in the dataset. Exceedance probabilities are
#'   calculated from the descending order of Daily Flows. By default, the
#'   Weibull plotting position is used: \deqn{p = P(Q > q_i) =  \frac{i}{n+1}}
#'   where \eqn{q_i, i = 1, 2, ... n}, is the i-th sorted streamflow value.
#'
#'   The median streamflow +/- chosen confidence interval is calculated at each
#'   exceedance probability. The load duration curve is calculated by
#'   multiplying the median streamflow by the allowable concentration and
#'   appropriate conversions.
#' @references Vogel, Richard M., and Neil M. Fennessey. "Flow-duration curves.
#'   I: New interpretation and confidence intervals." Journal of Water Resources
#'   Planning and Management 120, no. 4 (1994): 485-504. \doi{10.1061/(ASCE)0733-9496(1994)120:4(485)}
#'
#' @return list of two tibbles (Q and C). Includes variables in .tbl and
#'   Daily_Flow_Volume (discharge volume), Daily_Load (pollutant sample volume),
#'   P_Exceedance (exeedance probability), Flow_Category (as defined by breaks
#'   and labels).
#' @export
#' @import rlang dplyr units
#' @importFrom purrr map_dfr map
#' @importFrom DescTools MedianCI
#'
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
#' df_ldc <- calc_annual_ldc(df,
#'                    Q = Flow,
#'                    C = Indicator_Bacteria,
#'                    allowable_concentration = allowable_concentration,
#'                    estimator = 5,
#'                    n = 1000)
#' df_ldc$Q
#'
#' ## cleanup
#' remove_unit("cfu")
calc_annual_ldc <- function(.tbl,
                            Q = NULL,
                            C = NULL,
                            Date = NULL,
                            allowable_concentration = NULL,
                            breaks = c(1, 0.8, 0.4, 0),
                            labels = c("High Flows", "Medium Flows", "Low Flows"),
                            conf_level = 0.90,
                            estimator = 6,
                            n = 500) {



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
    if(estimator != "hd") {
      stop("'estimator' must be one of 'c(5,6,7,8,9)' or 'hd'")
    }
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
  allowable_concentration_den <- units(allowable_concentration)$denominator

  ## if length is zero, then it is not a concentration
  if(length(C_den) == 0) {
    stop(paste0(as_name(enquo(C)),
                " does not have valid units, it is missing a denominator"))
  }
  if(length(allowable_concentration_den) == 0) {
    stop("'allowable_concentration' does not have valid units, it is missing a denominator")
  }

  ## Discharge units
  Q_units <- units(.tbl[[substitute(Q)]])
  ## daily volume units based on concentration
  fv_units <- as_units(paste0(C_den, "/day"))

  ## create the flow duration curve
  fdc.tbl <- .tbl %>%
    as_tibble() %>%
    arrange(!! enquo(Q)) %>%
    mutate(
      Year = as.numeric(format(!! enquo(Date), "%Y"))) %>%
    group_by(.data$Year) %>%
    named_group_split(.data$Year) %>%
    purrr::map(~annual_pp_estimator(.x$Flow,
               n = n,
               type = estimator)) %>%
    purrr::map_dfr( ~ tibble(Q = .x$Q,
                             pp = .x$pp),
                    .id = "Year") %>%
    mutate(Year = as.numeric(.data$Year)) %>%
    named_group_split(.data$pp) %>%
    purrr::map(~{
      x <- DescTools::MedianCI(.x$Q,
                               conf.level = conf_level)
      tibble(median_Q = x["median"],
             lwr.ci_Q = x["lwr.ci"],
             upr.ci_Q = x["upr.ci"])
    })  %>%
    purrr::map_dfr( ~ as_tibble(.x),
                    .id = "pp") %>%
    mutate(pp = as.numeric(.data$pp),
           median_Q = set_units(.data$median_Q, Q_units, mode = "standard"),
           lwr.ci_Q = set_units(.data$lwr.ci_Q, Q_units, mode = "standard"),
           upr.ci_Q = set_units(.data$upr.ci_Q, Q_units, mode = "standard")) %>%
    dplyr::rename(P_Exceedance = .data$pp)


  ## create the ldc
  q.ldc.tbl <- fdc.tbl %>%
    mutate(median_Daily_Flow_Volume = set_units(.data$median_Q, fv_units, mode = "standard"),
           lwr.ci_Daily_Flow_Volume = set_units(.data$lwr.ci_Q, fv_units, mode = "standard"),
           upr.ci_Daily_Flow_Volume = set_units(.data$upr.ci_Q, fv_units, mode = "standard"),
           median_Allowable_Daily_Load = .data$median_Daily_Flow_Volume * allowable_concentration,
           lwr.ci_Allowable_Daily_Load = .data$lwr.ci_Daily_Flow_Volume * allowable_concentration,
           upr.ci_Allowable_Daily_Load = .data$upr.ci_Daily_Flow_Volume * allowable_concentration,
           ## bin the flow exceedance values
           Flow_Category = cut(.data$P_Exceedance, breaks = breaks, labels = labels,
                               right = FALSE))


  ## calculate exceedance percentiles for flow associated with pollutant samples
  median_fn <- ecdf(q.ldc.tbl[["median_Q"]])

  c.ldc.tbl <- .tbl %>%
    dplyr::filter(!is.na(!! enquo(C))) %>%
    mutate(P_Exceedance = 1 - median_fn(!! enquo(Q))) %>%
    ## calculate volume
    mutate(Daily_Flow_Volume = set_units(.data$Flow, fv_units, mode = "standard"),
           ## daily measured load
           Daily_Load = .data$Daily_Flow_Volume * !! enquo(C),
           Flow_Category = cut(.data$P_Exceedance, breaks = breaks, labels = labels,
                               right = FALSE))

  ## C measurements at extremely high or low flows might be outside of the prob range for annual median flows
  ## not sure how to handle these points yet.
  ## I think we can calculate the quantiles for the flow associated with the sample in a given year.
  ## this wouldn't be as efficient but might be more logical.

  return(list(Q = q.ldc.tbl,C = c.ldc.tbl))


}


#' Generates sequence of exceedance probabilities
#'
#' @param x numeric vector
#' @param n output length
#' @param type corresponds to types 5 through 9 in \code{quantile} or \code{'hd'} for the Harrell-Davis Distribution-Free Quantile estimator.
#'
#' @return tibble with variables Q and pp
#' @keywords internal
#' @noRd
#' @importFrom stats quantile ppoints ecdf
#' @importFrom Hmisc hdquantile
#' @import dplyr rlang
annual_pp_estimator <- function(x, n, type = 6) {
  if (is.numeric(type)) {
    if (type == 5) a = 0.5
    if (type == 6) a = 0 # weibull default
    if (type == 7) a = 1
    if (type == 8) a = 1/3
    if (type == 9) a = 3/8

    Fn <- ecdf(x)
    probs <- ppoints(n = n, a = a)
    p <- quantile(Fn, probs, type = type)
    pp <- names(p)
    pp <- gsub("%", "", pp)
    return(tibble(Q = p,
                  pp = 1-(as.numeric(pp)/100)))
  }

  if (type == "hd") {
    probs <- ppoints(n)
    p <- hdquantile(x, probs, se = FALSE, na.rm = FALSE, names = TRUE, weights = FALSE)
    pp <- names(p)
    return(tibble(Q = p,
                  pp = 1-as.numeric(pp)))

  }
}


named_group_split <- function(.tbl, ...) {
  grouped <- group_by(.tbl, ...)
  names <- rlang::eval_bare(rlang::expr(paste(!!!group_keys(grouped), sep = " / ")))

  grouped %>%
    group_split() %>%
    rlang::set_names(names)
}
