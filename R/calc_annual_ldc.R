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
#' @param estimator numeric, one of \code{c(5,6,7,8,9)}. \code{6} is the default
#'   method correponding to the Weibull plotting position. Further details are
#'   provided in \code{stats::quantile()}.
#' @param n numeric, the length of generated probability points. Larger n may
#'   result in a slightly smoother curve at a cost of increased processing time.
#'   The probability points are used to generate the continuous sample quantiles
#'   types 5 to 9 (see \code{quantile}).
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
calc_annual_ldc <- function(.tbl,
                            Q = NULL,
                            C = NULL,
                            Date = NULL,
                            allowable_concentration = NULL,
                            breaks = c(1, 0.8, 0.4, 0),
                            labels = c("High Flows", "Medium Flows", "Low Flows"),
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

  ## if length is zero, then it is not a concentration
  if(length(C_den) == 0) {
    stop(paste0(as_name(enquo(C)),
                " does not have valid units, it is missing a denominator"))
  }

  ## Discharge units
  Q_units <- units(.tbl[[substitute(Q)]])
  ## daily volume units based on concentration
  fv_units <- as_units(paste0(C_den, "/day"))

  ## create the flow duration curve
  fdc.tbl <- .tbl %>%
    as_tibble(.tbl) %>%
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
                               conf.level = 0.90)
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
#' @param type corresponds to types 5 through 9 in \code{quantile} or \code{'hd'} for the Harrel-Davis Distribution-Free Quantile estimator.
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
