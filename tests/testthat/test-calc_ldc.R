test_that("calc_ldc outputs and errors", {

  library(dplyr)
  library(units)

  install_unit("cfu")
  df <- tres_palacios %>%
    filter(!is.na(Indicator_Bacteria)) %>%
    mutate(Flow = set_units(Flow, "ft^3/s")) %>%
    mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu/100mL"))
  allowable_concentration <- 126
  units(allowable_concentration) <- "cfu/100mL"
  df_ldc <- calc_ldc(df,
                     Q = Flow,
                     C = Indicator_Bacteria,
                     allowable_concentration = allowable_concentration)

  ## test that returns a tibble with 9 columns
  testthat::expect_s3_class(df_ldc, "tbl_df")
  testthat::expect_equal(length(df_ldc), 9)

  ## test that returns an error if arguments are not correct type
  testthat::expect_error(calc_ldc(df,
                                  Q = NULL,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))
  testthat::expect_error(calc_ldc(df,
                                  Q = Flow,
                                  C = NULL,
                                  allowable_concentration = allowable_concentration))
  testthat::expect_error(calc_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = NULL))
  ## test that error is returned if estiamtor value isn't valid
  testthat::expect_error(calc_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration,
                                  estimator = 1))

  ## if units is not a concentration, return an error
  allowable_concentration <- 126
  units(allowable_concentration) <- "cfu"
  df <- tres_palacios %>%
    filter(!is.na(Indicator_Bacteria)) %>%
    mutate(Flow = set_units(Flow, "ft^3/s")) %>%
    mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu"))
  testthat::expect_error(calc_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))

  df <- tres_palacios %>%
    filter(!is.na(Indicator_Bacteria)) %>%
    mutate(Flow = set_units(Flow, "ft^3/s")) %>%
    mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu/100mL"))
  testthat::expect_error(calc_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))


  ## if units for Q are not specified, return an error
  allowable_concentration <- 126
  units(allowable_concentration) <- "cfu/100mL"

  df <- tres_palacios %>%
    filter(!is.na(Indicator_Bacteria))
  testthat::expect_error(calc_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))


  ## if units for C are not specified, return an error
  df <- tres_palacios %>%
    filter(!is.na(Indicator_Bacteria)) %>%
    mutate(Flow = set_units(Flow, "ft^3/s"))
  testthat::expect_error(calc_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))

  remove_unit("cfu")
})


test_that("p_estimator returns vector", {

  x <- ldc:::p_estimator(1:100)
  testthat::expect_equal(length(x), 100)
  testthat::expect_type(x, "double")
})
