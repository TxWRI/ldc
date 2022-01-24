test_that("calc_annual returns expected output", {

  df <- as_tibble(tres_palacios)
  install_unit("cfu")
  df <- df %>%
    mutate(Flow = set_units(Flow, "ft^3/s"),
           Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu/100mL"))
  allowable_concentration <- 126
  units(allowable_concentration) <- "cfu/100mL"


  list(5, 6, 7, 8, 9, "hd") %>%
    purrr::map(~{
      x <- calc_annual_ldc(df,
                            Q = Flow,
                            C = Indicator_Bacteria,
                            Date = Date,
                            allowable_concentration = allowable_concentration,
                            estimator = .x)
      ## returns list of two tibbles
      testthat::expect_equal(length(x), 2)
      ## returns Q tibble with 11 columns
      testthat::expect_equal(length(x$Q), 11)
      ## returns C tibble with 8 columns
      testthat::expect_equal(length(x$C), 8)
    })


  ## test that returns an error if arguments are not correct type
  testthat::expect_error(calc_annual_ldc(df,
                                  Q = NULL,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))
  testthat::expect_error(calc_annual_ldc(df,
                                  Q = Flow,
                                  C = NULL,
                                  allowable_concentration = allowable_concentration))
  testthat::expect_error(calc_annual_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = NULL))
  testthat::expect_error(calc_annual_ldc(df,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration,
                                  estimator = 1))

  ## if units is not a concentration, return an error
  allowable_concentration <- 126
  units(allowable_concentration) <- "cfu"
  x <- tres_palacios %>%
    filter(!is.na(Indicator_Bacteria)) %>%
    mutate(Flow = set_units(Flow, "ft^3/s")) %>%
    mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu"))
  testthat::expect_error(calc_annual_ldc(x,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))

  x <- tres_palacios %>%
    filter(!is.na(Indicator_Bacteria)) %>%
    mutate(Flow = set_units(Flow, "ft^3/s")) %>%
    mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu/100mL"))
  testthat::expect_error(calc_annual_ldc(x,
                                  Q = Flow,
                                  C = Indicator_Bacteria,
                                  allowable_concentration = allowable_concentration))



  remove_unit("cfu")

})
