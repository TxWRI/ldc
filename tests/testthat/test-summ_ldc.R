test_that("summ_ldc outputs and errors", {

  ## test that returns a tibble with 6 columns
  install_unit("cfu")
  df <- as_tibble(tres_palacios) %>%
          filter(!is.na(Indicator_Bacteria)) %>%
          mutate(Flow = set_units(Flow, "ft^3/s")) %>%
          mutate(Indicator_Bacteria = set_units(Indicator_Bacteria, "cfu/100mL"))
  allowable_concentration <- 126
  units(allowable_concentration) <- "cfu/100mL"
  df_ldc <- calc_ldc(df,
                     Q = Flow,
                     C = Indicator_Bacteria,
                     allowable_concentration = allowable_concentration)
  df_sum <- summ_ldc(df_ldc,
                     Q = Flow,
                     C = Indicator_Bacteria,
                     Exceedance = P_Exceedance,
                     groups = Flow_Category,
                     method = "geomean")
  testthat::expect_s3_class(df_sum, "tbl_df")
  testthat::expect_equal(length(df_sum), 6)

  df_sum <- summ_ldc(df_ldc,
                     Q = Flow,
                     C = Indicator_Bacteria,
                     Exceedance = P_Exceedance,
                     groups = Flow_Category,
                     method = "mean")
  testthat::expect_s3_class(df_sum, "tbl_df")
  testthat::expect_equal(length(df_sum), 6)

  df_sum <- summ_ldc(df_ldc,
                     Q = Flow,
                     C = Indicator_Bacteria,
                     Exceedance = P_Exceedance,
                     groups = Flow_Category,
                     method = "median")
  testthat::expect_s3_class(df_sum, "tbl_df")
  testthat::expect_equal(length(df_sum), 6)

  ## return error if method is not correct
  testthat::expect_error(summ_ldc(df_ldc,
                                 Q = Flow,
                                 C = Indicator_Bacteria,
                                 Exceedance = P_Exceedance,
                                 groups = Flow_Category,
                                 method = "max"))
})
