test_that("draw_ldc creates expected ggplot objects", {
  skip_on_cran()
  library(dplyr)
  library(units)
  library(ggplot2)
  # Format data
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
  p <- draw_ldc(df_ldc,
           df_sum,
           y_lab = expression(paste(italic("E. coli"))),
           label_nudge_y = log10(1000)) +
           scale_y_log10() +
           theme(legend.title = element_blank(),
                 legend.direction = "vertical",
                 legend.position = "bottom")

  ## cleanup
  remove_unit("cfu")

  ## visual test
  vdiffr::expect_doppelganger("basic ldc", p)
})
