library(dataRetrieval)
library(readr)
library(dplyr)
library(janitor)

## download mean daily flow from USGS
Q_df <- readNWISdv(siteNumbers = "08162600",
                   startDate = "2000-01-01",
                   endDate = "2020-12-31",
                   parameterCd = "00060",
                   statCd = "00003")
Q_df <- renameNWISColumns(Q_df)

df_12517 <- read_delim("data-raw/SWQM-12517-P31699.txt",
                       "|", escape_double = FALSE, col_types = cols(Segment = col_character(),
                                                                    `Station ID` = col_character(), `Parameter Code` = col_character(),
                                                                    `End Date` = col_date(format = "%m/%d/%Y"),
                                                                    `End Time` = col_skip(), `End Depth` = col_skip(),
                                                                    `Start Date` = col_skip(), `Start Time` = col_skip(),
                                                                    `Start Depth` = col_skip(), `Composite Category` = col_skip(),
                                                                    `Composite Type` = col_skip(), `Submitting Entity` = col_skip(),
                                                                    `Collecting Entity` = col_skip(),
                                                                    `Monitoring Type` = col_skip(), Comments = col_skip()),
                       trim_ws = TRUE)
df_12517 <- clean_names(df_12517)

Q_df %>%
  left_join(df_12517, by = c("Date" = "end_date")) %>%
  select(site_no, Date, Flow, Indicator_Bacteria = value) -> tres_palacios

usethis::use_data(tres_palacios, overwrite = TRUE)
