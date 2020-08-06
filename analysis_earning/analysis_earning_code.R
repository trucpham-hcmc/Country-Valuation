# Add library 
library(xml2)
library(httr)
library(rvest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(magrittr)
library(tidyselect)
library(lubridate)
library(readxl)

raw <- readxl::read_xls(path = here::here("data","earning_1h2020_data.xls"),
                        sheet = "DATA", range = NULL, col_names = TRUE,
                        col_types = NULL, na = c("", "na"), trim_ws = TRUE, skip = 0,
                        n_max = Inf, guess_max = 1000,
                        progress = readxl_progress(), .name_repair = "unique") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(dplyr::across(c(tidyselect::starts_with("earning"),
                                tidyselect::starts_with("eps"),
                                tidyselect::starts_with("pe_ratio")), 
                              ~ as.numeric(.))) %>%
  dplyr::mutate(dplyr::across(c(tidyselect::starts_with("ticker"),
                                tidyselect::starts_with("primary_exchange_code"),
                                tidyselect::starts_with("fundamental"),
                                tidyselect::starts_with("unit"),
                                tidyselect::starts_with("currency"),
                                tidyselect::starts_with("updated_date")),
                              ~ as.character(.))) %>%
  dplyr::mutate(updated_date = lubridate::dmy(updated_date))
#' earning_primary_exchange

earning_primary_exchange_code <- raw %>%
  dplyr::group_by(primary_exchange_code) %>%
  dplyr::summarise(number_constituent = n(),
                   earning_value = sum(earning_value),na.rm = TRUE) %>%
  dplyr::ungroup()

earning_classification <- raw %>%
  dplyr::mutate(earning_classification_code = dplyr::case_when(earning_value >0 & earning_yoy > 0 ~ "Exellence",
                                                          earning_value >0 & earning_yoy < 0 ~ "Moderate",
                                                          TRUE ~ "Bad")) %>%
  dplyr::group_by(primary_exchange_code, earning_classification_code) %>%
  dplyr::arrange(dplyr::desc(earning_value)) %>%
  dplyr::summarise(number_constituent = n(), 
                   earning_value = sum(earning_value, na.rm = TRUE),
                   top_10_company = paste(purrr::map(1:10,~ dplyr::nth(ticker,n = .)), collapse = ",")) %>%
  dplyr::ungroup()
  
