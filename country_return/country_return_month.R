#' Calling Packeages
library(xml2)
library(httr)
library(rvest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(magrittr)
library(lubridate)
library(group_by)

raw <- openxlsx::read.xlsx(xlsxFile = here::here("data","country_return_data.xlsx"),
                    sheet = "DATA",
                    startRow = 1,
                    colNames = TRUE,
                    rowNames = FALSE,
                    detectDates = FALSE,
                    skipEmptyRows = TRUE,
                    skipEmptyCols = TRUE,
                    rows = NULL,
                    cols = NULL,
                    check.names = FALSE,
                    sep.names = ".",
                    namedRegion = NULL,
                    na.strings = c("na"," "),
                    fillMergedCells = FALSE) %>%
  tibble::tibble() %>%
  dplyr::select(index, date, return) %>%
  dplyr::mutate(date = as.Date(date, origin = "1899/12/30"))

index_return <- raw %>%
  dplyr::mutate(month = lubridate::month(date),
                year = lubridate::year(date),
                is_positive = dplyr::case_when(return > 0 ~ TRUE,
                                               TRUE ~ FALSE))
index_count_by_month <- index_return %>%
  dplyr::group_by(index, month, is_positive) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup()

index_return_by_month <- index_return %>%
  dplyr::group_by(index,month) %>%
  dplyr::summarise(return = mean (return, na.rm= TRUE)) %>%
  dplyr::left_join(dplyr::rename(dplyr::filter(index_count_by_month, is_positive == TRUE),count_positive = count),
                   by = c("index"="index",
                          "month"="month")) %>%
  dplyr::left_join(dplyr::rename(dplyr::filter(index_count_by_month, is_positive == FALSE),count_negative = count),by = c("index"="index","month"="month")) %>%
  dplyr::select(-dplyr::starts_with("is_postive")) %>%
  dplyr::mutate(
    count_total = count_positive + count_negative,
    prob_positive = round(count_positive/ count_total,3),
    prob_negative = round(count_negative/ count_total,3)
  )

openxlsx::write.xlsx()
