# Control
control = list(
  descriptions = "Vietnam exchange analysis",
  date = "2020/08/06",
  unit = "billion",
  currency = "dong",
  threshold_extreme = 2
)
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
library(rslang)

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
  dplyr::mutate(updated_date = lubridate::dmy(updated_date)) %>%
  dplyr::mutate(pe_ratio = dplyr::case_when(pe_ratio > 0 ~ pe_ratio,
                                                     TRUE ~ na_dbl)) %>%
  dplyr::mutate(earning_classification_code = dplyr::case_when(earning_value >0 & earning_yoy > 100 ~ "Excellence",
                                                               earning_value >0 & earning_yoy < 50~ "Moderate",
                                                               TRUE ~ "Bad"))
#' earning_primary_exchange

earning_primary_exchange_code <- raw %>%
  dplyr::group_by(primary_exchange_code) %>%
  dplyr::summarise(number_constituent = n(),
                   earning_value = sum(earning_value),na.rm = TRUE) %>%
  dplyr::ungroup()

gics_sector <- openxlsx::read.xlsx(xlsxFile = here::here("data","vietnam_bloomberg_gics.xlsx"),
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
                                   na.strings = "NA",
                                   fillMergedCells = FALSE)
master_raw <- raw %>%
  dplyr::left_join(gics_sector, by = "ticker")

gics_sector_analysis <- master_raw %>%
  dplyr::group_by(primary_exchange_code,gics_sector_name) %>%
  dplyr::summarise(number_of_companies = n(),
                   earning_gics_sector = sum(earning_value,na.rm = TRUE)) %>%
  dplyr::ungroup()
gics_sector_order <- master_raw %>%
  dplyr::group_by(primary_exchange_code,gics_sector_name) %>%
  dplyr::arrange(desc(earning_value)) %>%
  dplyr::summarise(number_constituent = n(), 
                  earning_value = sum(earning_value, na.rm = TRUE),
                  top_10_company = paste(purrr::keep(purrr::map(1:10,~ dplyr::nth(ticker,n = .)), ~!is.na(.)),collapse = ",")) %>%
  dplyr::ungroup()

earning_classification_by_sector <- master_raw %>%
  dplyr::group_by(primary_exchange_code,gics_sector_name,earning_classification_code) %>%
  dplyr::arrange(desc(earning_value)) %>%
  dplyr::summarise(number_constituent = n(), 
                   earning_value = sum(earning_value, na.rm = TRUE),
                   current_pe_ratio = median(pe_ratio, na.rm = TRUE)) %>%
  dplyr::ungroup()

detect_extreme <- master_raw %>%
  dplyr::group_by(gics_sector_name)%>%
  dplyr::summarise(pe_industry_cap = median(pe_ratio, na.rm = TRUE) + control$threshold_extreme* sd(pe_ratio,na.rm = TRUE),
                   pe_industry_floor = median(pe_ratio, na.rm = TRUE) - control$threshold_extreme* sd(pe_ratio, na.rm = TRUE),
                   pe_industry_average = mean(pe_ratio, na.rm = TRUE),
                   pe_industry_median = median(pe_ratio, na.rm = TRUE), 
                   pe_industry_range = paste(min(pe_ratio, na.rm = TRUE),
                                             max(pe_ratio, na.rm = TRUE))) %>%
  dplyr::ungroup()

detect_normal <- master_raw %>%
  dplyr::left_join(detect_extreme, by = "gics_sector_name") %>%
  dplyr::mutate(is_extreme = dplyr::case_when(pe_ratio > pe_industry_cap | pe_ratio < pe_industry_floor ~ "Extreme",
                                              TRUE ~ "Normal")) %>%
  dplyr::filter(is_extreme == "Normal")

gics_sector_valuation <- master_raw %>%
  dplyr::group_by(gics_sector_name) %>%
  dplyr::summarise(pe_industry_average = mean(pe_ratio, na.rm = TRUE),
                   pe_industry_median = median(pe_ratio, na.rm = TRUE),
                   pe_industry_range = paste(min(pe_ratio, na.rm = TRUE),
                                             max(pe_ratio, na.rm = TRUE), sep = "-"))
  dplyr::ungroup()

pe_gics_classification <- master_raw %>%
  dplyr::left_join (gics_sector_valuation, by = "gics_sector_name") %>%
  dplyr::mutate(pe_to_sector_percentage = (pe_ratio/pe_industry_median)-1) %>%
  dplyr::mutate(pe_gics_classification = dplyr::case_when(pe_to_sector_percentage > 0 ~ "Premium",
                                                          TRUE ~ "Discount"))
  

    
                
  

  
  
  



  
                                 
  
