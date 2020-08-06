library(xml2)
library(httr)
library(rvest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(magrittr)
library(here)
library(lubridate)

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
                           na.strings = "NA",
                           fillMergedCells = FALSE)
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
                           na.strings = "NA",
                           fillMergedCells = FALSE)
dplyr::as_tibble(raw)
tidy_raw <- dplyr::mutate(raw,)
