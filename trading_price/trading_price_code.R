#' Calling Packeages
library(xml2)
library(httr)
library(rvest)
library(jsonlite)
library(purrr)
library(tibble)
library(dplyr)
library(magrittr)
#' Create Pool
pool_ticker <- c("NT2", "OGC", "MWG", "DHC")

#' Get trading price of ACB
trading_price <- httr::GET(url = "api.ptcp.vn/stocks/eod?symbol=ACB&limit=-1") %>%
  httr::content(as = "text") %>%
  jsonlite::fromJSON(.) %>%
  purrr::pluck("data") %>%
  tibble::as_tibble()
#' Subset Trading price
subset_trading_price <- trading_price %>%
  dplyr::select(date, open, high, low, close, volume) %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::mutate(dplyr::across(.cols = dplyr::matches(c("open", "high", "low", "close", "volume")), ~ as.numeric(.))) %>%
  dplyr::mutate(
    max_price = max(high, na.rm = TRUE),
    min_price = min(low, na.rm = TRUE),
    median_price = median(close, na.rm = TRUE),
    average_price = mean(close, na.rm = TRUE)
  ) %>%
  dplyr::mutate(is_break_out = dplyr::case_when(
    close > median_price ~ TRUE,
    TRUE ~ FALSE
  )) %>%
  dplyr::mutate(return = round(close / dplyr::lead(close) - 1, 3))
for (tic in pool_ticker) {
  url <- glue::glue("api.ptcp.vn/stocks/eod?symbol={tic}&limit=-1")
  .trading_price <- httr::GET(url = url) %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON(.) %>%
    purrr::pluck("data") %>%
    tibble::as_tibble()
  #' Bind
  trading_price <- dplyr::bind_rows(trading_price, .trading_price, .id = NULL)
  #' Subset Trading price
  .subset_trading_price <- .trading_price %>%
    dplyr::select(date, open, high, low, close, volume) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(dplyr::across(.cols = dplyr::matches(c("open", "high", "low", "close", "volume")), ~ as.numeric(.))) %>%
    dplyr::mutate(
      max_price = max(high, na.rm = TRUE),
      min_price = min(low, na.rm = TRUE),
      median_price = median(close, na.rm = TRUE),
      average_price = mean(close, na.rm = TRUE)
    ) %>%
    dplyr::mutate(is_break_out = dplyr::case_when(
      close > median_price ~ TRUE,
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(return = round(close / dplyr::lead(close) - 1, 3))
  #' Bind subset trading price
  subset_trading_price <- dplyr::bind_rows(subset_trading_price, .subset_trading_price, .id = NULL)
}
#' Output
openxlsx::write.xlsx(x = subset_trading_price, file = here::here("trading_price", "trading_price.xlsx"))