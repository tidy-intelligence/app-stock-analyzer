library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggplot2)

dates <- readRDS("data/dates.rds")
stock_data <- readRDS("data/stock_data.rds")
capm_data <- readRDS("data/capm_data.rds")
stocks <- sort(unique(stock_data$symbol))

# Helper functions ----------------------------------------------------

source("R/helpers.R")

input <- list("selected_symbols" = c("AAPL", "MSFT", "NVDA", "TSLA"), "multiple"=3)

capm_data_prepared <- capm_data |> 
  prepare_capm_data(input)

stock_data_prepared <- stock_data |> 
  prepare_stock_data(input) |> 
  left_join(capm_data_prepared, join_by(symbol))

portfolio_weights <- stock_data |> 
  calculate_portfolio_weights(input) 

stock_data_prepared |>
  create_table_summary(dates)

portfolio_weights |>
  create_table_weights(input)

draw_efficient_frontier(stock_data, input, portfolio_weights)
