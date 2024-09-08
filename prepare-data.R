library(tidyverse)
library(tidyfinance)
library(duckdb)
library(httr2)
library(DBI)

# Initialize database -------------------------------------------------------------------------

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/stock-analyzer.duckdb")

# Define parameters ---------------------------------------------------------------------------

start_date <- as.Date("2004-07-31")
end_date <-  as.Date("2024-08-31")

# Download stock prices -----------------------------------------------------------------------

symbols <- download_data("constituents", index = "S&P 500")

stock_prices <- download_data(
  "stock_prices", 
  start_date = start_date, 
  end_date = end_date, symbols = symbols$symbol
)

stock_data <- stock_prices |>
  group_by(symbol) |> 
  arrange(date) |> 
  fill(adjusted_close, .direction = "down") |>
  mutate(ret = adjusted_close / lag(adjusted_close) - 1) |>
  ungroup() |> 
  drop_na(ret) |> 
  arrange(symbol, date)

# Download market prices
market_prices <- download_data(
  "stock_prices", 
  start_date = start_date, 
  end_date = end_date,
  symbols = "^GSPC"
)

market_data <- market_prices |> 
  group_by(symbol) |> 
  arrange(date) |> 
  fill(adjusted_close, .direction = "down") |> 
  mutate(ret = adjusted_close / lag(adjusted_close) - 1) |>
  ungroup() |> 
  drop_na(ret) |> 
  select(date, ret_market = ret)

# Download risk-free rates
risk_free_raw <- download_data(
  "fred", 
  start_date = start_date, 
  end_date = end_date,
  series = "DGS1MO"
)

risk_free_data <- risk_free_raw |> 
  mutate(value = value / 100,
         risk_free = (1 + value)^(1/252) - 1) |> 
  select(date, risk_free)

# Calculate date range
dates <- stock_data |> 
  inner_join(risk_free_data, join_by(date)) |> 
  inner_join(market_data, join_by(date)) |> 
  summarize(start_date = min(date),
            end_date = max(date))

# Store data
if (!dir.exists("data")) {
  dir.create("data")
}

stocks <- stock_data |> 
  distinct(symbol)

dbWriteTable(con, "dates", dates, overwrite = TRUE)
dbWriteTable(con, "stock_data", stock_data, overwrite = TRUE)
dbWriteTable(con, "risk_free_data", risk_free_data, overwrite = TRUE)
dbWriteTable(con, "market_data", market_data, overwrite = TRUE)
dbWriteTable(con, "stocks", stocks, overwrite = TRUE)

# Estimate alphas and betas -------------------------------------------

estimate_capm <- function(data) {
  fit <- lm("ret_excess ~ ret_market", data = data)
  broom::tidy(fit)
}

# Combine data
capm_data <- stock_data |> 
  filter(date >= dates$start_date) |> 
  left_join(risk_free_data, join_by(date)) |> 
  left_join(market_data, join_by(date)) |> 
  mutate(ret_excess = ret - risk_free,
         ret_market_excess = ret_market - risk_free) |> 
  select(symbol, ret_excess, ret_market) |> 
  group_by(symbol) |> 
  nest(data = c(ret_excess, ret_market)) |> 
  mutate(capm = map(data, estimate_capm)) |> 
  unnest(capm) |> 
  select(symbol, term, estimate, p_value = p.value) |> 
  mutate(term = if_else(term == "(Intercept)", "alpha", "beta"),
         estimate = if_else(term == "alpha", (1 + estimate)^252 - 1, estimate)) |> 
  ungroup()

if (!dir.exists("data")) {
  dir.create("data")
}

dbWriteTable(con, "capm_data", capm_data, overwrite = TRUE)

# Download logos ------------------------------------------------------

base_url <- "https://companiesmarketcap.com/img/company-logos/256/"

symbols <- symbols |> 
  mutate(symbol_alt = case_when(symbol == "BRKB" ~ "BRK-B", 
                                symbol == "GOOGL" ~ "GOOG",
                                symbol == "CPAY" ~ "FLT",
                                symbol == "NWSA" ~ "NWS",
                                symbol == "FOXA" ~ "FOX",
                                symbol == "BFB" ~ "BF-A",
                                symbol == "BF-B" ~ "BF-A",
                                TRUE ~ symbol)) |> 
  filter(!symbol %in% c("GEV", "VLTO", "SOLV", "SW"))

if (!dir.exists("data/logos")) {
  dir.create("data/logos")
}

for (j in 1:nrow(symbols)) {
  
  image_url <- paste0(base_url, symbols$symbol_alt[j], ".webp")
  destfile <- paste0("data/logos/", symbols$symbol[j], ".webp")
  
  if (!file.exists(destfile)) {
    request <- request(image_url)
    
    request|>
      req_perform() |>
      resp_body_raw() |>
      writeBin(destfile)
  }
}
