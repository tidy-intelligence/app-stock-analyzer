library(tidyverse)
library(tidyquant)
library(httr2)

# Define parameters
start_date <- as.Date("2000-01-01")
end_date <- Sys.Date()-1

# Download stock prices
symbols <- tq_index("SP500") |> 
  filter(company != "US DOLLAR")

stock_prices <- tq_get(
  symbols, get = "stock.prices", from = start_date, to = end_date
)

stock_data <- stock_prices |>
  group_by(symbol) |> 
  arrange(date) |> 
  fill(adjusted, .direction = "down") |> 
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  ungroup() |> 
  drop_na(ret) 

# Download market prices
market_prices <- tq_get(
  "^GSPC", get = "stock.prices", from = start_date, to = end_date
)

market_data <- market_prices |> 
  group_by(symbol) |> 
  arrange(date) |> 
  fill(adjusted, .direction = "down") |> 
  mutate(ret = adjusted / lag(adjusted) - 1) |>
  ungroup() |> 
  drop_na(ret) |> 
  select(date, ret_market = ret)

# Download risk-free rates
risk_free_raw <- tq_get(
  "DGS1MO", get = "economic.data", from = start_date, to = end_date
)

risk_free_data <- risk_free_raw |> 
  mutate(price = price / 100,
         risk_free = (1 + price)^(1/252) - 1) |> 
  select(date, risk_free)

# Combine data
stock_data <- stock_data |> 
  left_join(risk_free_data, join_by(date)) |> 
  left_join(market_data, join_by(date)) |> 
  mutate(ret_excess = ret - risk_free,
         ret_market_excess = ret_market - risk_free)


# Store data
write_rds(stock_data, "data/stock_data.rds")

# Estimate alphas and betas -------------------------------------------
estimate_capm <- function(data) {
  fit <- lm("ret_excess ~ ret_market", data = data)
  broom::tidy(fit)
}

capm_data <- stock_data |> 
  select(symbol, ret_excess, ret_market) |> 
  group_by(symbol) |> 
  nest(data = c(ret_excess, ret_market)) |> 
  mutate(capm = map(data, estimate_capm)) |> 
  unnest(capm) |> 
  select(symbol, term, estimate, p_value = p.value) |> 
  mutate(term = if_else(term == "(Intercept)", "alpha", "beta"),
         estimate = if_else(term == "alpha", (1 + estimate)^252 - 1, estimate)) |> 
  ungroup()
  
write_rds(capm_data, "data/capm_data.rds")

# Download logos ------------------------------------------------------
symbols <- symbols |> 
  mutate(symbol_alt = case_when(symbol == "GOOGL" ~ "GOOG",
                                symbol == "CPAY" ~ "FLT",
                                symbol == "NWSA" ~ "NWS",
                                symbol == "FOXA" ~ "FOX",
                                symbol == "BF-B" ~ "BF-A",
                                TRUE ~ symbol)) |> 
  filter(!symbol %in% c("GEV", "VLTO", "SOLV"))

# TODO: add logic to skip already downloaded logos

base_url <- "https://companiesmarketcap.com/img/company-logos/64/"

for (j in 477:nrow(symbols)) {
  
  image_url <- paste0(base_url, symbols$symbol_alt[j], ".webp")
  destfile <- paste0("data/logos/", symbols$symbol[j], ".webp")
  
  request <- request(image_url)

  request|>
    req_perform() |>
    resp_body_raw() |>
    writeBin(destfile)
}

