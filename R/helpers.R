create_table_summary <- function(data, dates) {
  data |> 
    select(image, symbol, mean, sd, alpha, beta, everything()) |> 
    arrange(symbol) |> 
    gt() |>
    text_transform(
      locations = cells_body(columns = image),
      fn = function(x) {
        local_image(
          filename = paste0("data/logos/", x, ".webp")
        )
      }
    ) |> 
    gt_plt_dist(ret_data, trim = TRUE) |> 
    gt_plt_sparkline(adjusted_data, type = "shaded", same_limit = FALSE, label = FALSE, 
                     palette = c("black", NA, NA, NA, "grey")) |> 
    gt_plt_bar_pct(distance_from_ath, fill = "black", background = "lightgrey", width = 75) |> 
    cols_label(image = "",
               symbol = "", 
               mean = "Return",
               sd = "Volatility",
               sharpe_ratio = "SR",
               alpha = "Alpha",
               beta = "Beta",
               adjusted_data = "Prices",
               ret_data = "Returns",
               last_price = "Price",
               distance_from_ath = html("&Delta; ATH")) |> 
    cols_align(columns = distance_from_ath, "center") |> 
    tab_spanner(
      label = "Last 12 Months",
      columns = c(adjusted_data, ret_data)
    ) |>  
    tab_spanner(
      label = "Yesterday",
      columns = c(last_price, distance_from_ath)
    ) |> 
    tab_spanner(
      label = "Annualized",
      columns = c(mean, sd, sharpe_ratio)
    ) |> 
    tab_spanner(
      label = "CAPM",
      columns = c(alpha, beta)
    ) |> 
    tab_footnote(
      footnote = "CAPM is based on S&P 500 and shows only coefficients that are statistically significant at 5%.",
      locations = cells_column_spanners("CAPM")
    )  |> 
    tab_footnote(
      footnote = paste0("Distance from all-time high (ATH) since ", dates$start_date, "."),
      locations = cells_column_labels("distance_from_ath")
    ) |> 
    tab_options(table.width = pct(100))
}

create_table_weights <- function(portfolio_weights, input) {
  portfolio_weights |> 
    arrange(symbol) |> 
    mutate(image = symbol) |> 
    select(image, symbol, everything()) |> 
    gt() |> 
    text_transform(
      locations = cells_body(columns = image),
      fn = function(x) {
        local_image(
          filename = paste0("data/logos/", x, ".webp")
        )
      }
    ) |> 
    fmt_percent(
      columns = c(mvp_weights, efp_weights),
      decimals = 0
    ) |> 
    cols_label(image = "",
               symbol = "", 
               mvp_weights = "Minimum-Variance",
               efp_weights = "Efficient Portfolio") |> 
    tab_spanner(
      label = "Portfolio Weights",
      columns = c(mvp_weights, efp_weights)
    ) |> 
    tab_footnote(
      footnote = paste0("Efficient portfolio weights are computed using a benchmark multiple of ", input$multiple, "."),
      locations = cells_column_labels("efp_weights")
    ) |> 
    tab_options(table.width = pct(100))
}

prepare_capm_data <- function(capm_data, input) {
  capm_data |> 
    filter(symbol %in% input$selected_symbols) |> 
    mutate(estimate = if_else(p_value > 0.05, as.numeric(NA), estimate)) |> 
    select(symbol, term, estimate) |> 
    pivot_wider(id_cols = symbol, names_from = term, values_from = estimate) |> 
    mutate(beta = as.character(round(beta, 2)), 
           alpha = percent(as.numeric(alpha), accuracy = 1L),
           across(c(alpha, beta), ~if_else(is.na(.), "", .)))
}

prepare_stock_data <- function(stock_data, input) {
  stock_data |> 
    filter(symbol %in% input$selected_symbols) |> 
    group_by(symbol) |> 
    summarize(
      mean = percent((1+mean(ret))^252-1, accuracy = 1L),
      sd = percent(sd(ret)*sqrt(252), accuracy = 1L),
      sharpe_ratio = round(mean(ret) / sd(ret) * sqrt(252), 2),
      adjusted_data = list(adjusted[date >= Sys.Date()-365]), 
      ret_data = list(ret[date >= Sys.Date()-365]),
      last_price = round(last(adjusted), 0),
      distance_from_ath = last(adjusted) / max(adjusted),
      .groups = "drop") |>
    mutate(
      image = symbol
    ) 
}

calculate_portfolio_weights <- function(stock_data, input) {
  
  # Create returns matrix
  returns <- stock_data |> 
    filter(symbol %in% input$selected_symbols) |> 
    select(symbol, ret, date)
  
  returns_matrix <- returns |>
    pivot_wider(
      names_from = symbol,
      values_from = ret
    ) |>
    select(-date)
  sigma <- cov(returns_matrix)
  mu <- colMeans(returns_matrix)
  
  # Minimum-variance portfolio weights
  N <- ncol(returns_matrix)
  iota <- rep(1, N)
  sigma_inv <- solve(sigma)
  mvp_weights <- sigma_inv %*% iota
  mvp_weights <- mvp_weights / sum(mvp_weights)
  tibble(
    average_ret = as.numeric(t(mvp_weights) %*% mu),
    volatility = as.numeric(sqrt(t(mvp_weights) %*% sigma %*% mvp_weights))
  )
  
  # Efficient portfolio weights
  benchmark_multiple <- input$multiple
  mu_bar <- benchmark_multiple * t(mvp_weights) %*% mu
  C <- as.numeric(t(iota) %*% sigma_inv %*% iota)
  D <- as.numeric(t(iota) %*% sigma_inv %*% mu)
  E <- as.numeric(t(mu) %*% sigma_inv %*% mu)
  lambda_tilde <- as.numeric(2 * (mu_bar - D / C) / (E - D^2 / C))
  efp_weights <- mvp_weights +
    lambda_tilde / 2 * (sigma_inv %*% mu - D * mvp_weights)
  
  tibble(
    "symbol" = colnames(returns_matrix),
    "mvp_weights" = as.numeric(mvp_weights),
    "efp_weights" = as.numeric(efp_weights)
  )
}

draw_efficient_frontier <- function(stock_data, input, portfolio_weights) {
  returns <- stock_data |> 
    filter(symbol %in% input$selected_symbols) |> 
    select(symbol, ret, date)
  
  returns_matrix <- returns |>
    pivot_wider(
      names_from = symbol,
      values_from = ret
    ) |>
    select(-date)
  sigma <- cov(returns_matrix)
  mu <- colMeans(returns_matrix)
  
  res <- tibble(
    a = seq(from = -0.4, to = 1.9, by = 0.01),
    mu = NA,
    sd = NA
  )
  for (i in seq_along(res$a)) {
    w <- (1 - res$a[i]) * portfolio_weights$mvp_weights + res$a[i] * portfolio_weights$efp_weights
    res$mu[i] <-  (1+t(w) %*% mu)^252 -1
    res$sd[i] <- sqrt(252) * sqrt(t(w) %*% sigma %*% w)
  }
  
  figure <- res |>
    ggplot(aes(x = sd, y = mu)) +
    geom_point() +
    geom_point(
      data = res |> filter(a %in% c(0, 1)),
      size = 4
    ) +
    geom_point(
      data = tibble(
        mu = (1+mu)^252-1,       
        sd = sqrt(252) * sqrt(diag(sigma))
      ),
      aes(y = mu, x = sd), size = 1
    ) +
    labs(
      x = "Annualized standard deviation",
      y = "Annualized expected return",
      title = "Efficient frontier for selected stocks"
    ) +
    scale_x_continuous(labels = percent) +
    scale_y_continuous(labels = percent) +
    theme_classic() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          strip.background = element_blank(),
          axis.ticks = element_blank())
  
  figure
}
