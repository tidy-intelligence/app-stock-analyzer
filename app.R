library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggplot2)

# TODO: add 52 week range
# TODO: add average trading volume
# TODO: add daily trading volume as bar charts
# TODO: add option to switch indexes (high effort)
# TODO: make sure that all boxes are centered and have same width
# TODO: add figure description below figure
# TODO: make figure interactive

# Load data -----------------------------------------------------------

dates <- readRDS("data/dates.rds")
stock_data <- readRDS("data/stock_data.rds")
capm_data <- readRDS("data/capm_data.rds")
stocks <- sort(unique(stock_data$symbol))

# Helper functions ----------------------------------------------------

create_table_summary <- function(data) {
  data |> 
    select(image, symbol, mean, sd, alpha, beta, everything()) |> 
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
      footnote = "CAPM shows only coefficients that are statistically significant at 5%.",
      locations = cells_column_spanners("CAPM")
    )  |> 
    tab_footnote(
      footnote = "Distance from all-time high (ATH) since beginning of 2000.",
      locations = cells_column_labels("distance_from_ath")
    ) 
}

create_table_weights <- function(portfolio_weights) {
  portfolio_weights |> 
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
      footnote = "Efficient portfolio weights are computed using a benchmark multiple of 3.",
      locations = cells_column_labels("efp_weights")
    ) 
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
  benchmark_multiple <- 3
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

draw_efficient_frontier <- function(portfolio_weights) {
  length_year <- 12
  res <- tibble(
    a = seq(from = -0.4, to = 1.9, by = 0.01),
    mu = NA,
    sd = NA
  )
  for (i in seq_along(a)) {
    w <- (1 - a[i]) * portfolio_weights$mvp_weights + a[i] * portfolio_weights$efp_weights
    res$mu[i] <- length_year * t(w) %*% mu   
    res$sd[i] <- sqrt(length_year) * sqrt(t(w) %*% sigma %*% w)
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
        mu = length_year * mu,       
        sd = sqrt(length_year) * sqrt(diag(sigma))
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

# User interface ----------------------------------------------------------
ui <- fluidPage(
  
  # Custom styling
  tags$head(
    tags$style(HTML("
      .row {
        margin-bottom: 20px;
      }
    "))
  ),
  
  # App title
  titlePanel("Compute stock-specific summaries and optimal portfolio weights"),
  
  # Input panel
  fluidRow(
    box(
      width = 12,
      p("This app compute stock-specific return metrics and calculates optimal portfolio weights for your favorite stocks from the S&P 500 index. ",
        "You can check-out ", tags$a(href = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", target = "_blank", "wikipedia"), " for a list of S&P 500 companies and their symbols. ",
        "This app is a design concept and the data was last updated on ", dates$start_date, "."),
      selectizeInput("selected_symbols", label = "Select one or more symbols", choices = NULL, multiple = TRUE),
      actionButton("button", "Create tables") 
    )
  ),
  
  # Summary table
  fluidRow(
    box(
      width = 12,
      withSpinner(
        gt_output("table_summary")
      )
    )
  ),

  # Weights table
  fluidRow(
    box(
      width = 12,
      withSpinner(
        gt_output("table_weights")
      )
    )
  ),
  
  # Efficient frontier figure
  fluidRow(
    box(
      width = 12,
      withSpinner(
        plotOutput("figure_frontier"),
      )
    )
  )
)

# input <- list("selected_symbols" = c("MSFT", "NVDA", "UNH", "AAPL"))

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "selected_symbols", choices = stocks, server = TRUE)
  
  capm_data_prepared <- eventReactive(input$button, {
    if (is.null(input$selected_symbols) || length(input$selected_symbols) == 0) {
      return(NULL)
    } else {
      capm_data |> 
        prepare_capm_data(input)
    }
  })
  
  stock_data_prepared <- eventReactive(input$button, {
    if (is.null(input$selected_symbols) || length(input$selected_symbols) == 0) {
      return(NULL)
    } else {
      stock_data |> 
        prepare_stock_data(input) |> 
        left_join(capm_data_prepared(), join_by(symbol))
    }
  })
  
  portfolio_weights <- eventReactive(input$button, {
    stock_data |> 
      calculate_portfolio_weights(input) 
  })
  
  output$table_summary <- render_gt({
    if (is.null(stock_data_prepared())) {
      gt(tibble())
    } else {
      stock_data_prepared() |>
        create_table_summary()
    }
  })

  output$table_weights <- render_gt({
    if (is.null(stock_data_prepared())) {
      gt(tibble())
    } else {
      portfolio_weights() |>
        create_table_weights()
    }
  })
  
  output$figure_frontier <- renderPlot({
    draw_efficient_frontier(portfolio_weights())
  })
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
