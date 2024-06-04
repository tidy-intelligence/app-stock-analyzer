library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(svglite)
library(scales)

# TODO: add 52 week range
# TODO: add average trading volume
# TODO: add daily trading volume as bar charts
# TODO: create second table with optimal weights
# TODO: add option to switch indexes (high effort)

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
      withSpinner(gt_output("table_summary"))
    )
  )
)

# input <- list("selected_symbols" = c("MSFT", "MCD", "UNH", "AAPL"))

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
  
  output$table_summary <- render_gt({
    if (is.null(stock_data_prepared())) {
      gt(tibble())
    } else {
      stock_data_prepared() |> 
        create_table_summary()
    }
  })
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
