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

# TODO: make figure interactive
# TODO: figure should only be drawn when button is hit

# Load data -----------------------------------------------------------

dates <- readRDS("data/dates.rds")
stock_data <- readRDS("data/stock_data.rds")
capm_data <- readRDS("data/capm_data.rds")
stocks <- sort(unique(stock_data$symbol))

# Helper functions ----------------------------------------------------

source("R/helpers.R")

# User interface ----------------------------------------------------------
ui <- fluidPage(
  
  # Custom styling
  tags$head(
    tags$style(HTML("
      .row {
        margin-bottom: 32px;
      }
      .box {
        max-width: 720px;
        margin: 0 auto;
        min-height: 180px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        border-radius: 8px;
        padding: 8px;
      }
      .load-container {
        height: 180px;
      }
    "))
  ),
  
  # App title
  titlePanel(h2("Stock-specific summaries and optimal portfolio weights", align = "center"),
             windowTitle = "Stock Analyzer"),
  
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
        plotOutput("figure_frontier")
      ),
      textOutput("figure_description")
    )
  )
)

# input <- list("selected_symbols" = c("MSFT", "NVDA", "UNH", "AAPL"))

# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  updateSelectizeInput(session, "selected_symbols", choices = stocks, server = TRUE,
                       selected = c("MSFT", "NVDA", "UNH", "AAPL"))
  
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
    draw_efficient_frontier(stock_data, input, portfolio_weights())
  })
  
  output$figure_description <- renderText({
    "The big dots indicate the location of the minimum variance and the efficient portfolio that delivers 3 times the expected return of the minimum variance portfolio, respectively. The small dots indicate the location of the individual constituents."
  })
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
