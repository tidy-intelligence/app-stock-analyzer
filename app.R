library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggplot2)

# TODO: add benchmark performance of s&p 500 and risk-free rate

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
      .row #input-row {
        margin-bottom: 0px;
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
      .irs-grid-pol.small {
        height: 0px;
      }
      .irs-min, .irs-max {
        visibility: hidden !important;
      }
    "))
  ),
  
  chooseSliderSkin("Shiny", color = "black"),
  
  # App title
  titlePanel(h2("Stock-specific summaries and optimal portfolio weights", align = "center"),
             windowTitle = "Stock Analyzer"),
  
  # Input panel
  fluidRow(
    box(
      width = 12,
      p("This app compute stock-specific return metrics and calculates optimal portfolio weights for your favorite stocks from the S&P 500 index. ",
        "You can check-out ", tags$a(href = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", target = "_blank", "wikipedia"), " for a list of S&P 500 companies and their symbols. ", tags$br(),
        "This app is a prototype based on ", tags$a(href = "https://www.tidy-finance.org/r/introduction-to-tidy-finance.html", target = "_blank", "Tidy Finance with R"),
        ". The data starts at ", dates$start_date, " and was last updated on ", dates$end_date, ". ", tags$br(),
        "You can find the source code of this app on ", tags$a(href = "https://github.com/christophscheuch/app-stock-analyzer", target = "_blank", "GitHub"), "."),
      fluidRow(
        id = "input-row",
        column(6, style = "padding-right: 16px;",
               selectizeInput("selected_symbols", label = "Select one or more symbols", choices = NULL, multiple = TRUE)
        ),
        column(6,
               sliderInput("multiple", "Pick a benchmark multiple", min = 1, max = 5, value = 3)
        )
      ),
      actionButton("button", "Create tables")
    )
  ),
  
  # Summary table
  fluidRow(
    box(
      width = 12,
      withSpinner(
        gt_output("table_summary"),
        color = "black"
      )
    )
  ),

  # Weights table
  fluidRow(
    box(
      width = 12,
      withSpinner(
        gt_output("table_weights"),
        color = "black"
      )
    )
  ),
  
  # Efficient frontier figure
  fluidRow(
    box(
      width = 12,
      withSpinner(
        plotOutput("figure_frontier"),
        color = "black"
      ),
      textOutput("figure_description")
    )
  )
)

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
        create_table_summary(dates)
    }
  })

  output$table_weights <- render_gt({
    if (is.null(stock_data_prepared())) {
      gt(tibble())
    } else {
      portfolio_weights() |>
        create_table_weights(input)
    }
  })
  
  figure_frontier <- eventReactive(input$button, {
    draw_efficient_frontier(stock_data, input, portfolio_weights())
  })
  
  output$figure_frontier <- renderPlot({
    figure_frontier()
  })
  
  figure_description <- eventReactive(input$button, {
    paste0(
      "The big dots indicate the location of the minimum variance and the efficient portfolio that delivers ",  
      input$multiple,
      " times the expected return of the minimum variance portfolio, respectively. The small dots indicate the location of the individual constituents.")
  })
  
  output$figure_description <- renderText({
    figure_description()
  })
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
