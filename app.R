library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(tidyr)
library(gt)
library(gtExtras)
library(svglite)
library(scales)
library(ggplot2)
library(duckdb)
library(DBI)

# Load helpers --------------------------------------------------------------------------------

source("R/helpers.R")

# Load data -----------------------------------------------------------

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/stock-analyzer.duckdb")

dates <- tbl(con, "dates") |> collect()
stocks <- tbl(con, "stocks") |> collect()

dbDisconnect(con)

# Helper functions ----------------------------------------------------

source("R/helpers.R")

# User interface ------------------------------------------------------

ui <- fluidPage(
  
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  chooseSliderSkin("Shiny", color = "black"),
  
  # App title
  titlePanel(h2("Analyze stock returns & optimal portfolio weights", align = "center"),
             windowTitle = "Stock Analyzer"),
  
  # Input panel
  fluidRow(
    box(
      width = 12,
      p("This app is a prototype based on ", tags$a(href = "https://www.tidy-finance.org/r/introduction-to-tidy-finance.html", target = "_blank", "Tidy Finance with R"), 
        ". It computes stock-specific return metrics and optimal portfolio weights for your favorite stocks from the S&P 500 index. ",
        "You can check-out ", tags$a(href = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies", target = "_blank", "wikipedia"), 
        " for a list of S&P 500 companies and their symbols. ", tags$br(), tags$br(),
        "The data starts at ", dates$start_date, " and was last updated on ", dates$end_date, ". ", tags$br(), tags$br(),
        "You can find the source code of this app on ", tags$a(href = "https://github.com/tidy-intelligence/app-stock-analyzer", target = "_blank", "GitHub.")),
      fluidRow(
        id = "input-row",
        column(6, style = "padding-right: 16px;",
               selectizeInput("selected_symbols", label = "Select one or more symbols", choices = NULL, multiple = TRUE,
                              selected = c("MSFT", "NVDA", "UNH", "AAPL", "TSLA"))
        ),
        column(6,
               sliderInput("multiple", "Pick a benchmark multiple", min = 1, max = 5, value = 3)
        )
      ),
      actionButton("button", "Update tables")
    )
  ),
  
  # Summary table
  fluidRow(
    box(
      width = 12,
      div(class = "scrollable-box",
          div(class = "wide-table",
          withSpinner(
            gt_output("table_summary"),
            color = "black"
          )
          )
      )
    )
  ),

  # Weights table
  fluidRow(
    box(
      width = 12,
      div(class = "scrollable-box",
      withSpinner(
        gt_output("table_weights"),
        color = "black"
      ))
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
  
  updateSelectizeInput(session, "selected_symbols", choices = stocks$symbol, server = TRUE,
                       selected = c("MSFT", "NVDA", "UNH", "AAPL", "TSLA", "ABNB"))
  
  stock_data_filtered <- eventReactive(input$button, {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/stock-analyzer.duckdb")
    results <- tbl(con, "stock_data") |> 
      filter(symbol %in% input$selected_symbols) |> 
      collect() 
    dbDisconnect(con)
    results
  })
  
  capm_data_filtered <- eventReactive(input$button, {
    con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "data/stock-analyzer.duckdb")
    results <- tbl(con, "capm_data") |> 
      filter(symbol %in% input$selected_symbols) |> 
      collect() 
    dbDisconnect(con)
    results
  })
  
  capm_data_prepared <- eventReactive(input$button, {
      capm_data_filtered() |> 
        prepare_capm_data()
  })
  
  stock_data_prepared <- eventReactive(input$button, {
    stock_data_filtered() |> 
        prepare_stock_data() |> 
        left_join(capm_data_prepared(), join_by(symbol))
  })
  
  portfolio_weights <- eventReactive(input$button, {
    stock_data_filtered() |> 
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
  
  figure_frontier <- reactive({
    if (is.null(portfolio_weights())) {
      ggplot() + theme_classic()
    } else {
      draw_efficient_frontier(stock_data_filtered(), portfolio_weights())
    }
  })
  
  output$figure_frontier <- renderPlot({
    figure_frontier()
  })
  
  figure_description <- reactive({
    paste0(
      "The big dots indicate the location of the minimum variance and the efficient portfolio that delivers ",  
      input$multiple,
      " times the expected return of the minimum variance portfolio, respectively. The small dots indicate the location of the individual constituents.")
  })
  
  output$figure_description <- renderText({
    figure_description()
  })
  
  delay(1000, click("button"))
  
}

# Run app -----------------------------------------------------------------

shinyApp(ui, server)
