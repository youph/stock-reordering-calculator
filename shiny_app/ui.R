library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Stock reordering calculator"),
  
  sidebarLayout(
    sidebarPanel(
                 
                 fileInput("file", label = h4("Upload Stock On Hand and usage history (.csv file)")),
                 
                 dateInput("today", 
                           label = h4("Order date"), 
                           value = Sys.Date()),
                 
                 numericInput("tau_min", 
                              label = h4("Minimum days for stock to last"), 
                              min=1, value = 7),
                 
                 numericInput("discount_threshold", 
                              label = h4("Discount threshold, items"), 
                              min=1, value = 36),
                 
                 numericInput("planning_horizon", 
                              label = h4("Planning horizon, months"), 
                              min=1, value = 6),
                 
                 selectInput("forecasting_model", label = h4("Sales forecasting model"), 
                             choices = list("Holt-Winters" = "Holt-Winters", "ARIMA" = "ARIMA"), selected = "ARIMA")
                 
                 # checkboxInput("plot.forecasts", label=h4("Plot sales forecasts"), value=FALSE)
                 
                 # checkboxInput("add.total", label=h4("Add totals"), value=FALSE)
                 
    ),
    mainPanel(
      actionButton("action", label = "Calculate re-order", icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      tabsetPanel(
        tabPanel('Stock On Hand and Usage history',dataTableOutput("stock_df")),
        tabPanel('Stock to re-order',dataTableOutput("order"),downloadButton('downloadOrder', 'Download order as csv file'))
      )
    )
  ),
  
  # Add the "busy" indicator as described in http://withr.me/add-calculation-in-process-indictor-for-shiny-application/
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "busy.js")
    )
  ),
  div(class = "busy",  
      p("Calculation in progress...") ,
      img(src="ajax-loader-2.gif")
  )
))