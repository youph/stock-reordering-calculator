# Run this code once when the app is launched:
library(shiny)
# library(DT)
# source("helpers.R")

# Define interactive server logic
shinyServer(function(input, output, session) {
  # Run this code once each time a user visits the app
  # PARAMETERS of the run:
  is.seasonality <- FALSE    # should we take into account seasonality in the historical sales data, when forecasting future sales? 
  # To estimate seasonality, set this to NULL
  # TODO: change this to automatic assignment!
  
  action_counter <- 0   # initialize the action counter for the user
  
  observe({
    # Observe input$tau_min and adjust if necessary:
    if (!is.na(input$tau_min)){
      tau_min <- input$tau_min
      planning_horizon <- input$planning_horizon
      
      if (tau_min < 1) {
        cat("WARNING: tau_min cannot be less than 1 day, adjusting tau_min.\n")
        tau_min <- 1
      } 
      
      if (tau_min > planning_horizon*days_in_month) {
        cat("WARNING: tau_min cannot be larger than the planning horizon, adjusting tau_min.\n")
        tau_min <- planning_horizon*days_in_month
      }
      
      updateNumericInput(session, "tau_min",
                         value = tau_min, min = 1, max = planning_horizon*days_in_month, step = 1)
    }
    
    # Observe input$today and adjust if necessary:
    today <- input$today
    stock_df <- get_stock_df()
    if (!is.null(stock_df)){
      years <- as.integer(unique(sapply(colnames(stock_df[4:ncol(stock_df)]), extract_year)))   # extract years from stock_df
      months <- sapply(colnames(stock_df[4:ncol(stock_df)]), extract_month)   # extract months from stock_df
      last_sales_month_start <- as.Date(paste("1",months[1],years[1]), format="%d %b %y")   # the 1st day of the last month in the sales data
      if (today - last_sales_month_start > 31) {
        warning("The uploaded SOH and usage history is obsolete for the selected order date of ",as.character(today),
                ". Consider uploading a fresh SOH and usage history up to ",months(today)," ",unlist(strsplit(as.character(today),"-"))[1],
                " , or selecting another order date. Adjusting the order date.")
        updateDateInput(session, "today", value = last_sales_month_start + 15, min = last_sales_month_start, max = last_sales_month_start + 31)
      }
    }
    
    # Observe input$discount_threshold and adjust if less than 1:
    if (input$discount_threshold < 1) updateDateInput(session, "discount_threshold", value = 1, min = 1)
  })
  
  get_stock_df <- reactive({
    if (!is.null(input$file)) {
      stock_df <- read.csv(input$file$datapath, comment.char="#")
      # remove columns with all NAs:
      stock_df <- Filter(function(x) !all(is.na(x)), stock_df)
      # remove rows with all NAs:
      stock_df <- na.omit(stock_df)
      # Reload stock_df, this time interpreting PDE as character (to avoid losing leading zeros):
      stock_df.colClasses <- sapply(stock_df[1,], class)
      stock_df.colClasses["PDE"] <- "character"
      stock_df <- read.csv(input$file$datapath, comment.char="#", colClasses = stock_df.colClasses)
      # remove columns with all NAs:
      stock_df <- Filter(function(x) !all(is.na(x)), stock_df)
      # remove rows with all NAs:
      stock_df <- na.omit(stock_df)
      return(stock_df)
    }
  })
  
  calculate_order <- reactive({
    # Run this code every time a user changes widget(s) that output$order relies on
    if (input$action > action_counter){# take a dependency on input$action
      # cat('input$action',input$action,', action counter',action_counter,'\n')
      action_counter <<- input$action  # update the action counter (note that it is a global variable with respect to renderDataTable())
      stock_df <- get_stock_df()
      tau_min <- input$tau_min
      if (!is.null(stock_df)){
        years <- as.integer(unique(sapply(colnames(stock_df[4:ncol(stock_df)]), extract_year)))   # extract years from stock_df
        months <- sapply(colnames(stock_df[4:ncol(stock_df)]), extract_month)   # extract months from stock_df
        months_int <- match(months, month.abb)
        sales_matrix <- t(as.matrix(stock_df[,4:ncol(stock_df)]))
        sales <- ts(sales_matrix[nrow(sales_matrix):1,], start=c(tail(years,1),tail(months_int,1)), end=c(head(years,1),head(months_int,1)), frequency = 12)
        colnames(sales) <- stock_df$Stock.Name
        rm(sales_matrix)
        # Note that the last month data in 'sales' might be incomplete (if the month is still in progress)
        # today <- as.Date("2015-11-30")
        today <- input$today   # order date. Isolate to avoid dependence on input$today?
        last_sales_month_start <- as.Date(paste("1",months[1],years[1]), format="%d %b %y")   # the 1st day of the last month in the sales data
        
        # Forecast future sales of each product, using historical sales
        planning_horizon <- input$planning_horizon
        sales_forecast <- forecast_future_sales(sales,planning_horizon,today,last_sales_month_start,input$forecasting_model,is.seasonality)
        sales_rates <- daily_sales_rate(sales_forecast, planning_horizon, today, last_sales_month_start)
        # Determine time-to-last for each product:
        time_to_last <- list()
        for (pid in colnames(sales_rates)){
          time_to_last[[pid]] <- calc_time_to_last(stock_df, sales_rates, pid)   # time to last for each product, given current SOH and forecasted sales [in days]
        }
        time_to_last <- unlist(time_to_last)
        cat("\n\nEstimated times-to-last with the current SOH levels:\n\n")
        print(time_to_last)
        
        if (any(time_to_last < tau_min)){  # if any of the times to last with the current SOH levels is below tau_min, prompt the new order:
          cat("\nOne or more times-to-last is below the required minimum of",tau_min,"days, hence new order is needed.\n")
          # Determine quantities to order, for each product, to ensure the new time-to-last is above tau_min, and also the order size is above discount_threshold
          tmp <- calc_order(stock_df, sales_rates, tau_min, input$discount_threshold)
          tau <- tmp$tau
          order <- tmp$order
          new_time_to_last <- tmp$new_time_to_last
          rm(tmp) 
          #         cat("Order quantities :\n")
          #         print(order)
          #         cat("Order total =",sum(order),"\n\n")
          #         cat("Estimated times-to-last with new stock levels after the order:\n\n")
          #         print(new_time_to_last)
        } else {
          cat("\n\nNone of the times-to-last is below the required minimum of",tau_min,"days. Re-ordering these products is not needed.\n")
          tmp <- calc_order(stock_df, sales_rates, tau_min, input$discount_threshold)
          order <- tmp$order*0   # no order is needed
          new_time_to_last <- time_to_last   # current stock levels time to last
        }
        order_df = data.frame("PDE"=stock_df$PDE,"Product Name"=names(order), "SOH"=stock_df$SOH, "Expected days to last"=time_to_last, "Reorder qty"=order, "Expected days to last after reorder"=new_time_to_last, row.names = NULL)
        order_df = rbind(order_df, data.frame("PDE"="TOTAL","Product Name"="TOTAL", "SOH"=sum(stock_df$SOH), "Expected days to last"=round(mean(time_to_last)), "Reorder qty"=sum(order), "Expected days to last after reorder"=round(mean(new_time_to_last))))
        
        return(order_df)
      }
    }   
  })
  
  # Outputs:
  
  output$stock_df <- renderDataTable({
    # Run this code every time a user changes widget(s) that output$stock_df relies on
    return(get_stock_df())
    }
    )
  
  output$order <- renderDataTable({
    calculate_order()
  }, options = list(rowCallback = I(   # this highlights the row with totals by yellow background
    'function(row, data) {
    if (data[0] == "TOTAL" & data[1] == "TOTAL")
    $("td", row).css("background", "yellow");
    }'
                        ), pageLength = 25)
  )
  
  output$downloadOrder <- downloadHandler(
    filename = function() { paste(Sys.Date(),'Order',input$file,sep="__") }, 
    content = function(file) {
      write.csv(calculate_order()[,c("PDE","Product.Name","Reorder.qty")], file, row.names = FALSE)
    }
  )
  
#   proxy = dataTableProxy('order')
#   observeEvent(input$add.total, {
#     addRow(proxy, data.frame("PDE"="TOTAL","Product Name"="TOTAL", "SOH"=sum(stock_df$SOH), "Expected days to last"=round(mean(time_to_last)), "Reorder qty"=sum(order), "Expected days to last after reorder"=round(mean(new_time_to_last))))
#   })
  
})