days_in_month <- 30     # assume this for all months, roughly
# Functions called by server.R
forecast_future_sales <- function(sales,horizon,today,last_sales_month_start,forecasting_model,is.seasonality)
{
  # sales <- log(1+sales)
  sales_forecast_mean <- list()   # list of sales mean forecasts for all products
  # The current month is not complete, so will just project the sales in the current month so far to the entire month
  for (pid in colnames(sales)){
    if (sales[nrow(sales),pid]>0){  # if there were sales of pid in the current month:
      # cat("pid",pid,": current month's actual sales:",sales[nrow(sales),pid],",")
      # print(today)
      sales[nrow(sales),pid] <- sales[nrow(sales),pid]*(days_in_month/as.integer(today-last_sales_month_start))   # assuming that the current month's sales will continue at the same rate as they have been for this month, and the month's duration is 30 days
      # cat(" extrapolated sales:",sales[nrow(sales),pid],"\n")
      if (forecasting_model == "Holt-Winters"){
        sales_forecast <- HoltWinters(sales[,pid], gamma=is.seasonality)   # forecast sales for pid, using historical sales, using Holt-Winters exponential smoothing
        future_sales <- forecast::forecast.HoltWinters(sales_forecast, h=horizon) 
      } else if (forecasting_model == "ARIMA"){
        sales_forecast <- forecast::auto.arima(sales[,pid])   # forecast sales for pid, using historical sales, using Holt-Winters exponential smoothing
        future_sales <- forecast::forecast(sales_forecast, h=horizon) 
      } else {
        stop(paste("Forecasting model",sQuote(forecasting_model),'is not implemented. Stopping.'))
      }
      
      sales_forecast_mean[[pid]] <- future_sales$mean   # mean forecast for the next m months defined by the horizon (starting from the next month)
      sales_forecast_mean[[pid]] <- c(sales[nrow(sales),pid], sales_forecast_mean[[pid]])    # add the projected sales for the current month
    } else {   # if there were no sales of pid in the current month yet:
      sales_forecast <- HoltWinters(sales[-nrow(sales),pid], gamma=is.seasonality)   # forecast sales for pid, using historical sales for all previous months, using Holt-Winters exponential smoothing
      future_sales <- forecast::forecast.HoltWinters(sales_forecast, h=horizon+1)
      sales_forecast_mean[[pid]] <- future_sales$mean   # mean forecast for the current and the next m months defined by the horizon
    }
    # sales_forecast_mean[[pid]] <- exp(sales_forecast_mean[[pid]]) - 1
    sales_forecast_mean[[pid]] <- pmax(sales_forecast_mean[[pid]],0)   # just in case the forecasted sales are negative - reset to 0
  }
  return(sales_forecast_mean)
}

daily_sales_rate <- function(sales_forecast, planning_horizon, today, last_sales_month_start)
  # Convert monthly sales rates into daily sales rates (in sales per day) - needed to obtain the time-to-last, in days.
{
#   sales_rate <- ts(matrix(NA,ncol=length(stock_df$Stock.Name)), start = 1, end = (days_in_month-as.integer(today-last_sales_month_start)) + days_in_month*planning_horizon)   # sales per day
#   colnames(sales_rate) <- stock_df$Stock.Name
  sales_rate <- ts(matrix(NA,ncol=length(sales_forecast)), start = 1, end = (days_in_month-as.integer(today-last_sales_month_start)) + days_in_month*planning_horizon)   # sales per day
  colnames(sales_rate) <- names(sales_forecast)
  
  current_month <- match(months.Date(today), month.name)
  for (d in 1:nrow(sales_rate)){
    m <- match(months.Date(today+d), month.name) - current_month + 1   # the number of month corresponding to day d, starting from the current month (current month = 1)
    if (m <= 0) m <- m+12
    for (pid in colnames(sales_rate)){
      sales_rate[d,pid] <- sales_forecast[[pid]][m]/days_in_month
    }
  }
  
  return(sales_rate)
}

calc_time_to_last <- function(stock_df, sales_rates, pid, new_order=0)
  # Calculate time-to-last for a selected product pid, using the current SOH + new_order (defaults to 0), and the forecasted sales. 
  # The maximum time-to-last is limited by the planning horizon.
{
  
  tau <- 0   # initialize
  soh <- stock_df[stock_df$Stock.Name==pid,'SOH'] + new_order   # initialize the SOH
  while (soh>0){
    tau <- tau + 1
    if (tau<=nrow(sales_rates)){
      soh <- soh - sales_rates[tau,pid]*1    # update the soh
    } 
    else{
      break 
    }
  }  # tau after this loop is the time-to-last, given the current SOH level and the forecasted sales rate, for the specified pid
  
  return(tau)
}

calc_order <- function(stock_df, sales_rates, tau_min, discount_threshold)
  # Calculate the order sizes for all products (requiring that the order size should be enough to attract the discount), and the new times to last
{
  order_size <- 0
  tau <- tau_min    # initialize the desired time to last for all products
  tau_increment <- 1   # number of days to increase tau in one step
  
  while (order_size < discount_threshold){
    order <- list()
    new_time_to_last <- list()
    for (pid in colnames(sales_rates)){
      order[[pid]] <- 0   # initialize
      new_time_to_last[[pid]] <- calc_time_to_last(stock_df, sales_rates, pid)   # time to last with the current level of stock
      while (new_time_to_last[[pid]] < tau){
        order[[pid]] <- order[[pid]] + 1   # increase the order for pid by 1
        new_time_to_last[[pid]] <- calc_time_to_last(stock_df, sales_rates, pid, order[[pid]])
      }
    }
    order <- unlist(order)
    new_time_to_last <- unlist(new_time_to_last)
    order_size <- sum(order)
    tau <- tau + tau_increment
    if (tau > nrow(sales_rates)){
      cat("\n\nWARNING: the target time-to-last (",tau,"days) is above the planning horizon, while the total order",order_size,
          "is still below the discount threshold.\n")
      cat("Consider increasing the planning horizon and re-running.\n\n")
      break
    }
  }
  return (list('tau'=tau, 'order'=order, 'new_time_to_last'=new_time_to_last)) 
}

extract_month <- function(s) strsplit(s, "[.]")[[1]][1]
extract_year <- function(s) strsplit(s, "[.]")[[1]][2]