### Simple input of data
library(tidyverse)
library(tidyquant)
library(quantmod)

getLargeStocks <- function() {
  # Get and read all sector data ----

  # Get SP500 data
  stock_index <-tq_index("SP500")
  stock_index <- drop_na(stock_index)

  # Get and read all pricing data ----
  data <- NULL
  for (i in stock_index$symbol) {
    print(i)
    
    prices <- tryCatch({
      tq_get(i, get = "stock.prices")
    }, error = function(e) {
      print(paste0("Error with connection", e))
    })
    if(is.na(prices)){
      print("Data is Na")
      next
    }
    prices$date <- as.Date(prices$date)
    if (nrow(prices) < 400) {
      print("Not enough data")
      next
    }
    
    data <- rbind(data, prices)
  }
  # Write Final Data
  
  write_rds(data, "dailyPrice.RDS")
}

