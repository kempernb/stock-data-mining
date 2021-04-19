### Simple input of data
library(tidyverse)
library(tidyquant)

getLargeStocks <- function() {
  # Get and read all sector data ----

  # Get SP500 data only for now
  stock_index <-tq_index("SP500")
  stock_index <- drop_na(stock_index)

  # Get and read all pricing data ----
  data <- NULL
  for (i in stock_index$symbol) {
    print(i)
    
    # Get the stock prices! Includes error handeling if occurs
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
    if (nrow(prices) < 200) {
      print("Not enough data")
      next
    }
    
    data <- rbind(data, prices)
  }
  
  # Write Final Data
  write_rds(data, "dailyPrice.RDS")
}

