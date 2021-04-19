# Stock Analysis, Nicholas Kemper, 2021

# Read in packages and data compiler ----
library(tidyverse)
library(fpc)
library(tidyquant)  # Analyzing stocks
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(caret)      # pre-processing
source("Data-Compiler/data-compiler.R")

# Get all data ----
#getLargeStocks()

# Read in and format prices ----
prices <- readRDS("dailyPrice.RDS")

# Read in stock index
stock_index <-tq_index("SP500")
stock_index <- drop_na(stock_index)

year_ago <- as.Date(Sys.Date()) - 365

# Add momentum
price.momentum <- prices %>%
  subset(date >= year_ago) %>%
  mutate(momentum = (close - open) / open) %>%
  select(date, symbol, momentum)
# pivot_wider(names_from = symbol,values_from = momentum)

# Analysis ----
# Top moving stocks from last year
price.momentum %>%
  group_by(symbol) %>%
  dplyr::summarise(totmoment = sum(momentum)) %>%
  arrange(-totmoment)

# create 3 new variables, year, month, and month year
# Then group by and compute total and sd
price.momentum <- price.momentum %>%
  mutate(
    year = year(date),
    month = month(date),
    yearmonth = paste0(year, "-", month)
  ) %>% 
  group_by(symbol, yearmonth) %>% 
  dplyr::summarise(totmoment = sum(momentum), stdmoment = sd(momentum))

# Translate to wide format for cluster analysis
price.wide <- price.momentum %>%
  select(symbol, yearmonth, totmoment, stdmoment) %>%
  pivot_wider(names_from = yearmonth,
              values_from = c(totmoment, stdmoment)) %>% 
  column_to_rownames("symbol") %>% 
  # Drop the na values, only about 2-3 rows
  drop_na()

# normalized distance
norm.values <- preProcess(price.wide, method = c("center", "scale"))
prices.norm <- predict(norm.values, price.wide)

summary(prices.norm)

fviz_nbclust(prices.norm, kmeans, method = "wss")

n_clusters <- 3
km <- kmeans(prices.norm, n_clusters, nstart = 25)
fviz_cluster(km, data = prices.norm)


# show cluster membership
km$cluster

# centroids
km$centers

# within-cluster sum of squares
km$withinss

# cluster size
km$size


# ## plotting profile plot of centroids
# # plot an empty scatter plot
# plot(c(0), xaxt = 'n', ylab = "", type = "l", ylim = c(min(km$centers), max(km$centers)), xlim = c(0, 8))
# 
# # label x-axes
# axis(1, at = c(1:length(names(prices.norm))), labels = names(prices.norm))
# 
# # plot centroids
# for (i in c(1:n_clusters)) {
#   lines(km$centers[i, ], lty = i, lwd = 2, col = ifelse(i %in% c(1, 3, 5),"black", "dark gray"))
# }
# 
# # name clusters
# text(x = 0.5, y = km$centers[, 1], labels = paste("Cluster", c(1:n_clusters)))




