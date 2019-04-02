
# libraries --------------------------------------------------------------------
library(tidyverse)
library(xts)
library(dygraphs)

# data -------------------------------------------------------------------------
bitcoin_file <- "bitstampUSD.csv.gz"
URL <- "http://api.bitcoincharts.com/v1/csv"
source_file <- file.path(URL,bitcoin_file)
# Data destination on local disk
dataDir <- here::here("data")
dest_file <- file.path(dataDir,bitcoin_file)
# Download to disk
download.file(source_file,destfile = dest_file)
# Uncompress .gz file and read into a data frame
bitcoin_raw <- read.csv(gzfile(dest_file),header=FALSE) %>% 
  dplyr::rename(unixtime = V1, price = V2, amount = V3)

# data wrangling ---------------------------------------------------------------
bitcoin_data <- bitcoin_raw %>% 
  dplyr::mutate(date = as.POSIXct(raw$unixtime, origin="1970-01-01") %>% as.Date()) %>% 
  dplyr::select(-unixtime) %>% 
  dplyr::mutate(value = price * amount) %>% 
  dplyr::group_by(date) %>% 
  dplyr::summarise(
    count = n(),
    m_price = mean(price, na.rm = TRUE),
    m_amount = mean(amount, na.rm = TRUE),
    m_value = mean(value, na.rm = TRUE))

# Make the m_value variable into a time series object
daily_ts <- xts::xts(bitcoin_data$m_value, order.by=bitcoin_data$date)
# Plot with htmlwidget dygraph
dygraph(daily_ts,ylab="US Dollars", 
        main="Average Value of bitstampUSD Buys") %>%
  dySeries("V1",label="Avg_Buy") %>%
  dyRangeSelector()
