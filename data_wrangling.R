
# libraries --------------------------------------------------------------------
library(tidyverse)
library(bigrquery)
library(janitor)
library(lubridate)
library(plotly)
# database ---------------------------------------------------------------------
con <- DBI::dbConnect(bigquery(),
                      project = config::get("dcu_bigquery")$project,
                      dataset = config::get("dcu_bigquery")$dataset,
                      billing = config::get("dcu_bigquery")$billing
)
#DBI::dbListTables(con)
bitcoin_blockchain <- con %>% tbl("bitcoin_blockchain")
# data -------------------------------------------------------------------------
data_raw <- bitcoin_blockchain %>%
  dplyr::select(id,actor_postedTime, object_summary) %>% 
  dplyr::filter(!is.na(id)) %>%
  dplyr::filter(!is.na(actor_postedTime)) %>%
  collect()
# data -------------------------------------------------------------------------
data_raw <- data_raw %>% 
  dplyr::filter(stringr::str_detect(object_summary, "bitcoin|Bitcoin")) %>% 
  janitor::remove_empty("cols") %>%
  janitor::clean_names() %>%
  dplyr::mutate(actor_posted_date = as.Date(actor_posted_time))%>% 
  dplyr::mutate(n = 1)

first_date <- min(data_raw$actor_posted_date)
last_date <- max(data_raw$actor_posted_date)
# data -------------------------------------------------------------------------
# WARNING: doesn't take timezones into account
data_day <- data_raw %>% 
  dplyr::group_by(day = lubridate::floor_date(actor_posted_date, "day")) %>%
  dplyr::summarize(amount = sum(n)) %>%
  tidyr::complete(day = seq.Date(first_date, last_date, by="day")) %>% 
  dplyr::mutate(amount = tidyr::replace_na(amount, 0))

# readr::write_rds(data_day, "data/data_day.rds")
data_day <- readr::read_rds("data/data_day.rds")
# plot -------------------------------------------------------------------------
plot_day <- data_day %>%
  ggplot(aes(day, amount)) + 
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", span = 0.01) +
  scale_x_date("Time") +
  scale_y_continuous("N tweet")

ggplotly(plot_day)

# SQL --------------------------------------------------------------------------
# sql <- "SELECT id, DATE(actor_postedTime) AS date_tweet, count(*) AS n
#         FROM `stable-plasma-690.Bitcoin_Blockchain.bitcoin_blockchain`
#         GROUP BY id, DATE(actor_postedTime);"
# 
# tb <- bq_project_query(config::get("dcu_bigquery")$billing, sql)
# 
# data_day <- bq_table_download(tb) # too long
