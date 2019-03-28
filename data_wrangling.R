
# libraries --------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(lubridate)
# data -------------------------------------------------------------------------
data_raw <- readr::read_csv("data/bitcoin_part_1.csv") %>% 
  dplyr::filter(!is.na(id)) %>%
  janitor::remove_empty("cols") %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(actor_posted_date = as.Date(actor_posted_time)) %>% 
  dplyr::mutate(n = 1)

# WARNING: doesn't take timezones into account
data_day <- data_raw %>% 
  dplyr::group_by(day = lubridate::floor_date(actor_posted_date, "day")) %>%
  dplyr::summarize(amount = sum(n)) %>%
  tidyr::complete(day = seq.Date(min(data_raw$actor_posted_date), max(data_raw$actor_posted_date), by="day")) %>% 
  dplyr::mutate(amount = tidyr::replace_na(amount, 0))

plot_day <- data_day %>%
  ggplot(aes(day, amount)) + 
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", span = 0.01) +
  scale_x_date("Time") +
  scale_y_continuous("N tweet")

library(plotly)

ggplotly(plot_day)
