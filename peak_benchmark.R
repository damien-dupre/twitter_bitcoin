
# libraries --------------------------------------------------------------------
# data wrangling
library(tidyverse)
# time series
library(zoo)
# filter
library(features)
library(signal)
# peak detection
library(splus2R)

# data -------------------------------------------------------------------------
data_day <- readr::read_rds("data_day.rds") %>% 
  tibble::rowid_to_column("row_id")

# time-serie -------------------------------------------------------------------
zoo_day <- zoo::zoo(data_day$amount, order.by = data_day$row_id)

################################################################################
#                                    Filters                                   #
################################################################################

# filter 1 ---------------------------------------------------------------------
data_day$filter1 <- loess(amount ~ row_id, data=data_day, span=0.1) %>% predict() 
plot(data_day$filter1, type = "l")

# filter 2 ---------------------------------------------------------------------
# Generate Butterworth filter polynomial coefficients 3rd order for 10Hz
bf <- signal::butter(3, 0.01) 
data_day$filter2 <- signal::filter(bf, data_day$amount)

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter2), col = "red", alpha = 0.5)

# filter 3 ---------------------------------------------------------------------
data_day$filter3 <- stats::filter(
  data_day$amount, filter = 0.1, method = "convolution") 

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter3), col = "red", alpha = 0.5)

# filter 4 ---------------------------------------------------------------------
zoo_day <- zoo::zoo(data_day$amount, order.by = data_day$row_id)

data_day$filter4 <- stl(ts(zoo_day, frequency=100), "periodic")$time.series[,"trend"]

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter4), col = "red", alpha = 0.5)

# filter 5 ---------------------------------------------------------------------
features_method <- features::features(data_day$row_id, data_day$amount)

plot(features_method)

################################################################################
#                             Peak detection                                   #
################################################################################

# peak function 1 --------------------------------------------------------------
peaks <- function (x, y = NULL, mode = "maxmin"){
  if (!mode %in% c("max", "min", "maxmin"))
    stop("unknown mode:", mode)
  xy <- xy.coords(x, y)
  x <- xy$x
  y <- xy$y
  l <- length(y)
  ym1 <- c(y[-1], y[l])
  yp1 <- c(y[1], y[-l])
  if (mode == "min") {
    xx <- x[y < ym1 & y < yp1]
    yy <- y[y < ym1 & y < yp1]
  }
  else if (mode == "max") {
    xx <- x[y > ym1 & y > yp1]
    yy <- y[y > ym1 & y > yp1]
  }
  else {
    xx <- x[y > ym1 & y > yp1 | y < ym1 & y < yp1]
    yy <- y[y > ym1 & y > yp1 | y < ym1 & y < yp1]
  }
  list(x = xx, y = yy)
}

data_day_f1 <- peaks(data_day$row_id, data_day$filter1) %>% 
  as.data.frame()

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5) +
  geom_point(data_day_f1, mapping = aes(x, y), col = "red")

# peak function 2 --------------------------------------------------------------
data_day$peak2 <- splus2R::peaks(data_day$trend, span=7, strict=TRUE)

data_day <- data_day %>% 
  dplyr::mutate(peak2 = case_when(
    peak2 == TRUE ~ filter1,
    TRUE ~ NA_real_
  ))

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5) +
  geom_point(data_day, mapping = aes(row_id, peak2), col = "red")
















