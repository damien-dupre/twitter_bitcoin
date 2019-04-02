
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
library(wmtsa)
library(pastecs)
library(ggpmisc)

# data -------------------------------------------------------------------------
data_day <- readr::read_rds("data/data_day.rds") %>% 
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
data_day$peak2 <- splus2R::peaks(data_day$filter1, span=7, strict=TRUE)

data_day <- data_day %>% 
  dplyr::mutate(peak2 = case_when(
    peak2 == TRUE ~ filter1,
    TRUE ~ NA_real_
  ))

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5) +
  geom_point(data_day, mapping = aes(row_id, peak2), col = "red")

# peak function 3 --------------------------------------------------------------
PeakCycle <- function(Data=as.vector(sunspots), SearchFrac=0.02){
  # using package "wmtsa"
  #the SearchFrac parameter just controls how much to look to either side 
  #of wavCWTPeaks()'s estimated maxima for a bigger value
  #see dRange
  Wave <- wavCWT(Data)
  WaveTree <- wavCWTTree(Wave)
  WavePeaks <- wavCWTPeaks(WaveTree, snr.min=5)
  WavePeaks_Times <- attr(WavePeaks, which="peaks")[,"iendtime"]
  
  NewPeakTimes <- c()
  dRange <- round(SearchFrac*length(Data))
  for(i in 1:length(WavePeaks_Times)){
    NewRange <- max(c(WavePeaks_Times[i]-dRange, 1)):min(c(WavePeaks_Times[i]+dRange, length(Data)))
    NewPeakTimes[i] <- which.max(Data[NewRange])+NewRange[1]-1
  }
  
  return(matrix(c(NewPeakTimes, Data[NewPeakTimes]), ncol=2, dimnames=list(NULL, c("PeakIndices", "Peaks"))))
}

peak3 <- PeakCycle(data_day$filter1) %>% 
  as.data.frame()

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5) +
  geom_point(peak3, mapping = aes(PeakIndices, Peaks), col = "red")

# peak function 4 --------------------------------------------------------------

data_day$peak4 <- turnpoints(data_day$filter1)$peaks

data_day <- data_day %>% 
  dplyr::mutate(peak4 = case_when(
    peak4 == TRUE ~ filter1,
    TRUE ~ NA_real_
  ))

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5) +
  geom_point(data_day, mapping = aes(row_id, peak4), col = "red")

# peak function 5 --------------------------------------------------------------
findpeaks <- function(vec,bw=1,x.coo=c(1:length(vec))){
  pos.x.max <- NULL
  pos.y.max <- NULL
  pos.x.min <- NULL
  pos.y.min <- NULL
  for(i in 1:(length(vec)-1)) {
    if((i+1+bw)>length(vec)){
      sup.stop <- length(vec)
    }else{
      sup.stop <- i+1+bw
    }
    if((i-bw)<1){
      inf.stop <- 1
    }else{
      inf.stop <- i-bw
    }
    subset.sup <- vec[(i+1):sup.stop]
    subset.inf <- vec[inf.stop:(i-1)]
    
    is.max   <- sum(subset.inf > vec[i]) == 0
    is.nomin <- sum(subset.sup > vec[i]) == 0
    
    no.max   <- sum(subset.inf > vec[i]) == length(subset.inf)
    no.nomin <- sum(subset.sup > vec[i]) == length(subset.sup)
    
    if(is.max & is.nomin){
      pos.x.max <- c(pos.x.max,x.coo[i])
      pos.y.max <- c(pos.y.max,vec[i])
    }
    if(no.max & no.nomin){
      pos.x.min <- c(pos.x.min,x.coo[i])
      pos.y.min <- c(pos.y.min,vec[i])
    }
  }
  return(list(pos.x.max,pos.y.max,pos.x.min,pos.y.min))
}

peak5 <- findpeaks(data_day$filter1)

peak5 <- data.frame(x = peak5[[1]], y = peak5[[2]])

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5) +
  geom_point(peak5, mapping = aes(x, y), col = "red")

# peak function 6 --------------------------------------------------------------
ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5)+
  stat_peaks(data_day, mapping = aes(row_id, filter1), col = "red", span =51) +
  stat_valleys(data_day, mapping = aes(row_id, filter1), col = "blue", span =51)

# peak function 7 --------------------------------------------------------------
argmax <- function(x, y, w=1, ...) {
  n <- length(y)
  y.smooth <- loess(y ~ x, ...)$fitted
  y.max <- zoo::rollapply(zoo::zoo(y.smooth), 2*w+1, max, align="center")
  delta <- y.max - y.smooth[-c(1:w, n+1-1:w)]
  i.max <- which(delta <= 0) + w
  list(x=x[i.max], i=i.max, y.hat=y.smooth)
}

peak7 <- argmax(data_day$row_id, data_day$filter1, w = 10, span = 0.005)[["x"]]

peak7 <- data_day %>% 
  dplyr::filter(row_id %in% peak7)

ggplot() +
  geom_line(data_day, mapping = aes(row_id, amount), col = "gray") +
  geom_line(data_day, mapping = aes(row_id, filter1), col = "red", alpha = 0.5) +
  geom_point(peak7, mapping = aes(row_id, filter1), col = "red")
