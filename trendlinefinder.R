#Find and draw trend lines above and below security price structure

library(tidyverse)
library(tidyquant)

# Here, select your ticker and timeframe
ticker <- "TSLA"
start <- Sys.Date() %m-% years(2)
  
# Get split-adjusted price data from Yahoo! Finance and rond to nearest two digits
prices <- tq_get(ticker, from = start) %>%
  mutate(open = round(open,digits=2),
         high = round(high,digits=2),
         low = round(low,digits=2),
         close = round(close,digits=2)) %>%
  select(symbol,date,open,high,low,close)

# Filter prices data for lows that are below the 10-day simple moving average (SMA)
lows <- prices %>%
  filter(low < SMA(close),date<max(date))
# Filter prices data for highs that are above the 10-day SMA
highs <- prices %>%
  filter(high > SMA(close),date<max(date))

# Find all unique possible combinations of two lows or two highs
# (and all unique possible combinations of their associated dates)
# and rename columns in the resulting data frame
all_lowcombos <- bind_cols(as.data.frame(t(combn(lows$date,m=2,simplify=TRUE))),as.data.frame(t(combn(lows$low,m=2,simplify=TRUE))))
colnames(all_lowcombos) <- c("X1","X2","Y1","Y2")
all_highcombos <- bind_cols(as.data.frame(t(combn(highs$date,m=2,simplify=TRUE))),as.data.frame(t(combn(highs$high,m=2,simplify=TRUE))))
colnames(all_highcombos) <- c("X1","X2","Y1","Y2")

# Generate a trendline for every unique combination of two points
# and return its intercept and slope.
# Intercept of a trendline: lm(ys~xs)$coefficients[1]
# Slope of a trendline: lm(ys~xs)$coefficients[2]
n <- seq(1:nrow(all_lowcombos))
low_trendfinder <- function(n,all_lowcombos){
  model <- lm(c(all_lowcombos$Y1[n],all_lowcombos$Y2[n])~c(all_lowcombos$X1[n],all_lowcombos$X2[n]))
  data.frame(intercept = model$coefficients[1],slope = model$coefficients[2])
}
low_trendlines <- map_dfr(n,low_trendfinder,all_lowcombos = all_lowcombos)

n <- seq(1:nrow(all_highcombos))
high_trendfinder <- function(n,all_highcombos){
  model <- lm(c(all_highcombos$Y1[n],all_highcombos$Y2[n])~c(all_highcombos$X1[n],all_highcombos$X2[n]))
  data.frame(intercept = model$coefficients[1],slope = model$coefficients[2])
}
high_trendlines <- map_dfr(n,high_trendfinder,all_highcombos = all_highcombos)

# For each low_trendline, check if any low in the prices dataframe falls below the line
# Keep only trendlines for which this is false
# Also make sure the trendline wouldn't be below half the current price for today's date
low_trendline_test <- function(x,y,prices){
  !any(x*as.numeric(prices$date) + y > prices$low + 0.01) & !(x*as.numeric(Sys.Date())+y < 0.5*prices$close[nrow(prices)])
}
none_below <- map2(.x = low_trendlines$slope,.y = low_trendlines$intercept,.f = low_trendline_test,prices = prices)
none_below <- unlist(none_below)
low_trendlines <- low_trendlines[none_below,]

# For each high_trendline, check if any high in the prices dataframe falls above the line
# Also discard any greater than 1.5x the current price for today's date
high_trendline_test <- function(x,y,prices){
  !any(x*as.numeric(prices$date) + y < prices$high - 0.01) & !(x*as.numeric(Sys.Date())+y > 1.5*prices$close[nrow(prices)])
}
none_above <- map2(.x = high_trendlines$slope,.y = high_trendlines$intercept,.f = high_trendline_test,prices = prices)
none_above <- unlist(none_above)
high_trendlines <- high_trendlines[none_above,]

# Chart all successful trendlines
prices %>%
  ggplot(aes(x = date, y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  #geom_ma(fun=SMA,n=10,color = "darkblue", linetype = 1, size = 1) +
  geom_abline(intercept=low_trendlines$intercept,slope=low_trendlines$slope) +
  geom_abline(intercept=high_trendlines$intercept,slope=high_trendlines$slope) +
  labs(title = paste(ticker,"Trendline Chart"), y = "Price", x = "Date", caption = paste("Price data courtesy of Yahoo! Finance. Accessed ",Sys.Date(),".",sep="")) +
  #coord_x_date(xlim = c(Sys.Date() %m-% months(1), Sys.Date())) +
  theme_tq()

#Export chart as image file
ggsave(
  filename = "trendlines.jpg",
  plot = last_plot(),
  path = "", #Insert your preferred file path here
  scale = 1,
  width = 1920/300,
  height = 1080/300,
  units = "in"
)
