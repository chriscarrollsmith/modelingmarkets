#Chart historical volume by security price to identify likely support & resistance levels

# for tidy data analysis
library(tidyverse)
library(tidyquant)
library(mgcv)
options(scipen = 999)

#choose a ticker
ticker <- "AAPL"

#pull 4 years of price data  for ticker
df = tq_get(ticker, from = Sys.Date() %m-% years(4), to = Sys.Date())

#Convert everything to integers to avoid floating point math errors
df <- df %>%
  mutate(adjusted_high = round(high*(adjusted/close),digits=2)*100,
         adjusted_low = round(low*(adjusted/close),digits=2)*100)

#Create a data frame for the full range of $ values this stock has taken
range <- min(df$adjusted_low):max(df$adjusted_high)
volume_profile <- data.frame(cents = range,
                             volume = rep(0))

#For every trading day, divide volume evenly across the price range for that day
for(val in 1:nrow(df)){
  day_range <- df$adjusted_low[val]:df$adjusted_high[val]
  vol_per_cent <- df$volume[val]/length(day_range)
  volume_profile$volume[volume_profile$cents %in% day_range] <- 
    volume_profile$volume[volume_profile$cents %in% day_range] + vol_per_cent
}

#Use the function that underlies geom_smooth to get y values for smooth model line
model <- gam(volume ~ s(cents,bs="cs"), data = volume_profile)
volume_profile <- volume_profile %>% 
  mutate(smooth = predict(model))

#Use a Monte Carlo simulation to search for local maxima of y within random 
#ranges for x. Disallow maxima that occur at ends of range.
maximum <- replicate(100,{
  x_vals <- sample(volume_profile$cents,size=2,replace=FALSE)
  max_loc <- volume_profile %>%
    filter(cents >= min(x_vals) & cents <= max(x_vals)) %>%
    filter(smooth == max(smooth)) %>%
    pull(cents)
  if(max_loc == min(x_vals)|max_loc == max(x_vals)){NA}else{max_loc}
})
unique_maxima <- unique(maximum[!is.na(maximum)])

#Convert everything back from integers to dollars and cents
volume_profile <- volume_profile %>%
  mutate(cents = cents/100,
         volume = round(volume))
unique_maxima <- unique_maxima/100
model <- gam(volume ~ s(cents,bs="cs"), data = volume_profile)
volume_profile <- volume_profile %>% 
  mutate(smooth = predict(model))

#Create density plot and overlay smoothed model and vertical lines at maxima
volume_profile %>% 
  ggplot() +
  geom_density(aes(x=cents,y=volume),stat="identity") +
  geom_line(aes(x=cents,y=smooth)) +
  geom_vline(xintercept = unique_maxima,col="Red") +
  labs(title = paste(ticker,"historical volume by split-adjusted stock price"), 
       x = paste(ticker,"split-adjusted stock price"),
       y = "Historical volume located at price",
       caption = paste("Price and volume data courtesy of Yahoo Finance",sep="")) +
  scale_x_continuous(breaks = round(seq(min(volume_profile$cents),max(volume_profile$cents),(max(volume_profile$cents)-min(volume_profile$cents))/10))) +
  theme_bw()

#Export chart as image file
ggsave(
  filename = "historicalvolume.jpg",
  plot = last_plot(),
  path = "", #Insert desired file path here
  scale = 1,
  width = 1920/200,
  height = 1080/200,
  units = "in"
)
