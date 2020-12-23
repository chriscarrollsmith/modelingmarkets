# for tidy data analysis
library(tidyverse)
library(tidyquant)
library(mgcv)

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

#Convert everything back from integers to dollars and cents
volume_profile <- volume_profile %>%
  mutate(cents = cents/100,
         volume = round(volume))

#Use the function that underlies geom_smooth and find price at which it hits maximum
model <- gam(volume ~ s(cents,bs="cs"), data = volume_profile)
volume_profile <- volume_profile %>% 
  mutate(smooth = predict(model))
maximum <- volume_profile %>%
  filter(smooth == max(smooth)) %>%
  pull(cents)

#Create density plot and overlay smoothed model and vertical line at maximum
volume_profile %>% 
  ggplot() +
  geom_density(aes(x=cents,y=volume),stat="identity") +
  geom_line(aes(x=cents,y=smooth)) +
  geom_vline(xintercept = maximum,col="Red")

print(paste("The strongest volume support node is located at $",maximum,sep=""))
