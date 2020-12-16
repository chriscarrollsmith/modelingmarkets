#Script to assess statistical significance of "down Friday, down Monday" indicator

library(tidyverse)
# tidy financial analysis 
library(tidyquant)
# tidy data cleaning functions

#Get y years of daily close price data for SPY from Yahoo Finance
#Value of y can be changed to test robustness against different sample sizes
y <- 10
get_prices <- tq_get("SPY",from=(Sys.Date()-1)-365*y,to=(Sys.Date()-1)) %>%
  mutate(weekday = weekdays(date)) 

get_prices <- get_prices %>%
  mutate(change = c(NA,adjusted[-1] - adjusted[-nrow(get_prices)])) %>%
  select(symbol, date, adjusted, weekday, change)

#Create data frame of day-pairs with logical variable for DF_DM indicator
day_pairs <- get_prices %>%
  mutate(date2 = c(date[-1],NA), adjusted2 = c(adjusted[-1],NA), weekday2 = c(weekday[-1],NA), change2 = c(change[-1],NA)) %>%
  filter(!is.na(change) & !is.na(change2)) %>%
  mutate(DF_DM = weekday == "Friday" & weekday2 == "Monday" & change < 0 & change2 < 0)

#Define function to calculate mean momentum change, n days pre vs. n days post
robustness_check <- function(n,day_pairs){
 #Create variables for n-day change preceding and succeeding the day-pair
 day_pairs <- day_pairs %>%
   mutate(pre = c(rep(NA,times=n),adjusted[-c(1:(n-1),nrow(day_pairs))] - adjusted[-c((nrow(day_pairs)-(n-1)):nrow(day_pairs))]),post = c(adjusted2[-c(1:n)] - adjusted2[-c(1,(nrow(day_pairs)-(n-2)):nrow(day_pairs))],rep(NA,times=n))) %>%
   filter(!is.na(pre) & !is.na(post))
 #Create variable for pre-post change of direction for all day pairs
 day_pairs <- day_pairs %>%
   mutate(mom_change = abs(post - pre))
 #Summarize average change of direction for DF_DM and non-DF_DM day pairs
 day_pairs %>%
   group_by(DF_DM) %>%
   summarize(n,mean = mean(mom_change))
}
#Create sequence of different values of n and run robustness check function
robustness_results <- map_dfr(.x = seq(from = 3,to = 15,by = 2),.f = robustness_check,day_pairs = day_pairs,n = )

#For each value of n, subtract DF_DM mean momentum change from non-DF_DM
results_summary <- robustness_results %>%
  group_by(n) %>%
  summarize(effect_size = mean[2] - mean[1])

#Print results table
results_summary

#Plot results table
results_summary %>%
  ggplot(aes(x = n,y = effect_size)) +
  geom_point()
