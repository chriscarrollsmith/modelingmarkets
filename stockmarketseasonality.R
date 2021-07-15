library(tidyverse)
library(tidyquant)

ticker <- "IWM"
start <- as.Date("01-01-1980",format="%m-%d-%Y")
end <- as.Date("12-31-2020",format="%m-%d-%Y")

price_data <- tq_get(ticker,from = start,to = end)

monthly_return <- price_data %>% 
  mutate(year_month = paste(year(date),month(date))) %>% 
  mutate(return = c(NA,diff(adjusted))) %>% 
  group_by(year_month) %>% 
  summarize(return = sum(return)) %>% 
  filter(!is.na(return)) %>%
  separate(year_month,c("year","month")) %>% 
  group_by(month) %>% 
  summarize(return = median(return)) %>%
  mutate(month = as.numeric(month)) %>%
  arrange(month) %>%
  mutate(month_of_year = c("January","February","March","April","May","June","July","August","September","October","November","December"))

monthly_return$month_of_year <- factor(monthly_return$month_of_year,levels = monthly_return$month_of_year[order(monthly_return$month)])

#monthly_return <- monthly_return %>% mutate(curr_month = (month %in% c(month(Sys.Date()),month(Sys.Date())+1)))
monthly_return <- monthly_return %>% mutate(positive = return >0 )

monthly_return %>% ggplot(aes(x=month_of_year,y=return,fill=positive)) +
  geom_col() + labs(title = paste(ticker," seasonality since ",month(price_data$date[1]) + 1,"/1/",year(price_data$date[1]),sep = ""),x = "Month",y = "Median return") + theme(legend.position = "none")

ggsave(filename = paste(ticker,"seasonality.jpg",sep=""),
       scale = 1,
       width = 1920/300,
       height = 1080/300,
       units = "in")
