library(tidyverse)
library(tidyquant)
options(digits=2)

ticker <- c("EWRE","RCD","RTM","RYE","RYF","RHS","RYH","RYU","XAR","XBI","XHB","XME","XTL","XTN","XSW","XSD")
start <- as.Date("01-01-1980",format="%m-%d-%Y")
end <- as.Date("12-31-2020",format="%m-%d-%Y")

seasonality_finder <- function(ticker){
  price_data <- tq_get(ticker,from = start,to = end)

  monthly_return <- price_data %>% 
    mutate(year_month = paste(year(date),month(date))) %>% 
    mutate(previous_close = lag(adjusted)) %>%
    mutate(return = c(NA,diff(adjusted))/previous_close) %>%
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
  
  monthly_return <- monthly_return %>% mutate(ticker = ticker,positive = return >0) %>% select(ticker,month,month_of_year,return,positive)
}

tidy_seasonality <- map_dfr(.x=ticker,.f=seasonality_finder)
best_of_month <- tidy_seasonality %>% group_by(month_of_year) %>% summarize(best_ticker = ticker[which.max(return)],return = max(return))

best_of_month %>% ggplot(aes(x=month_of_year,y=return,fill=return)) +
  geom_col() +
  labs(title = "Best-performing (equal-weight) sectors by month of year",x = "Month",y = "Best-performing sector's median return") +
  geom_text(aes(label = best_ticker), vjust = -0.5) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic()

ggsave(filename = "sectorseasonality.jpg",
       path = "C:/Users/chris/OneDrive/Pictures/Infographics",
       scale = 1,
       width = (1920*1.5)/300,
       height = (1080*1.5)/300,
       units = "in")
