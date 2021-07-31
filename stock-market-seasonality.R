#Seasonality by month
library(tidyverse)
library(tidyquant)

#Select the ticker to analyze and the type of chart you want ("barchart" or "linechart")
ticker <- "XLE"
type <- "linechart"
start <- as.Date("01-01-1980",format="%m-%d-%Y")
end <- as.Date(paste("12-31-",year(Sys.Date())-1,sep = ""),format="%m-%d-%Y")

#Get Yahoo Finance price history for the ticker using tidyquant
price_data <- tq_get(ticker,from = start,to = Sys.Date())

if(type == "barchart"){

  #If chart type is barchart, find median return by month, chart it, and save it
  
  monthly_return <- price_data %>%
    filter(date <= end) %>%
    mutate(year_month = paste(year(date),month(date))) %>%
    mutate(return = c(NA,diff(adjusted))/lag(adjusted)) %>%
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
  
  monthly_return <- monthly_return %>% mutate(positive = return >0 )
  
  monthly_return %>% ggplot(aes(x=month_of_year,y=return,fill=positive)) +
    geom_col() +
    labs(title = paste(ticker," seasonality since ",month(price_data$date[1]) + 1,"/1/",year(price_data$date[1]),sep = ""),x = "Month",y = "Median return") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values=c("TRUE" = "#00BFC4", "FALSE" = "#f8766d")) +
    theme(legend.position = "none")
  
}else if(type == "linechart"){

  #If chart type is linechart, find median and mean YTD return by day of year, chart it, and save it
  
  daily_return <- price_data %>% 
    mutate(return = c(NA,diff(adjusted))/lag(adjusted)) %>%
    filter(!is.na(return))
  
  avg_daily_return <- daily_return %>%
    filter(date <= end) %>%
    mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(ytd_return = cumsum(return)) %>% 
    ungroup() %>%
    mutate(month_day = paste(month(date),day(date))) %>%
    group_by(month_day) %>% 
    summarize(median_ytd_return = median(ytd_return),
              mean_ytd_return = mean(ytd_return)) %>%
    separate(month_day,c("month","day")) %>%
    mutate(month = as.numeric(month),day = as.numeric(day)) %>%
    arrange(month,day)
  
  avg_daily_return <- avg_daily_return %>%
    mutate(date = as.Date(paste(month,day,"2021"),"%m %d %Y"))
  
  daily_return <- daily_return %>% filter(date > end) %>%
    mutate(ytd_return = cumsum(return)) %>%
    select(date,actual_ytd_return = ytd_return)
  
  avg_daily_return <- full_join(avg_daily_return,daily_return) %>%
    select(-month,-day)
  
  avg_daily_return <- bind_rows(avg_daily_return,data.frame(date = end,mean_ytd_return = 0,median_ytd_return = 0,actual_ytd_return = 0)) %>%
    arrange(date) %>% mutate(actual_ytd_return = c(na.approx(actual_ytd_return,na.rm = FALSE)))
  
  avg_daily_return <- avg_daily_return %>% gather("type","return",median_ytd_return,mean_ytd_return,actual_ytd_return)
  
  avg_daily_return %>% ggplot(aes(x=date,y=return,col=type)) +
    geom_line(alpha = 0.5) +
    geom_ma(ma_fun = SMA, n = 7,linetype="solid",size=1.5) + 
    #geom_vline(xintercept = c(as.Date("03/22/2021",format="%m/%d/%Y"),as.Date("06/22/2021",format="%m/%d/%Y"),as.Date("09/22/2021",format="%m/%d/%Y"),as.Date("12/22/2021",format="%m/%d/%Y"))) +
    #uncomment the previous line and adjust dates to indicate ex-dividend dates as vertical lines
    labs(title = paste(ticker," seasonality since ",month(price_data$date[1]) + 1,"/1/",year(price_data$date[1]),sep = ""),x = "date",y = "Mean/median YTD return") + 
    scale_y_continuous(labels = scales::percent) +
    scale_x_date(labels = scales::date_format("%b"),breaks = "1 month") +
    labs(caption = paste(
      #"Vertical lines represent",ticker,"dividend dates.",
      "Bold lines represent the 7-day simple moving average of YTD return.\n
      Data courtesy of Yahoo! Finance. Copyright Wall Street Petting Zoo, 2021.",
      sep=""
      ))
  
}else{print("Invalid chart type")}

ggsave(filename = paste(ticker,"seasonality",type,".jpg",sep=""),
       path = "",
       scale = 1,
       width = 1920/220,
       height = 1080/220,
       units = "in")
