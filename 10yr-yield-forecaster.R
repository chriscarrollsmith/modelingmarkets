
#Get and clean high-yield spread data
download.file(url,"highyieldspread.csv")
yieldspread <- read.csv("highyieldspread.csv")
unlink("highyieldspread.csv")
yieldspread <- yieldspread %>%
  select(date=DATE,spread=BAMLH0A0HYM2) %>%
  mutate(date=as.Date(date),spread=as.numeric(spread)) %>%
  filter(!is.na(spread))

#Get S&P 500 price data and calculate next period percent returns
sp500 <- tq_get("VOO")
sp500$return_6_mo <- c(diff(sp500$adjusted,lag = round(253*.5)),rep(NA,times=round(253*.5)))/sp500$adjusted
sp500$return_1_yr <- c(diff(sp500$adjusted,lag = 253),rep(NA,times=253))/sp500$adjusted
sp500$return_2_yr <- c(diff(sp500$adjusted,lag = 253*2),rep(NA,times=253*2))/sp500$adjusted
sp500$return_3_yr <- c(diff(sp500$adjusted,lag = 253*3),rep(NA,times=253*3))/sp500$adjusted

#Join returns data with bond yield spread data
sp500 <- sp500 %>%
  select(date,return_6_mo,return_1_yr,return_2_yr,return_3_yr)
yieldspread <- inner_join(yieldspread,sp500,by="date")

#Calculate correlation coefficient between high-yield spread and next-year return
data.frame(pearsons_6mo = cor(yieldspread$spread,yieldspread$return_6_mo,use="complete.obs"),
           pearsons_1yr = cor(yieldspread$spread,yieldspread$return_1_yr,use="complete.obs"),
           pearsons_2yr = cor(yieldspread$spread,yieldspread$return_2_yr,use="complete.obs"),
           pearsons_3yr = cor(yieldspread$spread,yieldspread$return_3_yr,use="complete.obs"))

#Forecast current expected 1-yr return based on high-yield spread
#Plot high-yield spread vs. 1-year return
model_1yr <- lm(yieldspread,formula = return_1_yr ~ spread)
model_2yr <- lm(yieldspread,formula = return_2_yr ~ spread)
yieldspread <- yieldspread %>%
  mutate(forecast_1yr = predict(object=model_1yr,newdata=.),
         forecast_2yr = predict(object=model_2yr,newdata=.))
label = paste(scales::percent(round(last(yieldspread$forecast_1yr),2))," expected 1-year return\nbased on current spread")
yieldspread %>%
  ggplot(aes(x=spread,y=return_1_yr)) +
  geom_point() +
  geom_line(aes(y=forecast_1yr),col="blue",lwd=1) +
  geom_point(aes(x=last(spread),y=last(forecast_1yr)),col="red",size=3) +
  geom_hline(aes(yintercept=last(forecast_1yr)),col="red",lwd=1,linetype="dashed") +
  geom_text(aes(x=last(spread),y=last(forecast_1yr),label=label),nudge_y=-.05,nudge_x=4.3,col="red",size=4) +
  labs(title="\"Buy when high-yield spreads are high\": S&P500 forward 1-year return vs. high-yield spread",
       x="High-yield spread",
       y="Next 1-year S&P 500 return",
       caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") + 
  scale_y_continuous(labels = scales::percent)

#save the plot
ggsave(filename = "hy-spread-1-year-return.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in")

#Forecast current expected 2-yr return based on high-yield spread
#Plot high-yield spread vs. 2-year return
label = paste(scales::percent(round(last(yieldspread$forecast_2yr),2))," expected 2-year return\nbased on current spread")
yieldspread %>%
  ggplot(aes(x=spread,y=return_2_yr)) +
  geom_point() +
  geom_line(aes(y=forecast_2yr),col="blue",lwd=1) +
  geom_point(aes(x=last(spread),y=last(forecast_2yr)),col="red",size=3) +
  geom_hline(aes(yintercept=last(forecast_2yr)),col="red",lwd=1,linetype="dashed") +
  geom_text(aes(x=last(spread),y=last(forecast_2yr),label=label),nudge_y=-.06,nudge_x=4.3,col="red",size=4) +
  labs(title="\"Buy when high-yield spreads are high\": S&P500 forward 1-year return vs. high-yield spread",
       x="High-yield spread",
       y="Next 2-year S&P 500 return",
       caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") + 
  scale_y_continuous(labels = scales::percent)

#save the plot
ggsave(filename = "hy-spread-2-year-return.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in")

#calculate percentile of yield spread
yieldspread <- yieldspread %>%
  mutate(percentile = rank(spread)/length(spread))
last(yieldspread$percentile)
