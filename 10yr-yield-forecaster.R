#Script to predict 10-year yields from PCE inflation rate, factoring in how 
#10-year yields have fallen over time

library(tidyverse)
library(xlsx)
library(lubridate)
library(RSelenium)

#Start Selenium driver and navigate to FRED series for 10-year yields
rD <- rsDriver(browser = "firefox")
driver <- rD[["client"]]
driver$navigate("https://fred.stlouisfed.org/series/DGS10")
Sys.sleep(2)

#Find "Max" button on the page and click it
element <- driver$findElement(using = "xpath",'//*[@id="zoom-all"]')
element$clickElement()
Sys.sleep(1)

#Find "Download" button on the page and click it
element <- driver$findElement(using = "xpath",'/html/body/div[2]/div[1]/div/div[1]/h1/div/span/div/button/span')
element$clickElement()
Sys.sleep(1)

#Find "CSV" button on the page and get the link from it
element <- driver$findElement(using = "xpath",'//*[@id="download-data-csv"]')
url <- unlist(element$getElementAttribute('href'))

#Get and clean 10-year yield data
download.file(url,"DGS10.csv")
tenyr_df <- read.csv("DGS10.csv") %>%
  mutate(yield = as.numeric(DGS10)) %>%
  filter(!is.na(yield))
tenyr_df <- tenyr_df %>% 
  mutate(year = year(DATE),month = month(DATE)) %>%
  group_by(year,month) %>%
  summarize(mean_yield = mean(yield))
unlink("DGS10.csv")

#Navigate to FRED series for PCE inflation rate
driver$navigate("https://fred.stlouisfed.org/series/PCETRIM12M159SFRBDAL")
Sys.sleep(2)

#Find "Max" button on the page and click it
element <- driver$findElement(using = "xpath",'//*[@id="zoom-all"]')
element$clickElement()
Sys.sleep(1)

#Find "Download" button on the page and click it
element <- driver$findElement(using = "xpath",'/html/body/div[2]/div[1]/div/div[1]/h1/div/span/div/button/span')
element$clickElement()
Sys.sleep(1)

#Find "CSV" button on the page and get the link from it
element <- driver$findElement(using = "xpath",'//*[@id="download-data-csv"]')
url <- unlist(element$getElementAttribute('href'))

#Get and clean PCE inflation dataset
download.file(url,"PCETRIM12M159SFRBDAL.csv")
pce_df <- read.csv("PCETRIM12M159SFRBDAL.csv") 
pce_df <- pce_df %>%
  mutate(year = year(DATE),month = month(DATE)) %>%
  mutate(twelve_month_pce = as.numeric(PCETRIM12M159SFRBDAL)) %>%
  filter(!is.na(twelve_month_pce)) %>%
  select(-DATE,-PCETRIM12M159SFRBDAL)
unlink("PCETRIM12M159SFRBDAL.csv")

#Join the two tables
joined_df <- inner_join(tenyr_df,pce_df) %>% 
  ungroup() %>%
  mutate(date = as.Date(paste(as.character(year),as.character(month),"1",sep="/"),format="%Y/%m/%d"),
         sequence = row_number())

#Predict 10-year yield based on this simple log regression model using PCE rate
fit <- joined_df %>% lm(mean_yield ~ log(twelve_month_pce), data = .)
predicted_yield <- predict(fit,newdata=data.frame(twelve_month_pce = last(joined_df$twelve_month_pce)))

#Plot PCE rate against 10-year yield, with date shown as color
joined_df %>%
  ggplot(aes(x=twelve_month_pce,y=mean_yield,col=date)) +
  geom_point() +
  geom_smooth(method = "lm",formula = y~log(x)) +
  geom_point(data=data.frame(twelve_month_pce = last(joined_df$twelve_month_pce),mean_yield = last(joined_df$mean_yield),date = last(joined_df$date)),color="red") +
  scale_x_continuous(breaks = seq(0,10,by = .5)) + 
  scale_y_continuous(limits=c(0,16),breaks = seq(0,16.25,by = 1.25)) +
  labs(x="TTM PCE Inflation Rate",y="Monthly Average 10-Year Yield",title="10-Year Yield by PCE Inflation Rate",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "pce-yield-log-regression.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in")

#Plot yield vs. date
joined_df %>% 
  mutate(twelve_month_pce_strat = factor(round(twelve_month_pce))) %>%
  ggplot(aes(x=date,y=mean_yield)) +
  geom_point() +
  geom_smooth(method="lm",formula=y~log(x)) +
  labs(x="Date",y="Monthly Average 10-Year Yield",title="10-Year Yield over Time",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "yield-over-time.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in")

#Stratify by PCE inflation rate and plot yield vs. date
joined_df %>% 
  mutate(twelve_month_pce_strat = factor(round(twelve_month_pce))) %>%
  group_by(twelve_month_pce_strat) %>%
  mutate(n=n()) %>%
  filter(n>20) %>%
  ggplot(aes(x=date,y=mean_yield)) +
  geom_point() +
  geom_smooth(method="lm",formula=y~log(x)) +
  facet_wrap( ~ twelve_month_pce_strat) +
  labs(x="Date",y="Monthly Average 10-Year Yield",title="10-Year Yield over Time, Stratified by PCE Inflation Rate",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "pce-yield-over-time.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in")

#Subtract PCE inflation model from the data and regress for time effect
joined_df %>% 
  mutate(pce_model = predict(lm(mean_yield ~ log(twelve_month_pce), data = .)),
         mean_yield_residual = mean_yield - pce_model) %>%
  ggplot(aes(x=date,y=mean_yield_residual)) +
  geom_point() +
  geom_smooth(method="lm",formula=y~x) +
  labs(x="Date",y="Residual Monthly Average 10-Year Yield",title="Residual 10-Year Yield over Time after Subtracting PCE Inflation-Rate Log Regression Model",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "residual-yield-over-time.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in")

#Do a multiple linear regression accounting for both PCE inflation and date
joined_df %>%
  do(tidy(lm(mean_yield ~ sequence + log(twelve_month_pce), data = .)))
fit <- joined_df %>% lm(mean_yield ~ log(sequence) + log(twelve_month_pce), data = .)

#Predict the model for rates today
df <- data.frame(twelve_month_pce = c(seq(1,8.5,by=0.5),last(joined_df$twelve_month_pce)),sequence=rep(530,times=17)) %>%
  mutate(predicted_yield = predict(fit,newdata = .)) %>%
  mutate(highlight = twelve_month_pce %in% last(joined_df$twelve_month_pce))
predicted_yield <- predict(fit,newdata=data.frame(twelve_month_pce = last(joined_df$twelve_month_pce),sequence = last(joined_df$sequence)))

#Plot the rate prediction curve for 2022
df %>% 
  ggplot(aes(x=twelve_month_pce,y=predicted_yield,col=highlight)) +
  geom_point(size=2,show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  scale_x_continuous(breaks=seq(0,10,by=.5)) + 
  labs(x="TTM PCE Inflation Rate",y="Monthly Average 10-Year Yield",title="Predicted 2022 10-Year Yield by PCE Inflation Rate",caption="Copyright Wall Street Petting Zoo 2022")

#save the plot
ggsave(filename = "2022-rate-prediction-curve.png",
       scale = 1,
       width = 1920/200,
       height = 1080/200,
       units = "in")