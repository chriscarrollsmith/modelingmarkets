# Script to predict market returns and sharpe ratios across various asset classes



# Load Libraries ----------------------------------------------------------



#Load libraries
library(tidyverse)
library(xlsx)
library(lubridate)
library(fredr)
library(tidyquant)
library(caret)
library(rvest)
library(gtools)
library(scales)
library(knitr)
library(janitor)

# Set FRED API key from .Renviron file
# NOTE: The script will only work if you have a valid FRED API key in .Renviron!
readRenviron("~/.Renviron")



# Define functions --------------------------------------------------------



#Define function to split time series into k parts
splitTimeSeries <- function(object,k){
  not_na <- which(!is.na(object))
  quotient <- length(not_na) %/% k
  remainder <- length(not_na) %% k
  segment_lengths <- rep(quotient,times=k)
  extras <- sample(1:k,remainder,replace=F)
  segment_lengths[extras] <- segment_lengths[extras] + 1
  indexes <- map(1:k,function(x){
    if(x==1){
      not_na[1:segment_lengths[1]]
    }else{
      not_na[(1+sum(segment_lengths[1:(x-1)])):sum(segment_lengths[1:x])]
    }
  })
  if(sum(unlist(map(1:k,function(x){length(indexes[[x]])}))) == length(not_na)){print("Success")}else{print("Error! Debug splitTimeSeries function!")}
  return(indexes)
}

#Define function to get root mean squared error of predictions
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings[!is.na(true_ratings) & !is.na(predicted_ratings)] - predicted_ratings[!is.na(true_ratings) & !is.na(predicted_ratings)])^2))
}

#Define function to get mean squared error of predictions
MSE <- function(true_ratings, predicted_ratings){
  mean((true_ratings[!is.na(true_ratings) & !is.na(predicted_ratings)] - predicted_ratings[!is.na(true_ratings) & !is.na(predicted_ratings)])^2)
}

#Define function to get standard deviation of squared error of predictions
SDSE <- function(true_ratings, predicted_ratings){
  sd((true_ratings[!is.na(true_ratings) & !is.na(predicted_ratings)] - predicted_ratings[!is.na(true_ratings) & !is.na(predicted_ratings)])^2)
}

#Select which metric to optimize for in tuning models
#Rather than tune based on RMSE, I probably should tune based on sd of squared
#error because neighboring data points are correlated and thus systematically
#biased relative to my model. Also, should I be using sd of error, or sd of SE?
#SE might be best, because higher magnitude errors will have wider spread (and
#thus we are measuring accuracy as well as dispersion).
#However, if I were to use SDSE, then I would want to change how I generate my models,
#so that they also use SDSE.
global_optimizer <- list(RMSE,"RMSE")

#Define function to calculate corr coeff between selected variables and returns
corchecker <- function(df = yieldspread_full,ticker = c("VOO"),vars_to_test = c("us_spread")){
  df <- df %>%
    filter(symbol %in% ticker)
  df1 <- map_dfr(c("return_6_mo","return_1_yr","return_2_yr","return_3_yr"),function(returns_to_check){
    data.frame(cor(subset(df,select=returns_to_check),subset(df,select=vars_to_test),use="complete.obs"))
  })
  df2 <- df1 %>% lapply(abs) %>% sapply(mean) %>% t() %>% data.frame()
  row.names(df2) <- "avg_of_abs_vals"
  bind_rows(df1,df2) %>%
    t() %>%
    data.frame() %>%
    arrange(desc(avg_of_abs_vals))
}

#Define a function to use five-fold cross-validation to compare models
cross_validator <- function(variables_to_study){

  #Split time series and create indexes for five-fold cross-validation
  test_index_0 <- splitTimeSeries(yieldspread$return_6_mo,k=5)
  test_index_1 <- splitTimeSeries(yieldspread$return_1_yr,k=5)
  test_index_2 <- splitTimeSeries(yieldspread$return_2_yr,k=5)
  test_index_3 <- splitTimeSeries(yieldspread$return_3_yr,k=5)

  #Find all unique combinations of up to two of the variables we're testing
  #(For combinations larger than one, use the vector 1:length(varlist) for x.)
  varcombos <- map(1:length(varlist),function(x){
    asplit(combinations(n = length(varlist),r = x,v = varlist,repeats.allowed = F),MARGIN = 1)
  }) %>% unlist(recursive=F)

  #Perform five-fold cross-validation and calculate errors
  errors <- map_dfr(.x = 1:5,optimizer = global_optimizer[[1]],.f = function(x,optimizer){

    #Partition data using the indexes we generated
    train_set_0 <- yieldspread[-test_index_0[[x]],]
    train_set_1 <- yieldspread[-test_index_1[[x]],]
    train_set_2 <- yieldspread[-test_index_2[[x]],]
    train_set_3 <- yieldspread[-test_index_3[[x]],]
    test_set_0 <- yieldspread[test_index_0[[x]],]
    test_set_1 <- yieldspread[test_index_1[[x]],]
    test_set_2 <- yieldspread[test_index_2[[x]],]
    test_set_3 <- yieldspread[test_index_3[[x]],]

    #Omit from training set any returns that overlap the first date in test set
    suppressWarnings(train_set_0$return_6_mo <- case_when(between(test_set_0$date[1] - train_set_0$date,0,252*0.5)~NA_real_,
                                           T~train_set_0$return_6_mo))
    suppressWarnings(train_set_1$return_1_yr <- case_when(between(test_set_1$date[1] - train_set_1$date,0,252*1)~NA_real_,
                                           T~train_set_1$return_1_yr))
    suppressWarnings(train_set_2$return_2_yr <- case_when(between(test_set_2$date[1] - train_set_2$date,0,252*2)~NA_real_,
                                           T~train_set_2$return_2_yr))
    suppressWarnings(train_set_3$return_3_yr <- case_when(between(test_set_3$date[1] - train_set_3$date,0,252*3)~NA_real_,
                                           T~train_set_3$return_3_yr))

    #Create models on training data, predict on test data, and calculate error
    map_dfr(1:length(varcombos),function(y){
      data.frame(model = paste(varcombos[[y]],collapse="+"),
                 trial = x,
                 error_measure = optimizer(c(test_set_0$return_6_mo,
                                             test_set_1$return_1_yr,
                                             test_set_2$return_2_yr,
                                             test_set_3$return_3_yr),
                                           c(predict(lm(formula = paste0("return_6_mo ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_0),test_set_0),
                                             predict(lm(formula = paste0("return_1_yr ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_1),test_set_1),
                                             predict(lm(formula = paste0("return_2_yr ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_2),test_set_2),
                                             predict(lm(formula = paste0("return_3_yr ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_3),test_set_3))))
    }) %>% bind_rows(.,data.frame(model = "naive_average",
                                  trial = x,
                                  error_measure = optimizer(
                                    c(test_set_0$return_6_mo,
                                      test_set_1$return_1_yr,
                                      test_set_2$return_2_yr,
                                      test_set_3$return_3_yr),
                                    c(rep(mean(train_set_0$return_6_mo,na.rm=T),length(test_set_0$return_6_mo)),
                                      rep(mean(train_set_1$return_1_yr,na.rm=T),length(test_set_1$return_1_yr)),
                                      rep(mean(train_set_2$return_2_yr,na.rm=T),length(test_set_2$return_2_yr)),
                                      rep(mean(train_set_3$return_3_yr,na.rm=T),length(test_set_3$return_3_yr)))
                                  )))
  })
}

#Define function to forecast models on either yieldspread or test_set and convert
#to tidy data frame for charting
tidify <- function(df){
  df[,c("date",
        "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
        varlist)] %>%
    gather("timeframe","value",return_6_mo,return_1_yr,return_2_yr,return_3_yr) %>%
    mutate(naive_forecast = case_when(timeframe == "return_6_mo" ~ mean(yieldspread$return_6_mo,na.rm=T),
                                      timeframe == "return_1_yr" ~ mean(yieldspread$return_1_yr,na.rm=T),
                                      timeframe == "return_2_yr" ~ mean(yieldspread$return_2_yr,na.rm=T),
                                      timeframe == "return_3_yr" ~ mean(yieldspread$return_3_yr,na.rm=T),
                                      TRUE ~ NA_real_)) %>%
    mutate(forecast = case_when(timeframe == "return_6_mo" ~ predict(object=model_6mo,newdata=.),
                                timeframe == "return_1_yr" ~ predict(object=model_1yr,newdata=.),
                                timeframe == "return_2_yr" ~ predict(object=model_2yr,newdata=.),
                                timeframe == "return_3_yr" ~ predict(object=model_3yr,newdata=.),
                                TRUE ~ NA_real_)) %>%
    filter(!is.na(forecast)) %>%
    mutate(label = case_when(date == max(date) & timeframe == "return_6_mo" ~ paste(scales::percent(round(forecast,2),accuracy=1),"expected\n6-month ARR"),
                             date == max(date) & timeframe == "return_1_yr" ~ paste(scales::percent(round(forecast,2),accuracy=1),"expected\n1-year ARR"),
                             date == max(date) & timeframe == "return_2_yr" ~ paste(scales::percent(round(forecast,2),accuracy=1),"expected\n2-year ARR"),
                             date == max(date) & timeframe == "return_3_yr" ~ paste(scales::percent(round(forecast,2),accuracy=1),"expected\n3-year ARR"),
                             TRUE ~ NA_character_)) %>%
    mutate(timeframe = case_when(timeframe == "return_6_mo" ~ "6-month forward return",
                                 timeframe == "return_1_yr" ~ "1-year forward return",
                                 timeframe == "return_2_yr" ~ "2-year forward return",
                                 timeframe == "return_3_yr" ~ "3-year forward return",
                                 TRUE ~ NA_character_)) %>%
    mutate(timeframe = factor(timeframe,levels=c("6-month forward return",
                                                 "1-year forward return",
                                                 "2-year forward return",
                                                 "3-year forward return")))
}

#Define function to chart model forecast against a selected focus variable, holding
#other model inputs steady at the last observed value. User specifies whether
#chart is using test_set or yieldspread (in-sample or out-of-sample data).
chartit <- function(df=test_set,insample=F){

  #Prep new_data, using today's value for forecasting non-focus variables but
  #historical values for forecasting the focus variable
  new_data <- df %>%
    .[varlist]
  for(x in 1:length(new_data)){
    if(names(new_data[,x]) != focus_var){new_data[,x] <- last(new_data[!is.na(new_data[,x]),x])}
  }

  #Chart forward actual and modeled returns against focus variable
  df %>%
    mutate(adj_forecast = case_when(timeframe == "6-month forward return" ~ predict(object=model_6mo,newdata=new_data),
                                    timeframe == "1-year forward return" ~ predict(object=model_1yr,newdata=new_data),
                                    timeframe == "2-year forward return" ~ predict(object=model_2yr,newdata=new_data),
                                    timeframe == "3-year forward return" ~ predict(object=model_3yr,newdata=new_data),
                                    TRUE ~ NA_real_)) %>%
    mutate(value=value+(adj_forecast-forecast)) %>%
    mutate(highlight = !is.na(label)) %>%
    ggplot(aes(x=.data[[focus_var]],y=value)) +
    geom_point() +
    geom_line(aes(y=adj_forecast),col="blue",lwd=1) +
    geom_point(aes(y=adj_forecast,color=highlight,alpha=highlight),size=3) +
    geom_hline(aes(yintercept=adj_forecast,color=highlight,alpha=highlight),lwd=1,linetype="dashed") +
    geom_text(aes(y=adj_forecast,label=label),col="red",size=4,fontface = "bold",hjust="inward") +
    facet_wrap(vars(timeframe),ncol=2,nrow=2) +
    labs(title=paste0(ticker_to_study," forward annualized rate of return (ARR) vs. ",focus_var),
         subtitle = paste0("Charted against ",
                     case_when(insample==F~"out-of-sample data",
                               TRUE~"in-sample data"),
                     case_when(length(varlist)>1~paste0(", adjusted for ",paste(varlist[varlist!=focus_var],collapse="/")," effects"),
                               TRUE~"")),
         x=focus_var,
         y=paste("Forward",ticker_to_study,"ARR"),
         caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") +
    scale_color_manual(values=c("black","red")) +
    scale_y_continuous(labels = scales::percent) +
    scale_alpha_manual(values=c(0,1)) +
    theme(legend.position="none")

}

#Define function to see if our model forecast performed better than a naive average
compare_errors <- function(df = test_set){
  tmp <- df %>%
    mutate(Timeframe = timeframe) %>%
    group_by(Timeframe) %>%
    summarize(`Model Error` = global_optimizer[[1]](value,forecast),
              `Naive Average Error` = global_optimizer[[1]](value,naive_forecast))
  tmp2 <- df %>%
    summarize(`Model Error` = global_optimizer[[1]](value,forecast),
              `Naive Average Error` = global_optimizer[[1]](value,naive_forecast)) %>%
    mutate(Timeframe = "Weighted average")
  bind_rows(tmp,tmp2) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
}

#Define function to calculate standard deviations of optimized model
#(with sds from a naive-average forecast for comparison)
get_sds <- function(df = test_set){
  tmp <- df %>%
    mutate(Timeframe = timeframe) %>%
    group_by(Timeframe) %>%
    mutate(`Model` = value-forecast,
           `Naive Average` = value-naive_forecast) %>%
    summarize(`Model` = sd(`Model`,na.rm=T),
              `Naive Average` = sd(`Naive Average`,na.rm=T))
  tmp2 <- df %>%
    mutate(`Model` = value-forecast,
           `Naive Average` = value-naive_forecast) %>%
    summarize(`Model` = sd(`Model`,na.rm=T),
              `Naive Average` = sd(`Naive Average`,na.rm=T)) %>%
    mutate(Timeframe = "Weighted Model Average")
  bind_rows(tmp,tmp2) %>%
    kable(caption="Standard Deviation from Forecast") %>%
    print()
  return(tmp)
}

#Define function to calculate expected sharpe ratios from forecasted returns,
#standard deviations, and risk-free rates
get_sharpes <- function(){

  #Join sds table with risk-free rate table
  sds <- sds %>%
    select(timeframe = Timeframe,model_sd = `Model`)
  sharpe <- left_join(sds,risk_free_rate)

  #Get model forecasts for the most recent date
  forecasts <- yieldspread %>%
    filter(!is.na(forecast)) %>%
    filter(date == max(date)) %>%
    select(timeframe,forecast)

  #Join forecasts table with sds and risk-free rates and calculate expected sharpe
  #for the next period
  sharpe <- full_join(sharpe,forecasts)
  sharpe <- sharpe %>% mutate(model_sharpe = (forecast-risk_free_return) / model_sd)

  #Annualize the sharpe ratios
  sharpes <- sharpe %>%
    mutate(Ticker = ticker_to_study) %>%
    mutate(Model = model_type) %>%
    select(Ticker,
           Model,
           Timeframe = timeframe,
           `Expected return` = forecast,
           `Expected standard deviation` = model_sd,
           `Expected Sharpe` = model_sharpe)
}



# Download Yield Spread Data -------------------------------------------------



#Get and clean high-yield spread data from FRED API
yieldspread <- fredr("BAMLH0A0HYM2")
yieldspread <- yieldspread %>%
  select(date,spread=value) %>%
  mutate(spread=spread/100) %>%
  filter(!is.na(spread))

#calculate percentile of yield spread
yieldspread <- yieldspread %>%
  mutate(percentile = rank(spread)/length(spread))
last(yieldspread$percentile)

#Plot today's high-yield spread
yieldspread %>%
  ggplot(aes(x=date,y=spread)) +
  geom_line() +
  labs(title="US high-yield spread",
       x="Date",
       y="Spread",
       caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1L))

#Get FRED series for EM high-yield spread
emyieldspread <- fredr("BAMLEMHYHYLCRPIUSOAS")
emyieldspread <- emyieldspread %>%
  select(date,spread=value) %>%
  mutate(spread=spread/100) %>%
  filter(!is.na(spread))

#calculate percentile of yield spread
emyieldspread <- emyieldspread %>%
  mutate(percentile = rank(spread)/length(spread))
last(emyieldspread$percentile)

#Plot today's high-yield spread
emyieldspread %>%
  ggplot(aes(x=date,y=spread)) +
  geom_line() +
  labs(title="EM high-yield spread",
       x="Date",
       y="Spread",
       caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1L))

#join US and EM high-yield spread data frames
yieldspread <- yieldspread %>%
  select(date,
         us_spread = spread,
         us_percentile = percentile)
emyieldspread <- emyieldspread %>%
  select(date,
         em_spread = spread,
         em_percentile = percentile)
yieldspread_full <- full_join(yieldspread,emyieldspread,by="date")
rm(yieldspread)
rm(emyieldspread)

#Get and clean Europe high-yield spread data from FRED API
euryieldspread <- fredr("BAMLHE00EHYIOAS")
euryieldspread <- euryieldspread %>%
  select(date,spread=value) %>%
  mutate(spread=spread/100) %>%
  filter(!is.na(spread))

#calculate percentile of yield spread
euryieldspread <- euryieldspread %>%
  mutate(percentile = rank(spread)/length(spread))
last(euryieldspread$percentile)

#Plot today's high-yield spread
euryieldspread %>%
  ggplot(aes(x=date,y=spread)) +
  geom_line() +
  labs(title="Europe high-yield spread",
       x="Date",
       y="Spread",
       caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") +
  scale_y_continuous(labels=scales::percent_format(accuracy=1L))

#Join Europe and full yieldspread data frames
yieldspread_full <- full_join(yieldspread_full,euryieldspread %>% select(date,eur_spread = spread,eur_percentile = percentile),by="date")
rm(euryieldspread)

# #See how correlated these variables are (~.89)
# cor(yieldspread_full$us_spread,yieldspread_full$eur_spread,use="complete.obs")
# cor(yieldspread_full$us_spread,yieldspread_full$em_spread,use="complete.obs")



# Download price data -----------------------------------------------------



#Get historical price data and calculate next period return
#To calculate simple annualized rate of return s from total y-year return r, s=r/y
#To calculate CAGR c from simple annualized rate of return s, c=(s*y+1)^(1/y)-1
#Or to get CAGR c from total return r, c=(r+1)^(1/y)-1
#For now, use simple annualized rate of return
prices <- tq_get(c("VOO","VGK","VIOO","VTWO","GOVT","VCLT","HYG","VWOB","EMHY","IEMG"))
prices <- prices %>%
  group_by(symbol) %>%
  mutate(return_6_mo = c(diff(adjusted,lag = round(252*.5)),rep(NA,times=round(252*.5)))/adjusted,
         return_1_yr = c(diff(adjusted,lag = 252),rep(NA,times=252))/adjusted,
         return_2_yr = c(diff(adjusted,lag = 252*2),rep(NA,times=252*2))/adjusted,
         return_3_yr = c(diff(adjusted,lag = 252*3),rep(NA,times=252*3))/adjusted) %>%
  mutate(return_6_mo = return_6_mo/0.5,
         return_1_yr = return_1_yr,
         return_2_yr = return_2_yr/2,
         return_3_yr = return_3_yr/3)
  # mutate(cagr_6_mo = (return_6_mo+1)^(1/0.5)-1,
  #        cagr_1_yr = (return_1_yr+1)^(1)-1,
  #        cagr_2_yr = (return_2_yr+1)^(1/2)-1,
  #        cagr_3_yr = (return_3_yr+1)^(1/3)-1)

#Join return data with high-yield spread data
prices <- prices %>%
  select(symbol,adjusted,date,return_6_mo,return_1_yr,return_2_yr,return_3_yr)
yieldspread_full <- full_join(yieldspread_full,prices,by="date")



# Download ETF dividend yields --------------------------------------------



#Get dividend history for IEMG, VOO, VTWO, and VIOO from Yahoo! Finance
divs <- tq_get(c("VOO","IEMG","VIOO","VTWO","VGK"),get="dividends")

#Clean up the data and divide payouts by number of days since last payout, then
#annualize by multiplying by 365
iemg_div <- divs %>%
  filter(symbol == "IEMG") %>%
  select(date,payout = value)
iemg_div <- iemg_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(date,payout) %>%
  filter(!is.na(payout))
vgk_div <- divs %>%
  filter(symbol == "VGK") %>%
  select(date,payout = value)
vgk_div <- vgk_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(date,payout) %>%
  filter(!is.na(payout))
voo_div <- divs %>%
  filter(symbol == "VOO") %>%
  select(date,payout = value)
voo_div <- voo_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(date,payout) %>%
  filter(!is.na(payout))
vioo_div <- divs %>%
  filter(symbol == "VIOO") %>%
  select(date,payout = value)
vioo_div <- vioo_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(date,payout) %>%
  filter(!is.na(payout))
vtwo_div <- divs %>%
  filter(symbol == "VTWO") %>%
  select(date,payout = value)
vtwo_div <- vtwo_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(date,payout) %>%
  filter(!is.na(payout))

# #Create loess models to even out noise in annualized dividend payout data
# iemg_div %>% ggplot(aes(x=date,y=payout)) + geom_point() + geom_smooth()
# voo_div %>% ggplot(aes(x=date,y=payout)) + geom_point() + geom_smooth()
# vioo_div %>% ggplot(aes(x=date,y=payout)) + geom_point() + geom_smooth()
# vtwo_div %>% ggplot(aes(x=date,y=payout)) + geom_point() + geom_smooth()

# #Define function to predict robust linear model of payout based on only trailing
# #data points
# linmod <- function(df){
#   df$modeled_payout <- unlist(c(NA,
#     map(2:nrow(df),function(r){
#       predict(MASS::rlm(payout~date,data=df[1:r,]),newdata=data.frame(date=df$date[r]))
#     })))
#   return(df)
# }
#
# #Run function on our data frames
# iemg_div <- linmod(iemg_div)
# vioo_div <- linmod(vioo_div)
# voo_div <- linmod(voo_div)
# vtwo_div <- linmod(vtwo_div)
#
# #Define a function to test plot my linear model approach against TTM moving avg
# testplot <- function(df,payouts_per_year){
#   df %>%
#       mutate(yield = SMA(payout,n=payouts_per_year)) %>%
#       ggplot(aes(x=date,y=payout)) +
#       geom_point() +
#       geom_line(aes(y=yield),col="red") +
#       geom_line(aes(y=modeled_payout),col="blue")
# }
#
# #Run the testplot function to see which approach is lumpier
# #(It's a mixed bag, but it looks to me like a moving average that somehow trims
# #outliers would be preferable to the choppy linear model.)
# testplot(iemg_div,2)
# testplot(voo_div,4)
# testplot(vioo_div,1)
# testplot(vtwo_div,4)

#Define function to calculate yields as two-year trailing moving averages
avg_finder <- function(df,periods_in_year){
  df$yield <- NA
  df <- df %>%
    mutate(yield = case_when(periods_in_year*2 == 8 & !is.na(SMA(payout,n=8))~SMA(payout,n=8),
                             periods_in_year*2 == 8 & !is.na(SMA(payout,n=7))~SMA(payout,n=7),
                             periods_in_year*2 == 8 & !is.na(SMA(payout,n=6))~SMA(payout,n=6),
                             periods_in_year*2 == 8 & !is.na(SMA(payout,n=5))~SMA(payout,n=5),
                             periods_in_year*2 >= 4 & !is.na(SMA(payout,n=4))~SMA(payout,n=4),
                             periods_in_year*2 >= 4 & !is.na(SMA(payout,n=3))~SMA(payout,n=3),
                             periods_in_year*2 >= 2 & !is.na(SMA(payout,n=2))~SMA(payout,n=2),
                             is.na(SMA(payout,n=2))~payout))
  return(df)
}

#Run the function on each data frame
iemg_div <- avg_finder(iemg_div,2)  %>%
  select(date,iemg_yield = yield)
vgk_div <- avg_finder(vgk_div,4)  %>%
  select(date,vgk_yield = yield)
voo_div <- avg_finder(voo_div,4)  %>%
  select(date,voo_yield = yield)
vioo_div <- avg_finder(vioo_div,1)  %>%
  select(date,vioo_yield = yield)
vtwo_div <- avg_finder(vtwo_div,4)  %>%
  select(date,vtwo_yield = yield)
rm(avg_finder)

# #Test plot yields to evaluate lumpiness
# plot(iemg_div,type="l")
# plot(voo_div,type="l")
# plot(vioo_div,type="l")
# plot(vtwo_div,type="l")

#Join with prices dataset
any(!iemg_div$date %in% prices$date)
iemg_div <- prices %>%
  filter(symbol == "IEMG") %>%
  ungroup() %>%
  select(date,adjusted) %>%
  left_join(iemg_div) %>%
  arrange(date)
any(!vgk_div$date %in% prices$date)
vgk_div <- prices %>%
  filter(symbol == "VGK") %>%
  ungroup() %>%
  select(date,adjusted) %>%
  left_join(vgk_div) %>%
  arrange(date)
any(!voo_div$date %in% prices$date)
voo_div <- prices %>%
  filter(symbol == "VOO") %>%
  ungroup() %>%
  select(date,adjusted) %>%
  left_join(voo_div) %>%
  arrange(date)
any(!vioo_div$date %in% prices$date)
vioo_div <- prices %>%
  filter(symbol == "VIOO") %>%
  ungroup() %>%
  select(date,adjusted) %>%
  left_join(vioo_div) %>%
  arrange(date)
any(!vtwo_div$date %in% prices$date)
vtwo_div <- prices %>%
  filter(symbol == "VTWO") %>%
  ungroup() %>%
  select(date,adjusted) %>%
  left_join(vtwo_div) %>%
  arrange(date)

#Interpolate missing values
iemg_div[iemg_div$date >= iemg_div$date[which(!is.na(iemg_div$iemg_yield))[1]],] <-
  iemg_div[iemg_div$date >= iemg_div$date[which(!is.na(iemg_div$iemg_yield))[1]],] %>%
  mutate(iemg_yield = na.locf(iemg_yield))
vgk_div[vgk_div$date >= vgk_div$date[which(!is.na(vgk_div$vgk_yield))[1]],] <-
  vgk_div[vgk_div$date >= vgk_div$date[which(!is.na(vgk_div$vgk_yield))[1]],] %>%
  mutate(vgk_yield = na.locf(vgk_yield))
voo_div[voo_div$date >= voo_div$date[which(!is.na(voo_div$voo_yield))[1]],] <-
  voo_div[voo_div$date >= voo_div$date[which(!is.na(voo_div$voo_yield))[1]],] %>%
  mutate(voo_yield = na.locf(voo_yield))
vioo_div[vioo_div$date >= vioo_div$date[which(!is.na(vioo_div$vioo_yield))[1]],] <-
  vioo_div[vioo_div$date >= vioo_div$date[which(!is.na(vioo_div$vioo_yield))[1]],] %>%
  mutate(vioo_yield = na.locf(vioo_yield))
vtwo_div[vtwo_div$date >= vtwo_div$date[which(!is.na(vtwo_div$vtwo_yield))[1]],] <-
  vtwo_div[vtwo_div$date >= vtwo_div$date[which(!is.na(vtwo_div$vtwo_yield))[1]],] %>%
  mutate(vtwo_yield = na.locf(vtwo_yield))

#Convert to percent yields and then remove adjusted price variable
iemg_div <- iemg_div %>%
  mutate(iemg_yield = iemg_yield/adjusted) %>%
  select(-adjusted)
vgk_div <- vgk_div %>%
  mutate(vgk_yield = vgk_yield/adjusted) %>%
  select(-adjusted)
voo_div <- voo_div %>%
  mutate(voo_yield = voo_yield/adjusted) %>%
  select(-adjusted)
vioo_div <- vioo_div %>%
  mutate(vioo_yield = vioo_yield/adjusted) %>%
  select(-adjusted)
vtwo_div <- vtwo_div %>%
  mutate(vtwo_yield = vtwo_yield/adjusted) %>%
  select(-adjusted)
range(iemg_div$iemg_yield,na.rm=T)
range(vgk_div$vgk_yield,na.rm=T)
range(voo_div$voo_yield,na.rm=T)
range(vioo_div$vioo_yield,na.rm=T)
range(vtwo_div$vtwo_yield,na.rm=T)

#Combine the data sets
df <- full_join(full_join(full_join(iemg_div,voo_div),full_join(vioo_div,vtwo_div)),vgk_div)

#Join with yieldspread_full
yieldspread_full <- left_join(yieldspread_full,df)



# Download Effective HY Yields --------------------------------------------



#Navigate to effective US HY yield page
us_hy_yield <- fredr("BAMLH0A0HYM2EY")
us_hy_yield <- us_hy_yield %>%
  select(date,us_hy_yield=value) %>%
  mutate(us_hy_yield=us_hy_yield/100) %>%
  filter(!is.na(us_hy_yield))

#calculate percentile of yield
us_hy_yield %>%
  mutate(percentile = rank(us_hy_yield)/length(us_hy_yield)) %>%
  pull(percentile) %>%
  last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,us_hy_yield,by="date")
rm(us_hy_yield)

#Navigate to effective EM HY yield page
em_hy_yield <- fredr("BAMLEMHBHYCRPIEY")
em_hy_yield <- em_hy_yield %>%
  select(date,em_hy_yield=value) %>%
  mutate(em_hy_yield=em_hy_yield/100) %>%
  filter(!is.na(em_hy_yield))

#calculate percentile of yield
em_hy_yield %>%
  mutate(percentile = rank(em_hy_yield)/length(em_hy_yield)) %>%
  pull(percentile) %>%
  last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,em_hy_yield,by="date")
rm(em_hy_yield)



# Download Effective IG Yields --------------------------------------------



#Navigate to effective US IG yield page
us_ig_yield <- fredr("BAMLC0A0CMEY")
us_ig_yield <- us_ig_yield %>%
  select(date,us_ig_yield=value) %>%
  mutate(us_ig_yield=us_ig_yield/100) %>%
  filter(!is.na(us_ig_yield))

#calculate percentile of yield
us_ig_yield %>%
  mutate(percentile = rank(us_ig_yield)/length(us_ig_yield)) %>%
  pull(percentile) %>%
  last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,us_ig_yield,by="date")
rm(us_ig_yield)

#Navigate to effective EM IG yield page
em_ig_yield <- fredr("BAMLEMIBHGCRPIEY")
em_ig_yield <- em_ig_yield %>%
  select(date,em_ig_yield=value) %>%
  mutate(em_ig_yield=em_ig_yield/100) %>%
  filter(!is.na(em_ig_yield))

#calculate percentile of yield
em_ig_yield %>%
  mutate(percentile = rank(em_ig_yield)/length(em_ig_yield)) %>%
  pull(percentile) %>%
  last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,em_ig_yield,by="date")
rm(em_ig_yield)

#Navigate to effective EM Govt yield page
em_govt_yield <- fredr("BAMLEMPBPUBSICRPIEY") %>%
  select(date,em_govt_yield=value) %>%
  mutate(em_govt_yield=em_govt_yield/100) %>%
  filter(!is.na(em_govt_yield))

#calculate percentile of yield
em_govt_yield %>%
  mutate(percentile = rank(em_govt_yield)/length(em_govt_yield)) %>%
  pull(percentile) %>%
  last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,em_govt_yield,by="date")
rm(em_govt_yield)



# Download Macroeconomic Indicators ---------------------------------------



#Get FRED US CLI Leading Indicators series
cli_lead_us <- fredr("USALOLITONOSTSAM")
cli_lead_us <- cli_lead_us %>%
  select(date,cli_lead_us=value) %>%
  mutate(cli_lead_us = cli_lead_us-100) %>%
  filter(!is.na(cli_lead_us))

#Get FRED equities as percentage of US households' assets data
us_equities_allocation <- fredr("BOGZ1FL153064486Q")
us_equities_allocation <- us_equities_allocation %>%
  filter(date >= as.Date("1951-10-01")) %>% #Remove pre-1951 non-quarterly data
  select(date,us_equities_allocation=value) %>%
  mutate(equities_allocation_12m_change = us_equities_allocation - lag(us_equities_allocation,4),
         equities_allocation_18m_change = us_equities_allocation - lag(us_equities_allocation,6)) %>%
  filter(!is.na(equities_allocation_12m_change),
         !is.na(equities_allocation_18m_change)) %>%
  select(date,equities_allocation_12m_change,equities_allocation_18m_change)

#Get FRED global price of industrial materials data
industrial_materials <- fredr("PINDUINDEXM")
industrial_materials <- industrial_materials %>%
  select(date,industrial_materials=value) %>%
  filter(!is.na(industrial_materials))

#Get FRED global price of raw materials data
raw_materials <- fredr("PRAWMINDEXM")
raw_materials <- raw_materials %>%
  select(date,raw_materials=value) %>%
  filter(!is.na(raw_materials))

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,cli_lead_us,by="date")
yieldspread_full <- full_join(yieldspread_full,us_equities_allocation,by="date")
yieldspread_full <- full_join(yieldspread_full,industrial_materials,by="date")
yieldspread_full <- full_join(yieldspread_full,raw_materials,by="date")
rm(cli_lead_us,us_equities_allocation,industrial_materials,raw_materials)

#Interpolate last non-NA value
yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$cli_lead_us))[1]],] <-
  yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$cli_lead_us))[1]],] %>%
  mutate(cli_lead_us = na.locf(cli_lead_us))
yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$equities_allocation_12m_change))[1]],] <-
  yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$equities_allocation_12m_change))[1]],] %>%
  mutate(equities_allocation_12m_change = na.locf(equities_allocation_12m_change),
         equities_allocation_18m_change = na.locf(equities_allocation_18m_change))
yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$industrial_materials))[1]],] <-
  yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$industrial_materials))[1]],] %>%
  mutate(industrial_materials = na.locf(industrial_materials))
yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$raw_materials))[1]],] <-
  yieldspread_full[yieldspread_full$date >= yieldspread_full$date[which(!is.na(yieldspread_full$raw_materials))[1]],] %>%
  mutate(raw_materials = na.locf(raw_materials))



# Download Risk-Free Rates ------------------------------------------------



#Navigate to 6-month T-Bill yield page
yield6mo <- fredr("DGS6MO")
yield6mo <- yield6mo %>%
  select(date,yield6mo=value) %>%
  mutate(yield6mo=yield6mo/100) %>%
  filter(!is.na(yield6mo))

#Navigate to 1-year Treasury yield page
yield1yr <- fredr("DGS1")
yield1yr <- yield1yr %>%
  select(date,yield1yr=value) %>%
  mutate(yield1yr=yield1yr/100) %>%
  filter(!is.na(yield1yr))

#Navigate to 2-year Treasury yield page
yield2yr <- fredr("DGS2")
yield2yr <- yield2yr %>%
  select(date,yield2yr=value) %>%
  mutate(yield2yr=yield2yr/100) %>%
  filter(!is.na(yield2yr))

#Navigate to 3-year Treasury yield page
yield3yr <- fredr("DGS3")
yield3yr <- yield3yr %>%
  select(date,yield3yr=value) %>%
  mutate(yield3yr=yield3yr/100) %>%
  filter(!is.na(yield3yr))

#Navigate to 7-year Treasury yield page
yield7yr <- fredr("DGS7")
yield7yr <- yield7yr %>%
  select(date,yield7yr=value) %>%
  mutate(yield7yr=yield7yr/100) %>%
  filter(!is.na(yield7yr))

#Put risk-free returns into a table
#For now, use the annualized returns, not the total returns
risk_free_rate <- data.frame(timeframe = c("6-month forward return",
                                           "1-year forward return",
                                           "2-year forward return",
                                           "3-year forward return"),
                             risk_free_return = c(last(yield6mo$yield6mo),
                                                  last(yield1yr$yield1yr),
                                                  last(yield2yr$yield2yr),
                                                  last(yield3yr$yield3yr))) #%>%
  # mutate(risk_free_return = c(last(yield6mo$yield6mo)*0.5,
  #                             last(yield1yr$yield1yr),
  #                             last(yield2yr$yield2yr)*2,
  #                             last(yield3yr$yield3yr)*3))

#Convert risk-free rates to compound interest using formula c=(s*y+1)^(1/y)-1
# risk_free_rate$risk_free_cagr[1] <- (risk_free_rate$risk_free_rate[1]*0.5+1)^(1/0.5)-1
# risk_free_rate$risk_free_cagr[2] <- (risk_free_rate$risk_free_rate[2]*1+1)^(1/1)-1
# risk_free_rate$risk_free_cagr[3] <- (risk_free_rate$risk_free_rate[3]*2+1)^(1/2)-1
# risk_free_rate$risk_free_cagr[4] <- (risk_free_rate$risk_free_rate[4]*3+1)^(1/3)-1

#Join risk-free rates with yieldspread_full
yieldspread_full <- full_join(yieldspread_full,yield6mo,by="date")
yieldspread_full <- full_join(yieldspread_full,yield1yr,by="date")
yieldspread_full <- full_join(yieldspread_full,yield2yr,by="date")
yieldspread_full <- full_join(yieldspread_full,yield3yr,by="date")
yieldspread_full <- full_join(yieldspread_full,yield7yr,by="date")

#Remove extra variables
rm(yield6mo)
rm(yield1yr)
rm(yield2yr)
rm(yield3yr)
rm(yield7yr)

#Sort yieldspread full by date and symbol
yieldspread_full <- yieldspread_full %>%
  arrange(date,symbol)



# Download VIX and DXY ----------------------------------------------------



#Navigate to VIX page
vix <- fredr("VIXCLS")
vix <- vix %>%
  select(date,vix=value) %>%
  filter(!is.na(vix))

#Navigate to DXY page
dxy <- fredr("DTWEXBGS")
dxy <- dxy %>%
  select(date,dxy=value) %>%
  filter(!is.na(dxy))

#Join with yieldspread dataframe
yieldspread_full <- left_join(yieldspread_full,dxy)
yieldspread_full <- left_join(yieldspread_full,vix)



# Analyze S&P 500 Returns -------------------------------------------------


#Define a ticker and a list of variables to study
ticker_to_study <- "VOO"
varlist <- c("us_spread","yield7yr","dxy","vix","voo_yield",
             "cli_lead_us","equities_allocation_12m_change","equities_allocation_18m_change",
             "industrial_materials","raw_materials")

#Calculate correlation coefficients between variables of interest and next-period return
cors <- yieldspread_full %>%
  corchecker(ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

#Winnow list of variables to study, keeping only the top 5
varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[which.min(errors$mean_error)]
varlist <- str_split(model_type,"\\+")[[1]]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set)

#Calculate model standard deviations
sds <- get_sds(df=test_set)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[2]
chartit(df = yieldspread,insample=T)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- get_sharpes()
sharpes
rm(sds)


# Analyze GOVT Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "GOVT"
varlist <- c("us_spread","yield7yr","dxy","vix",
             "cli_lead_us","industrial_materials","raw_materials",
             "equities_allocation_12m_change","equities_allocation_18m_change")

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread_full,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

#Winnow list of variables to study, keeping only the top 5
varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[which.min(errors$mean_error)]
varlist <- str_split(model_type,"\\+")[[1]]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set)

#Calculate model standard deviations
sds <- get_sds(df=test_set)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = yieldspread,insample=T)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Analyze VWOB Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "VWOB"
varlist <- c("em_spread","em_govt_yield","dxy",
             "industrial_materials","raw_materials")

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread_full,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

#Winnow list of variables to study, keeping only those with average correlation > .3
varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[which.min(errors$mean_error)]
varlist <- str_split(model_type,"\\+")[[1]]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set)

#Calculate model standard deviations
sds <- get_sds(df=test_set)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = yieldspread,insample=T)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Analyze VCLT Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "VCLT"
varlist <- c("yield7yr","us_spread","us_ig_yield","dxy","vix",
             "cli_lead_us","raw_materials","industrial_materials",
             "equities_allocation_12m_change","equities_allocation_18m_change")

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread_full,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

#Winnow list of variables to study, keeping only the top 5
varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[which.min(errors$mean_error)]
varlist <- str_split(model_type,"\\+")[[1]]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set)

#Calculate model standard deviations
sds <- get_sds(df=test_set)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = yieldspread,insample=T)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Analyze EMHY Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "EMHY"
varlist <- c("em_spread","em_hy_yield","em_govt_yield","raw_materials","industrial_materials")

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread_full,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

#Winnow list of variables to study, keeping only those with average correlation > .3
varlist <- row.names(cors[cors$avg_of_abs_vals>0.3,])

#Delete correlation coefficients table from workspace
rm(cors)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[which.min(errors$mean_error)]
varlist <- str_split(model_type,"\\+")[[1]]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set)

#Calculate model standard deviations
sds <- get_sds(df=test_set)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = yieldspread,insample=T)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Analyze IEMG Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "IEMG"
varlist <- c("em_spread","iemg_yield","em_govt_yield","em_ig_discount","dxy",
             "raw_materials","industrial_materials")

#Calculate correlation coefficient between variables of interest and next-period return
#Try out new variable subtracting EM investment-grade yield from US high yield
#(Perhaps this works because EM IG companies compete with the junkiest US sectors,
#like mining and oil and gas?)
cors <- yieldspread_full %>%
  mutate(em_ig_discount = us_hy_yield - em_ig_yield) %>%
  corchecker(ticker = ticker_to_study,
             vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

#Winnow list of variables to study, keeping only the top 5
varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  mutate(em_ig_discount = us_hy_yield - em_ig_yield) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[which.min(errors$mean_error)]
varlist <- str_split(model_type,"\\+")[[1]]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set)

#Calculate model standard deviations
sds <- get_sds(df=test_set)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  mutate(em_ig_discount = us_hy_yield - em_ig_yield) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = yieldspread,insample=T)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Analyze VGK Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "VGK"
varlist <- c("eur_spread","dxy","vgk_yield",
             "cli_lead_us","raw_materials","industrial_materials")

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread_full,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

#Winnow list of variables to study, keeping only those with average correlation > .3
varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[which.min(errors$mean_error)]
varlist <- str_split(model_type,"\\+")[[1]]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set)

#Calculate model standard deviations
sds <- get_sds(df=test_set)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      "return_6_mo","return_1_yr","return_2_yr","return_3_yr",
      varlist)]

#Retrain the optimal model on the full training data set
model_6mo <- lm(yieldspread,formula = paste0("return_6_mo ~ ",model_type))
model_1yr <- lm(yieldspread,formula = paste0("return_1_yr ~ ",model_type))
model_2yr <- lm(yieldspread,formula = paste0("return_2_yr ~ ",model_type))
model_3yr <- lm(yieldspread,formula = paste0("return_3_yr ~ ",model_type))

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = yieldspread,insample=T)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Cross-Asset Comparison -----------------------------------------------



#For comparison purposes, let's focus on one-year forecasts
sharpes <- sharpes %>%
  filter(Timeframe == "1-year forward return")
sharpes %>%
  mutate(`Asset class` = case_when(Ticker=="EMHY"~"EM HY Bonds",
                                   Ticker=="GOVT"~"US Govt Bonds",
                                   Ticker=="IEMG"~"EM Stocks",
                                   Ticker=="VCLT"~"US Corp Bonds",
                                   Ticker=="VGK"~"EU Stocks",
                                   Ticker=="VOO"~"US Stocks",
                                   Ticker=="EMHY"~"EM HY Bonds",
                                   Ticker=="VWOB"~"EM Govt Bonds")) %>%
  select(`Asset class`,Ticker,Model,`Expected return`,`Expected standard deviation`,`Expected Sharpe`) %>%
  arrange(desc(`Expected Sharpe`)) %>%
  kable(caption = "Sharpe ratios, 1-year timeframe")

#Plot forecasted one-year returns and standard deviations
sharpes %>%
  mutate(`Asset class` = case_when(Ticker=="EMHY"~"EM HY\nBonds",
                            Ticker=="GOVT"~"US Govt\nBonds",
                            Ticker=="IEMG"~"EM Stocks",
                            Ticker=="VCLT"~"US Corp\nBonds",
                            Ticker=="VGK"~"EU Stocks",
                            Ticker=="VOO"~"US Stocks",
                            Ticker=="EMHY"~"EM HY\nBonds",
                            Ticker=="VWOB"~"EM Govt\nBonds")) %>%
  mutate(`Asset class` = fct_reorder(`Asset class`,`Expected return`,.desc=T)) %>%
  ggplot(aes(x=`Asset class`,y=`Expected return`)) +
  geom_col(fill="lightblue") +
  geom_errorbar(aes(ymin=`Expected return`-`Expected standard deviation`,ymax=`Expected return`+`Expected standard deviation`)) +
  scale_y_continuous(labels = percent) +
  ggtitle("Forecasted 1-year returns and standard deviations for select stock\nand bond index ETFs") +
  labs(caption = "Copyright 2022 Christopher C. Smith") +
  #scale_x_discrete(guide = guide_axis(angle = 45)) +
  theme_minimal()

#save the plot
ggsave(filename = "multi-asset-model.jpg",
       scale = 1,
       width = 1920/300,
       height = 1080/300,
       units = "in",
       path=img_save_location)



# Development Notes -------------------------------------------------------



# I should probably be excluding training data that overlaps with test data
# during cross-validation. I could do this by recalculating the returns for any
# training set that includes a date less than three years before the first date
# in the test set.

# Development ideas:
# Try reverse repo stochastic as an indicator of market bottoms?
# Explore using P/E or earnings yield rather than div yield to predict index returns?
# (Dividend yield is problematic because it's a poor proxy for shareholder yield)
# Try optimizing a rebalance model, maybe using decision trees?
# When calculating sharpe, I may want to take the higher sd of in-sample and out-of-sample
# Build a modular log vs. linear tuner and apply to all my asset classes
# FRED no longer provides ISM manufacturing PMI; I should see if I can automate
# retrieval of this data for testing. (Data through 2015 available here:
# https://data.nasdaq.com/data/FRED/NAPM-ism-manufacturing-pmi-composite-index)
# Also try Fed balance sheet.
