# Script to predict market returns and sharpe ratios across various asset classes

#NOTE: The script will only work if you have a valid FRED API key in .Renviron!
#The easiest way to edit .Renviron is by calling usethis::edit_r_environ().
#Then add the line "FRED_API_KEY = yourAPIkeyhere", without quotes, and save file.



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

  #Find all unique combinations of up to two of the variables we're testing,
  #either linear or log. Delete any combos that have a linear and log of the same
  #variable.
  log_variables <- paste0("log(",variables_to_study,")")
  variables_to_study <- c(variables_to_study,log_variables)
  varcombos <- map(1:2,function(x){
    asplit(combinations(n = length(variables_to_study),r = x,v = variables_to_study,repeats.allowed = F),MARGIN = 1)
  }) %>% unlist(recursive=F)
  varcombos <- varcombos[unlist(map(1:length(varcombos),function(x){
    all(str_count(paste(varcombos[x],collapse=" "),variables_to_study) <= 1)
  }))]

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
    suppressWarnings(train_set_0$return_6_mo <- case_when(between(test_set_0$date[1] - train_set_0$date,0,365*0.5)~NA_real_,
                                           T~train_set_0$return_6_mo))
    suppressWarnings(train_set_1$return_1_yr <- case_when(between(test_set_1$date[1] - train_set_1$date,0,365*1)~NA_real_,
                                           T~train_set_1$return_1_yr))
    suppressWarnings(train_set_2$return_2_yr <- case_when(between(test_set_2$date[1] - train_set_2$date,0,365*2)~NA_real_,
                                           T~train_set_2$return_2_yr))
    suppressWarnings(train_set_3$return_3_yr <- case_when(between(test_set_3$date[1] - train_set_3$date,0,365*3)~NA_real_,
                                           T~train_set_3$return_3_yr))

    #Create models on training data, predict on test data, and calculate error
    map_dfr(1:length(varcombos),function(y){
      data.frame(model = paste(varcombos[[y]],collapse="+"),
                 trial = x,
                 error_measure = optimizer(c(test_set_0$return_6_mo,
                                             test_set_1$return_1_yr,
                                             test_set_2$return_2_yr,
                                             test_set_3$return_3_yr),
                                           c(predict(lm(formula = paste0("return_6_mo ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_0),newdata=test_set_0),
                                             predict(lm(formula = paste0("return_1_yr ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_1),newdata=test_set_1),
                                             predict(lm(formula = paste0("return_2_yr ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_2),newdata=test_set_2),
                                             predict(lm(formula = paste0("return_3_yr ~ ",paste(varcombos[[y]],collapse="+")),data=train_set_3),newdata=test_set_3))))
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
  bind_rows(tmp,tmp2)
}

#Define function to calculate standard deviations of optimized model
#(with sds from a naive-average forecast for comparison)
get_sds <- function(df = test_set,print=T,use_model=T){
  tmp <- df %>%
    mutate(Timeframe = timeframe) %>%
    group_by(Timeframe) %>%
    mutate(`Model` = case_when(use_model==T~value-forecast,
                               use_model==F~value-naive_forecast),
           `Naive Average` = value-naive_forecast) %>%
    summarize(`Model` = sd(`Model`,na.rm=T),
              `Naive Average` = sd(`Naive Average`,na.rm=T))
  tmp2 <- df %>%
    mutate(`Model` = case_when(use_model==T~value-forecast,
                               use_model==F~value-naive_forecast),
           `Naive Average` = value-naive_forecast) %>%
    summarize(`Model` = sd(`Model`,na.rm=T),
              `Naive Average` = sd(`Naive Average`,na.rm=T)) %>%
    mutate(Timeframe = "Weighted Model Average")
  if(print==T){bind_rows(tmp,tmp2) %>%
    kable(caption="Standard Deviation from Forecast") %>%
    print()}
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
  if(use_model==T){
    forecasts <- yieldspread %>%
      filter(!is.na(forecast)) %>%
      filter(date == max(date)) %>%
      select(timeframe,forecast)
  }else{
    forecasts <- yieldspread %>%
      filter(!is.na(naive_forecast)) %>%
      filter(date == max(date)) %>%
      select(timeframe,forecast=naive_forecast)
  }

  #Join forecasts table with sds and risk-free rates and calculate expected sharpe
  #for the next period
  #(For now, I am not going to adjust vs. risk-free rates because my US Govt
  #bond ETF is a more realistic comparison given my investment style.)
  sharpe <- full_join(sharpe,forecasts)
  #sharpe <- sharpe %>% mutate(model_sharpe = (forecast-risk_free_return) / model_sd)
  sharpe <- sharpe %>% mutate(model_sharpe = forecast / model_sd)

  #Annualize the sharpe ratios
  sharpes <- sharpe %>%
    mutate(Ticker = ticker_to_study) %>%
    mutate(Model = case_when(use_model==T~model_type,
                             use_model==F~"naive_average")) %>%
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

# #calculate percentile of yield spread
# yieldspread <- yieldspread %>%
#   mutate(percentile = rank(spread)/length(spread))
# last(yieldspread$percentile)
#
# #Plot today's high-yield spread
# yieldspread %>%
#   ggplot(aes(x=date,y=spread)) +
#   geom_line() +
#   labs(title="US high-yield spread",
#        x="Date",
#        y="Spread",
#        caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") +
#   scale_y_continuous(labels=scales::percent_format(accuracy=1L))

#Get FRED series for EM high-yield spread
emyieldspread <- fredr("BAMLEMHYHYLCRPIUSOAS")
emyieldspread <- emyieldspread %>%
  select(date,spread=value) %>%
  mutate(spread=spread/100) %>%
  filter(!is.na(spread))

# #calculate percentile of yield spread
# emyieldspread <- emyieldspread %>%
#   mutate(percentile = rank(spread)/length(spread))
# last(emyieldspread$percentile)
#
# #Plot today's high-yield spread
# emyieldspread %>%
#   ggplot(aes(x=date,y=spread)) +
#   geom_line() +
#   labs(title="EM high-yield spread",
#        x="Date",
#        y="Spread",
#        caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") +
#   scale_y_continuous(labels=scales::percent_format(accuracy=1L))

#join US and EM high-yield spread data frames
yieldspread <- yieldspread %>%
  select(date,
         us_spread = spread)
emyieldspread <- emyieldspread %>%
  select(date,
         em_spread = spread)
yieldspread_full <- full_join(yieldspread,emyieldspread,by="date")
rm(yieldspread)
rm(emyieldspread)

#Get and clean Europe high-yield spread data from FRED API
euryieldspread <- fredr("BAMLHE00EHYIOAS")
euryieldspread <- euryieldspread %>%
  select(date,spread=value) %>%
  mutate(spread=spread/100) %>%
  filter(!is.na(spread))

# #calculate percentile of yield spread
# euryieldspread <- euryieldspread %>%
#   mutate(percentile = rank(spread)/length(spread))
# last(euryieldspread$percentile)
#
# #Plot today's high-yield spread
# euryieldspread %>%
#   ggplot(aes(x=date,y=spread)) +
#   geom_line() +
#   labs(title="Europe high-yield spread",
#        x="Date",
#        y="Spread",
#        caption="Copyright 2022 Wall Street Petting Zoo\nData courtesy FRED and Yahoo! Finance API") +
#   scale_y_continuous(labels=scales::percent_format(accuracy=1L))

#Join Europe and full yieldspread data frames
yieldspread_full <- full_join(yieldspread_full,euryieldspread %>% select(date,eur_spread = spread),by="date")
rm(euryieldspread)

# #See how correlated these variables are (~.94)
cor(yieldspread_full$us_spread,yieldspread_full$eur_spread,use="complete.obs")
cor(yieldspread_full$us_spread,yieldspread_full$em_spread,use="complete.obs")



# Download Effective HY Yields --------------------------------------------



#Navigate to effective US HY yield page
us_hy_yield <- fredr("BAMLH0A0HYM2EY")
us_hy_yield <- us_hy_yield %>%
  select(date,us_hy_yield=value) %>%
  mutate(us_hy_yield=us_hy_yield/100) %>%
  filter(!is.na(us_hy_yield))

# #calculate percentile of yield
# us_hy_yield %>%
#   mutate(percentile = rank(us_hy_yield)/length(us_hy_yield)) %>%
#   pull(percentile) %>%
#   last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,us_hy_yield,by="date")
rm(us_hy_yield)

#Navigate to effective EM HY yield page
em_hy_yield <- fredr("BAMLEMHBHYCRPIEY")
em_hy_yield <- em_hy_yield %>%
  select(date,em_hy_yield=value) %>%
  mutate(em_hy_yield=em_hy_yield/100) %>%
  filter(!is.na(em_hy_yield))

# #calculate percentile of yield
# em_hy_yield %>%
#   mutate(percentile = rank(em_hy_yield)/length(em_hy_yield)) %>%
#   pull(percentile) %>%
#   last()

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

# #calculate percentile of yield
# us_ig_yield %>%
#   mutate(percentile = rank(us_ig_yield)/length(us_ig_yield)) %>%
#   pull(percentile) %>%
#   last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,us_ig_yield,by="date")
rm(us_ig_yield)

#Navigate to effective EM IG yield page
em_ig_yield <- fredr("BAMLEMIBHGCRPIEY")
em_ig_yield <- em_ig_yield %>%
  select(date,em_ig_yield=value) %>%
  mutate(em_ig_yield=em_ig_yield/100) %>%
  filter(!is.na(em_ig_yield))

# #calculate percentile of yield
# em_ig_yield %>%
#   mutate(percentile = rank(em_ig_yield)/length(em_ig_yield)) %>%
#   pull(percentile) %>%
#   last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,em_ig_yield,by="date")
rm(em_ig_yield)

#Navigate to effective EM Govt yield page
em_govt_yield <- fredr("BAMLEMPBPUBSICRPIEY") %>%
  select(date,em_govt_yield=value) %>%
  mutate(em_govt_yield=em_govt_yield/100) %>%
  filter(!is.na(em_govt_yield))

# #calculate percentile of yield
# em_govt_yield %>%
#   mutate(percentile = rank(em_govt_yield)/length(em_govt_yield)) %>%
#   pull(percentile) %>%
#   last()

#Join with yieldspread data frame
yieldspread_full <- full_join(yieldspread_full,em_govt_yield,by="date")
rm(em_govt_yield)



# Download Macroeconomic Indicators ---------------------------------------



#Get FRED US CLI Leading Indicators series
cli_lead_us <- fredr("USALOLITONOSTSAM")
cli_lead_us <- cli_lead_us %>%
  select(date,cli_lead_us=value)  %>%
  filter(!is.na(cli_lead_us)) %>%
  mutate(cli_lead_us = (cli_lead_us-min(cli_lead_us))/(max(cli_lead_us)-min(cli_lead_us))+1)

#Get FRED equities as percentage of US households' assets data
#Calculate trailing 12 and 18 month change
#Make sure all values are positive for log transformation
us_equities_allocation <- fredr("BOGZ1FL153064486Q")
us_equities_allocation <- us_equities_allocation %>%
  filter(date >= as.Date("1951-10-01")) %>% #Remove pre-1951 non-quarterly data
  select(date,us_equities_allocation=value) %>%
  mutate(equities_allocation_12m_change = us_equities_allocation - lag(us_equities_allocation,4),
         equities_allocation_18m_change = us_equities_allocation - lag(us_equities_allocation,6)) %>%
  filter(!is.na(equities_allocation_12m_change),
         !is.na(equities_allocation_18m_change)) %>%
  select(date,equities_allocation_12m_change,equities_allocation_18m_change) %>%
  mutate(equities_allocation_12m_change = (equities_allocation_12m_change-min(equities_allocation_12m_change))/(max(equities_allocation_12m_change)-min(equities_allocation_12m_change))+1,
         equities_allocation_18m_change = (equities_allocation_18m_change-min(equities_allocation_18m_change))/(max(equities_allocation_18m_change)-min(equities_allocation_18m_change))+1)

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

#Sort yieldspread_full by date
yieldspread_full <- yieldspread_full %>%
  arrange(date)



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
yieldspread_full <- full_join(yieldspread_full,dxy,by="date")
yieldspread_full <- full_join(yieldspread_full,vix,by="date")
rm(dxy,vix)



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
rm(prices)



# Download ETF dividend yields --------------------------------------------



#Get dividend history for IEMG, VOO, VTWO, VIOO, and VGK from Yahoo! Finance
divs <- tq_get(c("VOO","IEMG","VIOO","VTWO","VGK"),get="dividends")

#Clean up the data and divide payouts by number of days since last payout, then
#annualize by multiplying by 365
iemg_div <- divs %>%
  filter(symbol == "IEMG") %>%
  select(symbol,date,payout = value)
iemg_div <- iemg_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(symbol,date,payout) %>%
  filter(!is.na(payout))
vgk_div <- divs %>%
  filter(symbol == "VGK") %>%
  select(symbol,date,payout = value)
vgk_div <- vgk_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(symbol,date,payout) %>%
  filter(!is.na(payout))
voo_div <- divs %>%
  filter(symbol == "VOO") %>%
  select(symbol,date,payout = value)
voo_div <- voo_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(symbol,date,payout) %>%
  filter(!is.na(payout))
vioo_div <- divs %>%
  filter(symbol == "VIOO") %>%
  select(symbol,date,payout = value)
vioo_div <- vioo_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(symbol,date,payout) %>%
  filter(!is.na(payout))
vtwo_div <- divs %>%
  filter(symbol == "VTWO") %>%
  select(symbol,date,payout = value)
vtwo_div <- vtwo_div %>%
  arrange(date) %>%
  mutate(payout = (payout/as.numeric(date-lag(date)))*365) %>%
  select(symbol,date,payout) %>%
  filter(!is.na(payout))

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
  select(symbol,date,div_yield = yield)
vgk_div <- avg_finder(vgk_div,4)  %>%
  select(symbol,date,div_yield = yield)
voo_div <- avg_finder(voo_div,4)  %>%
  select(symbol,date,div_yield = yield)
vioo_div <- avg_finder(vioo_div,1)  %>%
  select(symbol,date,div_yield = yield)
vtwo_div <- avg_finder(vtwo_div,4)  %>%
  select(symbol,date,div_yield = yield)
rm(avg_finder)

#Join with yieldspread_full dataset
yieldspread_full <- full_join(yieldspread_full,
                              bind_rows(iemg_div,vgk_div,voo_div,vioo_div,vtwo_div),
                              by=c("date","symbol"))
rm(iemg_div,vgk_div,voo_div,vioo_div,vtwo_div)



# Interpolate missing values ----------------------------------------------



#Arrange dataset by date
yieldspread_full <- yieldspread_full %>%
  arrange(date)

#Interpolate last non-NA value for various variables
yieldspread_full <- yieldspread_full %>%
  fill(us_spread,em_spread,eur_spread,cli_lead_us,equities_allocation_12m_change,
       equities_allocation_18m_change,industrial_materials,raw_materials,yield6mo,
       yield1yr,yield2yr,yield3yr,yield7yr,vix,dxy)

#Interpolate last non-NA value for dividend yield variables
yieldspread_full <- yieldspread_full %>%
  group_by(symbol) %>%
  fill(div_yield)

#Convert to percent dividend yields and then remove adjusted price variable
yieldspread_full <- yieldspread_full %>%
  mutate(div_yield = div_yield/adjusted)

#Remove any rows we don't have price data for
yieldspread_full <- yieldspread_full %>%
  filter(!is.na(adjusted))



# Analyze S&P 500 Returns -------------------------------------------------


#Define a ticker and a list of variables to study
ticker_to_study <- "VOO"
varlist <- c("us_spread","yield7yr","dxy","vix","div_yield",
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
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

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
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
use_model <- case_when(compare_errors(df = test_set)[5,2] >= compare_errors(df = test_set)[5,3] &
                         compare_errors(df = test_set)[2,2] >= compare_errors(df = test_set)[2,3] ~ F,
                       T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

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

#If sds are higher than when we plotted out of sample, use the higher value
sds <- sds %>%
  mutate(comparer = get_sds(df=yieldspread,print=F,use_model=use_model)$Model) %>%
  mutate(Model = case_when(Model > comparer~Model,
                           T~comparer)) %>%
  select(-comparer)

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
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

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
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
use_model <- case_when(compare_errors(df = test_set)[5,2] >= compare_errors(df = test_set)[5,3] &
                         compare_errors(df = test_set)[2,2] >= compare_errors(df = test_set)[2,3] ~ F,
                       T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

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

#If sds are higher than when we plotted out of sample, use the higher value
sds <- sds %>%
  mutate(comparer = get_sds(df=yieldspread,print=F,use_model=use_model)$Model) %>%
  mutate(Model = case_when(Model > comparer~Model,
                           T~comparer)) %>%
  select(-comparer)

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
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

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
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
use_model <- case_when(compare_errors(df = test_set)[5,2] >= compare_errors(df = test_set)[5,3] &
                         compare_errors(df = test_set)[2,2] >= compare_errors(df = test_set)[2,3] ~ F,
                       T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

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

#If sds are higher than when we plotted out of sample, use the higher value
sds <- sds %>%
  mutate(comparer = get_sds(df=yieldspread,print=F,use_model=use_model)$Model) %>%
  mutate(Model = case_when(Model > comparer~Model,
                           T~comparer)) %>%
  select(-comparer)

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
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

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
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
use_model <- case_when(compare_errors(df = test_set)[5,2] >= compare_errors(df = test_set)[5,3] &
                         compare_errors(df = test_set)[2,2] >= compare_errors(df = test_set)[2,3] ~ F,
                       T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

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

#If sds are higher than when we plotted out of sample, use the higher value
sds <- sds %>%
  mutate(comparer = get_sds(df=yieldspread,print=F,use_model=use_model)$Model) %>%
  mutate(Model = case_when(Model > comparer~Model,
                           T~comparer)) %>%
  select(-comparer)

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
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

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
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
use_model <- case_when(compare_errors(df = test_set)[5,2] >= compare_errors(df = test_set)[5,3] &
                         compare_errors(df = test_set)[2,2] >= compare_errors(df = test_set)[2,3] ~ F,
                       T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

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

#If sds are higher than when we plotted out of sample, use the higher value
sds <- sds %>%
  mutate(comparer = get_sds(df=yieldspread,print=F,use_model=use_model)$Model) %>%
  mutate(Model = case_when(Model > comparer~Model,
                           T~comparer)) %>%
  select(-comparer)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Analyze IEMG Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "IEMG"
varlist <- c("em_spread","div_yield","em_govt_yield","em_ig_discount","dxy",
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
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

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
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
use_model <- case_when(compare_errors(df = test_set)[5,2] >= compare_errors(df = test_set)[5,3] &
                         compare_errors(df = test_set)[2,2] >= compare_errors(df = test_set)[2,3] ~ F,
                       T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  mutate(em_ig_discount = us_hy_yield - em_ig_yield) %>%
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

#If sds are higher than when we plotted out of sample, use the higher value
sds <- sds %>%
  mutate(comparer = get_sds(df=yieldspread,print=F,use_model=use_model)$Model) %>%
  mutate(Model = case_when(Model > comparer~Model,
                           T~comparer)) %>%
  select(-comparer)

#Based on forecast returns, standard deviations, and risk-free rates, calculate
#expected sharpe ratios
sharpes <- bind_rows(sharpes,get_sharpes())
sharpes
rm(sds)



# Analyze VGK Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "VGK"
varlist <- c("eur_spread","dxy","div_yield",
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
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

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
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
use_model <- case_when(compare_errors(df = test_set)[5,2] >= compare_errors(df = test_set)[5,3] &
                         compare_errors(df = test_set)[2,2] >= compare_errors(df = test_set)[2,3] ~ F,
                       T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

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

#If sds are higher than when we plotted out of sample, use the higher value
sds <- sds %>%
  mutate(comparer = get_sds(df=yieldspread,print=F,use_model=use_model)$Model) %>%
  mutate(Model = case_when(Model > comparer~Model,
                           T~comparer)) %>%
  select(-comparer)

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
  mutate(`Index ETF` = case_when(Ticker=="EMHY"~"EMHY\nEM HY\nBonds",
                            Ticker=="GOVT"~"GOVT\nUS Govt\nBonds",
                            Ticker=="IEMG"~"IEMG\nEM Stocks",
                            Ticker=="VCLT"~"VCLT\nUS Corp\nBonds",
                            Ticker=="VGK"~"VGK\nEU Stocks",
                            Ticker=="VOO"~"VOO\nUS Stocks",
                            Ticker=="EMHY"~"EMHY\nEM HY\nBonds",
                            Ticker=="VWOB"~"VWOB\nEM Govt\nBonds")) %>%
  mutate(`Index ETF` = fct_reorder(`Index ETF`,`Expected return`,.desc=T)) %>%
  ggplot(aes(x=`Index ETF`,y=`Expected return`)) +
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
       units = "in")



# Development Notes -------------------------------------------------------



# In calculating percent dividend yield, should I be using close or adjusted price?

# Near-term development ideas:
# Since VCLT has average maturity 10-15 years, try a yield series in that range
# rather than 7-year yield.
# Dream up some other new variables to test, in the hope of conquering my last
# couple of undefeated naive averages.
# Maybe try a PCE inflation series? A YoY change in yields series?
# Explore using P/E or earnings yield rather than div yield to predict index returns?
# (Dividend yield is problematic because it's a poor proxy for shareholder yield)
# FRED no longer provides ISM manufacturing PMI; I should see if I can automate
# retrieval of this data for testing. (Data through 2015 available here:
# https://data.nasdaq.com/data/FRED/NAPM-ism-manufacturing-pmi-composite-index)
# Also try Fed balance sheet.
# Try reverse repo, maybe with a stochastic transformation

# Future development ideas:
# Try optimizing a rebalance model, maybe using decision trees?
# Would be interesting to see how different US sectors react to yields and yield spreads
