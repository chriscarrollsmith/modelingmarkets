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

# Fetch FRED API key from .Renviron file
# NOTE: The script will only work if you have a valid FRED API key in .Renviron!
readRenviron("~/.Renviron")



# Define functions --------------------------------------------------------



#Select what timeframes to optimize returns over (in years)
#Use a comma-separated vector; optimally no more than four
timeframes_to_optimize <- c(1,2)

#Define function to get and combine multiple FRED series into a single data frame
get_freds <- function(symbols,varnames){
  map2_dfr(symbols,varnames,function(s,v){
    fredr(s) %>%
      mutate(variable = v) %>%
      select(date,value,variable)
  })
}

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
  df1 <- map_dfr(paste0("return_",timeframes_to_optimize,"_yr"),function(returns_to_check){
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
  test_index <- map(paste0("return_",timeframes_to_optimize,"_yr"),function(v){
    splitTimeSeries(yieldspread[v],k=5)
    })

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
    train_set <- map(1:length(test_index),function(item){
      yieldspread[-test_index[[item]][[x]],]
    })
    test_set <- map(1:length(test_index),function(item){
      yieldspread[test_index[[item]][[x]],]
    })

    #Omit from training set any returns that overlap the first date in test set
    train_set <- map2(1:length(timeframes_to_optimize),timeframes_to_optimize,function(a,b){
      train_set[[a]][[paste0("return_",b,"_yr")]] <- suppressWarnings(case_when(between(test_set[[a]]$date[1] - train_set[[a]]$date,0,365*b)~NA_real_,
                                           T~train_set[[a]][[paste0("return_",b,"_yr")]]))
      return(train_set[[a]])
    })

    #Create models on training data, predict on test data
    actuals <- unlist(map2(1:length(timeframes_to_optimize),timeframes_to_optimize,function(a,b){
      test_set[[a]][[paste0("return_",b,"_yr")]]
    }))
    model_forecasts <- function(y){unlist(map2(1:length(timeframes_to_optimize),timeframes_to_optimize,function(a,b){
      predict(lm(formula = paste0("return_",b,"_yr ~ ",paste(varcombos[[y]],collapse="+")),data=train_set[[a]]),newdata=test_set[[a]])
    }))}
    naive_forecasts <- unlist(map2(1:length(timeframes_to_optimize),timeframes_to_optimize,function(a,b){
      rep(mean(train_set[[a]][[paste0("return_",b,"_yr")]],na.rm=T),length(test_set[[a]][[paste0("return_",b,"_yr")]]))
    }))

    #Calculate error
    map_dfr(1:length(varcombos),function(y){
      data.frame(model = paste(varcombos[[y]],collapse="+"),
                 trial = x,
                 error_measure = optimizer(actuals,model_forecasts(y)))
    }) %>% bind_rows(.,data.frame(model = "naive_average",
                                  trial = x,
                                  error_measure = optimizer(actuals,naive_forecasts)))
  })
}

#Define function to forecast models on either yieldspread or test_set and convert
#to tidy data frame for charting
tidify <- function(df){

  df[,c("date",
        paste0("return_",timeframes_to_optimize,"_yr"),
        varlist)] %>%
    gather("timeframe","value",paste0("return_",timeframes_to_optimize,"_yr")) %>%
    left_join(.,data.frame(timeframe = paste0("return_",timeframes_to_optimize,"_yr"),
                           time_length = timeframes_to_optimize,
                           index = 1:length(timeframes_to_optimize))) %>%
    group_by(timeframe) %>%
    mutate(naive_forecast = mean(yieldspread[[first(timeframe)]],na.rm=T)) %>%
    mutate(forecast = predict(object=models[[first(index)]],newdata=cur_data())) %>%
    ungroup() %>%
    filter(!is.na(forecast)) %>%
    mutate(label = case_when(date == max(date) ~ paste0(scales::percent(round(forecast,2),accuracy=1)," expected\n,",time_length,"-year ARR"),
                             TRUE ~ NA_character_)) %>%
    mutate(timeframe = paste0(time_length,"-year forward return")) %>%
    mutate(timeframe = factor(timeframe,levels=paste0(timeframes_to_optimize,"-year forward return"))) %>%
    select(-time_length)

}

#Define function to chart model forecast against a selected focus variable, holding
#other model inputs steady at the last observed value. User specifies whether
#chart is using test_set or yieldspread (in-sample or out-of-sample data).
chartit <- function(df=test_set,insample=F){

  #Prep new_data, using today's value for forecasting non-focus variables but
  #historical values for forecasting the focus variable
  new_data <- df %>%
    .[c(varlist,"index")]
  for(x in 1:length(new_data)){
    if(names(new_data[,x]) != focus_var &
       names(new_data[,x]) != "index"){new_data[,x] <- last(new_data[!is.na(new_data[,x]),x])}
  }

  #Calculate adjusted forecasts, subtracting non-focus variable effects
  df$adj_forecast <- new_data %>%
    group_by(index) %>%
    mutate(adj_forecast = predict(object=models[[first(index)]],newdata=cur_data())) %>%
    ungroup() %>%
    pull(adj_forecast)

  #Chart forward actual and modeled returns against focus variable
  df %>%
    mutate(value=value+(adj_forecast-forecast)) %>%
    mutate(highlight = !is.na(label)) %>%
    ggplot(aes(x=.data[[focus_var]],y=value)) +
    geom_point() +
    geom_line(aes(y=adj_forecast),col="blue",lwd=1) +
    geom_point(aes(y=adj_forecast,color=highlight,alpha=highlight),size=3) +
    geom_hline(aes(yintercept=adj_forecast,color=highlight,alpha=highlight),lwd=1,linetype="dashed") +
    geom_text(aes(y=adj_forecast,label=label),col="red",size=4,fontface = "bold",hjust="inward") +
    facet_wrap(vars(timeframe),nrow=ceiling(length(timeframes_to_optimize)/2)) +
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
  sharpe <- sds
  # sharpe <- left_join(sds,risk_free_rate)

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



# Download FRED data ------------------------------------------------------



#Get a bunch of variables from FRED to test as model inputs
yieldspread_full <- get_freds(symbols = c("BAMLH0A0HYM2","BAMLEMHYHYLCRPIUSOAS",
                                          "BAMLHE00EHYIOAS","BAMLH0A0HYM2EY",
                                          "BAMLEMHBHYCRPIEY","BAMLC0A0CMEY",
                                          "BAMLEMIBHGCRPIEY","BAMLEMPBPUBSICRPIEY",
                                          "USALOLITONOSTSAM","OECDELOLITONOSTSAM",
                                          "BOGZ1FL153064486Q","PINDUINDEXM",
                                          "PRAWMINDEXM","DCOILBRENTEU",
                                          "DEXUSEU","PNGASEUUSDM","BAMLHE00EHYIEY",
                                          "ECBASSETSW","IRLTLT01EZM156N",
                                          "DGS6MO","DGS1","DGS2","DGS3",
                                          "DGS7","DGS10","DGS20",
                                          "VIXCLS","DTWEXBGS","WALCL",
                                          "CPHPTT01EZM659N","PCETRIM12M159SFRBDAL",
                                          "MICH","T10YIEM","PCEPI",
                                          "CP0000EZ19M086NEST"),
                              varnames = c("us_spread","em_spread",
                                           "eur_spread","us_hy_yield",
                                           "em_hy_yield","us_ig_yield",
                                           "em_ig_yield","em_govt_yield",
                                           "cli_lead_us","eur_cli",
                                           "us_equities_allocation","industrial_materials",
                                           "raw_materials","brent_crude",
                                           "usd_eur","nat_gas","eur_hy_yield",
                                           "ecb_bs","eur_10yr_yield",
                                           "yield6mo","yield1yr",
                                           "yield2yr","yield3yr",
                                           "yield7yr","yield10yr","yield20yr",
                                           "vix","dxy","fed_bs",
                                           "eur_cpi_12mo","us_pce_12m",
                                           "u_mich_inflation","breakeven_10yr",
                                           "us_pce_3mo","eur_cpi_3mo")) %>%
  #Convert percentage values to decimal
  mutate(value = case_when(variable %in% c("us_spread","em_spread",
                                           "eur_spread","us_hy_yield",
                                           "em_hy_yield","us_ig_yield",
                                           "em_ig_yield","em_govt_yield",
                                           "eur_10yr_yield","yield6mo",
                                           "yield1yr","yield2yr","yield3yr",
                                           "yield7yr","yield10yr","yield20yr",
                                           "eur_cpi_12mo","us_pce_12m",
                                           "u_mich_inflation","breakeven_10yr")~value/100,
                           T~value)) %>%
  #Adjust dates for FRED series to reflect release dates, not reference dates
  mutate(date = case_when(variable %in% c("eur_cli","industrial_materials",
                                          "raw_materials","nat_gas",
                                          "us_pce_12m","us_pce_3mo",
                                          "u_mich_inflation")~date %m+% months(2),
                          variable %in% c("cli_lead_us","breakeven_10yr")~date %m+% months(1) %m+% days(6),
                          variable %in% c("us_spread","em_spread",
                                          "eur_spread","us_hy_yield",
                                          "em_hy_yield","us_ig_yield",
                                          "em_ig_yield","em_govt_yield",
                                          "eur_hy_yield","eur_10yr_yield",
                                          "yield6mo","yield1yr",
                                          "yield2yr","yield3yr",
                                          "yield7yr","yield10yr","yield20yr",
                                          "vix")~date %m+% days(1),
                          variable == "us_equities_allocation"~date %m+% months(2) %m+% days(6),
                          variable %in% c("brent_crude","ecb_bs")~date %m+% days(7),
                          variable %in% c("usd_eur","dxy","fed_bs")~date %m+% days(4),
                          variable %in% c("eur_cpi_12mo")~date %m+% months(2) %m+% weeks(2),
                          variable %in% c("eur_cpi_3mo")~date %m+% months(1) %m+% weeks(3),
                          T~date)) %>%
  mutate(date = case_when(date > Sys.Date()~Sys.Date(),
                          T~date)) %>%
  #Rearrange table for ML model training
  spread(key = variable,value=value) %>%
  #Get rid of negative values that will screw up log transformation
  mutate(cli_lead_us = (cli_lead_us-min(cli_lead_us,na.rm=T))/(max(cli_lead_us,na.rm=T)-min(cli_lead_us,na.rm=T))+1,
         eur_cli = (eur_cli-min(eur_cli,na.rm=T))/(max(eur_cli,na.rm=T)-min(eur_cli,na.rm=T))+1,
         eur_cpi_12mo = (eur_cpi_12mo-min(eur_cpi_12mo,na.rm=T))/(max(eur_cpi_12mo,na.rm=T)-min(eur_cpi_12mo,na.rm=T))+1)

#Create a three-month moving average of month-over-month change variable for PCE price index
yieldspread_full$us_pce_3mo[!is.na(yieldspread_full$us_pce_3mo)] <- (yieldspread_full$us_pce_3mo[!is.na(yieldspread_full$us_pce_3mo)] -
  lag(yieldspread_full$us_pce_3mo[!is.na(yieldspread_full$us_pce_3mo)],n=1))/lag(yieldspread_full$us_pce_3mo[!is.na(yieldspread_full$us_pce_3mo)],n=1)
yieldspread_full$us_pce_3mo[!is.na(yieldspread_full$us_pce_3mo)] <- EMA(yieldspread_full$us_pce_3mo[!is.na(yieldspread_full$us_pce_3mo)],n=3)

#Create a three-month moving average of month-over-month change variable for Europe CPI price index
yieldspread_full$eur_cpi_3mo[!is.na(yieldspread_full$eur_cpi_3mo)] <- (yieldspread_full$eur_cpi_3mo[!is.na(yieldspread_full$eur_cpi_3mo)] -
                                                                       lag(yieldspread_full$eur_cpi_3mo[!is.na(yieldspread_full$eur_cpi_3mo)],n=1))/lag(yieldspread_full$eur_cpi_3mo[!is.na(yieldspread_full$eur_cpi_3mo)],n=1)
yieldspread_full$eur_cpi_3mo[!is.na(yieldspread_full$eur_cpi_3mo)] <- EMA(yieldspread_full$eur_cpi_3mo[!is.na(yieldspread_full$eur_cpi_3mo)],n=3)

#Get rid of negative values that will screw up log transformation
yieldspread_full <- yieldspread_full %>%
  mutate(us_pce_3mo = (us_pce_3mo-min(us_pce_3mo,na.rm=T))/(max(us_pce_3mo,na.rm=T)-min(us_pce_3mo,na.rm=T))+1,
         eur_cpi_3mo = (eur_cpi_3mo-min(eur_cpi_3mo,na.rm=T))/(max(eur_cpi_3mo,na.rm=T)-min(eur_cpi_3mo,na.rm=T))+1)

# See how correlated my yield spread variables are (.93 - .97)
cor(yieldspread_full[c("us_spread","em_spread","eur_spread")],use="complete.obs")




# Calculate Risk-Free Rates ----------------------------------------------



# #Make sure yieldspread_full is sorted by date
# yieldspread_full <- yieldspread_full %>%
#   arrange(date)
#
# #Put risk-free returns into a table
# #Use annualized returns, not total returns, for comparability
# risk_free_rate <- data.frame(timeframe = c("0.5-year forward return",
#                                            "1-year forward return",
#                                            "2-year forward return",
#                                            "3-year forward return"),
#                              risk_free_return = c(last(yieldspread_full$yield6mo),
#                                                   last(yieldspread_full$yield1yr),
#                                                   last(yieldspread_full$yield2yr),
#                                                   last(yieldspread_full$yield3yr)))



# Assess money market returns ---------------------------------------------



#Get money market returns, using .9 of Fed target range lower limit as a proxy
money_market <- fredr("DFEDTARL",frequency = "m") %>%
  select(date,value) %>%
  mutate(value = (value/100)*.9) %>%
  mutate(ahead_1mo = lead(value,1),
         ahead_2mo = lead(value,2),
         ahead_3mo = lead(value,3),
         ahead_4mo = lead(value,4),
         ahead_5mo = lead(value,5),
         ahead_6mo = lead(value,6),
         ahead_7mo = lead(value,7),
         ahead_8mo = lead(value,8),
         ahead_9mo = lead(value,9),
         ahead_10mo = lead(value,10),
         ahead_11mo = lead(value,11)) %>%
  mutate(avg_1yr_return = rowMeans(.[,2:13])) %>%
  mutate(timeframe = "1-year forward return") %>%
  mutate(naive_forecast = mean(avg_1yr_return,na.rm=T)) %>%
  mutate(forecast = predict(lm(avg_1yr_return ~ value,.),.))

# #Predict and plot returns model
money_market %>%
  ggplot(aes(x=value,y=forecast)) +
  geom_line(col="blue") +
  geom_point(aes(y=avg_1yr_return))

#Calculate Sharpe ratio
expected_return <- last(money_market$forecast[!is.na(money_market$forecast)])
expected_sd <- get_sds(money_market %>% filter(value > 0))[[1,2]]
sharpes <- tibble(Ticker = "SPAXX",
                      Model = "fed_rate",
                      Timeframe = "1-year forward return",
                      `Expected return` = expected_return,
                      `Expected standard deviation` = expected_sd) %>%
  mutate(`Expected Sharpe` = `Expected return`/`Expected standard deviation`)
rm(expected_return,expected_sd,money_market)



# Download price data -----------------------------------------------------



#Get historical price data
prices <- tq_get(c("VOO","VGK","VIOO","VTWO","GOVT","VCLT","HYG","VWOB","EMHY","IEMG","TLT","BSV"))

# get today's prices
last_price <- getQuote(c("VOO","VGK","VIOO","VTWO","GOVT","VCLT","HYG","VWOB","EMHY","IEMG","TLT","BSV")) %>%
  clean_names() %>%
  mutate(symbol = rownames(.),
         date = as.Date(trade_time),
         close=last,
         adjusted=last) %>%
  mutate(open = round(open,digits=2),
         high = round(high,digits=2),
         low = round(low,digits=2),
         close = round(close,digits=2),
         adjusted = round(adjusted,digits=2)) %>%
  select(symbol,date,open,high,low,close,adjusted,volume)

#Combine today's prices with the price history data frame
prices <- if(max(last_price$date) > max(prices$date)){
  bind_rows(prices,last_price)
}else{prices}

#calculate next period return
#To calculate simple annualized rate of return s from total y-year return r, s=r/y
#To calculate CAGR c from simple annualized rate of return s, c=(s*y+1)^(1/y)-1
#Or to get CAGR c from total return r, c=(r+1)^(1/y)-1
#For now, use simple annualized rate of return
prices <- map(timeframes_to_optimize,function(t){
  return <- prices %>%
    group_by(symbol) %>%
    mutate(return = c(diff(adjusted,lag = round(252*t)),rep(NA,times=round(252*t)))/adjusted) %>%
    mutate(return = return/t) %>%
    pull(return)
}) %>%
  set_names(paste0("return_",timeframes_to_optimize,"_yr")) %>%
  as_tibble() %>%
  bind_cols(prices,.)

#Join return data with high-yield spread data
prices <- prices[c("symbol","close","adjusted","date",paste0("return_",timeframes_to_optimize,"_yr"))]
yieldspread_full <- full_join(yieldspread_full,prices,by="date")
rm(prices)



# Download ETF dividend yields --------------------------------------------



#Get dividend history for IEMG, VOO, VTWO, VIOO, and VGK from Yahoo! Finance
divs <- tq_get(c("VOO","IEMG","VIOO","VTWO","VGK"),get="dividends")

#Since Yahoo! Finance data isn't split-adjusted but the close price data is, I
#need to split-adjust pre-April 20, 2021 dividends by multiplying by 4
divs <- divs %>%
  mutate(value = case_when(symbol == "VTWO" & date < as.Date("2021-04-20") ~ value*4,
                           T ~ value))

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



#Remove all data prior to 2010 (because we only have returns from 2013 on)
yieldspread_full <- yieldspread_full %>%
  filter(!date < as.Date("2010-01-01"))

#Create a data frame containing every date from 2010 till the present
dates <- as_tibble(data.table::CJ(date = seq.Date(as.Date("2010-01-01"),Sys.Date(),by="days"),
                                  symbol=c("VOO","VGK","VIOO","VTWO","GOVT","VCLT","HYG","VWOB","EMHY","IEMG","TLT")))

#Join dates dataframe with yieldspread_full, and arrange in ascending order by date
yieldspread_full <- yieldspread_full %>%
  full_join(dates,by=c("date","symbol")) %>%
  filter(!is.na(symbol)) %>%
  arrange(date)

#Interpolate last non-NA value for model inputs (but *not* for returns)
#(Why am I ending up with missing values for some of these?)
yieldspread_full <- yieldspread_full %>%
  fill(us_spread,em_spread,eur_spread,cli_lead_us,us_equities_allocation,
       industrial_materials,raw_materials,yield6mo,em_govt_yield,em_hy_yield,
       yield1yr,yield2yr,yield3yr,yield7yr,vix,dxy,em_ig_yield,fed_bs,
       brent_crude,usd_eur,nat_gas,eur_hy_yield,ecb_bs,eur_10yr_yield,eur_cli,
       us_hy_yield,us_ig_yield,yield10yr,yield20yr,us_pce_12m,eur_cpi_12mo,
       u_mich_inflation,breakeven_10yr,us_pce_3mo,eur_cpi_3mo) %>%
  group_by(symbol) %>%
  fill(div_yield)

#Convert to percent dividend yields
yieldspread_full <- yieldspread_full %>%
  mutate(div_yield = div_yield/close)

#Visualize dividend yields
yieldspread_full %>%
  filter(!is.na(div_yield) & symbol != "VGK") %>%
  ggplot(aes(x=date,y=div_yield)) +
  geom_line() + facet_wrap(vars(symbol))



# Calculate Trailing Period Change Variables ------------------------------



# #Create a vector of variables to try transforming to period-over-period change,
# #and a corresponding list of which ETFs to train each variable on.
# vars_to_tune <- c("us_equities_allocation","vix","fed_bs", #US only
#                   "ecb_bs","usd_eur", #Europe only
#                   "dxy","industrial_materials","raw_materials","brent_crude",
#                   "nat_gas") #US, Europe, and EM
# etfs_to_tune_on <- list(c("VOO","TLT","GOVT","VCLT"),
#                         c("VOO","TLT","GOVT","VCLT"),
#                         c("VOO","TLT","GOVT","VCLT"),
#                         c("VGK"),
#                         c("VGK"),
#                         c("VOO","TLT","GOVT","VGK","IEMG","EMHY","VWOB"),
#                         c("VOO","TLT","GOVT","VGK","IEMG","EMHY","VWOB"),
#                         c("VOO","TLT","GOVT","VGK","IEMG","EMHY","VWOB"),
#                         c("VOO","TLT","GOVT","VGK","IEMG","EMHY","VWOB"),
#                         c("VOO","TLT","GOVT","VGK","IEMG","EMHY","VWOB"))
#
# #Tune the right lengths for period-over-period change variables, then calculate.
# #(I simply tune by maximizing correlation coefficient, though a full-fledged
# #cross-validation would be better.)
# optimal_lengths <- map2_dfr(vars_to_tune,etfs_to_tune_on,function(v,e){
#
#   #Select only the variables we're interested in, standardize variable name
#   yieldspread_full <- yieldspread_full[c("date",v,"return_0.5_yr","return_1_yr",
#                                          "return_2_yr","return_3_yr","symbol")]
#   names(yieldspread_full) <- c("date","ourvar","return_0.5_yr","return_1_yr",
#                                "return_2_yr","return_3_yr","symbol")
#
#   #For lengths in months from 1 to 24, find the length that maximizes correlation
#   #across multiple ETFs
#   optimal_length <- map_dfr(1:24,function(length){
#
#     #Calculate a period-over-period change variable of the given length
#     yieldspread_full <- yieldspread_full %>%
#       group_by(symbol) %>%
#       mutate(change_var = ourvar - lag(ourvar,round(length*365.25/12)))
#
#     #For each of the various ETFs we're targeting, find the average of the absolute
#     #values of our change variables's correlation to returns
#     avg_cor <- map(e,function(etf){
#       corchecker(yieldspread_full,ticker = etf,vars_to_test = "change_var")["avg_of_abs_vals"]
#     }) %>% unlist() %>% mean()
#
#     return(data.frame(len = length,cor = avg_cor))
#
#   }) %>% slice_max(cor) %>% pull(len)
#
#   return(data.frame(variable = v,length = optimal_length))
#
# })
#
# #Calculate change variables of the desired lengths
# #Get rid of negative values that will screw up log transformation
# change_vars <- map(1:nrow(optimal_lengths),function(n){
#   df <- yieldspread_full[c("date","symbol",optimal_lengths[n,1])] %>%
#     group_by(symbol)
#   df[3] <- df[3] - lag(df[3],round(optimal_lengths[n,2]*365.25/12))
#   df[3] <- (df[3]-min(df[3],na.rm=T))/(max(df[3],na.rm=T)-min(df[3],na.rm=T))+1
#   return(df)
# }) %>% reduce(full_join,by=c("date","symbol"))
#
# #Assign descriptive names to change variables
# optimal_lengths <- optimal_lengths %>%
#   mutate(variable = paste0("change_in_",variable))
# names(change_vars) <- c("date","symbol",optimal_lengths$variable)
#
# #Join change variables with yieldspread_full
# yieldspread_full <- full_join(yieldspread_full,change_vars)
#
# #NA prices can either stay in the data frame or not, but perhaps best to
# #remove them (only after performing all tuning/transformations.)
# yieldspread_full <- yieldspread_full %>%
#   filter(!is.na(adjusted))



# Analyze S&P 500 Returns -------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "VOO"
varlist <- c("us_spread","yield7yr","dxy","vix","div_yield",
             "cli_lead_us","industrial_materials","raw_materials","us_pce_12m",
             "u_mich_inflation","breakeven_10yr","us_pce_3mo"
             # ,"change_in_us_equities_allocation","change_in_vix",
             # "change_in_fed_bs","change_in_dxy",
             # "change_in_industrial_materials","change_in_raw_materials",
             # "change_in_brent_crude","change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- yieldspread %>%
  corchecker(ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

#Call function to use five-fold cross-validation to compare error rates on linear
#models that combine the selected variables
errors <- cross_validator(varlist)

#Choose model that minimizes error on average
errors <- errors %>%
  group_by(model) %>%
  summarize(mean_error = mean(error_measure)) %>%
  arrange(mean_error)
errors %>% filter(rank(mean_error)<=10) %>% knitr::kable(caption = paste("Mean",global_optimizer[[2]],"by model"))
model_type <- errors$model[errors$model != "naive_average"][which.min(errors$mean_error[errors$model != "naive_average"])]
use_model <- case_when(errors$model[which.min(errors$mean_error)] == "naive_average" ~ F,
                       T ~ T)
varlist <- varlist[str_detect(model_type,varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
yieldspread <- tidify(yieldspread)

#Choose focus variable for charting, then create chart
focus_var <- varlist[2]
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



# Analyze GOVT Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "GOVT"
varlist <- c("us_spread","yield7yr","dxy","vix","spread7_10",
             "cli_lead_us","industrial_materials","raw_materials","us_pce_12m",
             "u_mich_inflation","breakeven_10yr","us_pce_3mo"
             # ,"change_in_us_equities_allocation","change_in_vix",
             # "change_in_fed_bs","change_in_dxy",
             # "change_in_industrial_materials","change_in_raw_materials",
             # "change_in_brent_crude","change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  mutate(spread7_10 = yield10yr - yield7yr) %>%
  mutate(spread7_10 = (spread7_10-min(spread7_10,na.rm=T))/(max(spread7_10,na.rm=T)-min(spread7_10,na.rm=T))+1) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  mutate(spread7_10 = yield10yr - yield7yr) %>%
  mutate(spread7_10 = (spread7_10-min(spread7_10,na.rm=T))/(max(spread7_10,na.rm=T)-min(spread7_10,na.rm=T))+1) %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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



# Analyze BSV Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "BSV"
varlist <- c("us_spread","yield2yr","yield3yr","dxy","vix","spread2_10",
             "cli_lead_us","industrial_materials","raw_materials","us_pce_12m",
             "u_mich_inflation","breakeven_10yr","us_pce_3mo"
             # ,"change_in_us_equities_allocation","change_in_vix",
             # "change_in_fed_bs","change_in_dxy",
             # "change_in_industrial_materials","change_in_raw_materials",
             # "change_in_brent_crude","change_in_nat_gas"
)

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  mutate(spread2_10 = yield10yr - yield2yr) %>%
  mutate(spread2_10 = (spread2_10-min(spread2_10,na.rm=T))/(max(spread2_10,na.rm=T)-min(spread2_10,na.rm=T))+1) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  mutate(spread2_10 = yield10yr - yield2yr) %>%
  mutate(spread2_10 = (spread2_10-min(spread2_10,na.rm=T))/(max(spread2_10,na.rm=T)-min(spread2_10,na.rm=T))+1) %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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


# Analyze TLT Returns ----------------------------------------------------



#Define a ticker and a list of variables to study
ticker_to_study <- "TLT"
varlist <- c("us_spread","yield20yr","dxy","vix",
             "cli_lead_us","industrial_materials","raw_materials","us_pce_12m",
             "u_mich_inflation","breakeven_10yr","us_pce_3mo"
             # ,"change_in_us_equities_allocation","change_in_vix",
             # "change_in_fed_bs","change_in_dxy",
             # "change_in_industrial_materials","change_in_raw_materials",
             # "change_in_brent_crude","change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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
             "industrial_materials","raw_materials"
             # ,"change_in_dxy",
             # "change_in_industrial_materials","change_in_raw_materials",
             # "change_in_brent_crude","change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only tthe top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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
varlist <- c("yield10yr","yield20yr","us_spread","us_ig_yield","dxy","vix",
             "cli_lead_us","raw_materials","industrial_materials","us_pce_12m",
             "u_mich_inflation","breakeven_10yr","us_pce_3mo"
             # ,"change_in_us_equities_allocation","change_in_vix",
             # "change_in_fed_bs","change_in_dxy",
             # "change_in_industrial_materials","change_in_raw_materials",
             # "change_in_brent_crude","change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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
varlist <- c("em_spread","em_hy_yield","em_govt_yield","raw_materials","industrial_materials"
             # ,"change_in_dxy","change_in_industrial_materials",
             # "change_in_raw_materials","change_in_brent_crude",
             # "change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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
             "raw_materials","industrial_materials","div_premium"
             # ,"change_in_dxy","change_in_industrial_materials",
             # "change_in_raw_materials","change_in_brent_crude",
             # "change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
#Try out variable subtracting EM investment-grade yield from US high yield
#(Perhaps this works because EM IG companies compete with the junkiest US sectors,
#like mining and oil and gas?)
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  mutate(em_ig_discount = us_hy_yield - em_ig_yield) %>%
  full_join(yieldspread_full %>%
              filter(symbol %in% c(ticker_to_study,"VOO")) %>%
              select(date,symbol,div_yield) %>%
              spread(symbol,div_yield) %>%
              mutate(div_premium = IEMG - VOO) %>%
              mutate(div_premium = (div_premium-min(div_premium,na.rm=T))/(max(div_premium,na.rm=T)-min(div_premium,na.rm=T))+1) %>%
              select(date,div_premium)) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficient between variables of interest and next-period return
cors <- yieldspread %>%
  corchecker(ticker = ticker_to_study,
             vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  mutate(em_ig_discount = us_hy_yield - em_ig_yield) %>%
  full_join(yieldspread_full %>%
              filter(symbol %in% c(ticker_to_study,"VOO")) %>%
              select(date,symbol,div_yield) %>%
              spread(symbol,div_yield) %>%
              mutate(div_premium = IEMG - VOO) %>%
              mutate(div_premium = (div_premium-min(div_premium,na.rm=T))/(max(div_premium,na.rm=T)-min(div_premium,na.rm=T))+1) %>%
              select(date,div_premium)) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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
varlist <- c("eur_spread","dxy","div_yield","raw_materials","industrial_materials",
             "brent_crude","usd_eur","nat_gas","eur_hy_yield","ecb_bs",
             "eur_10yr_yield","eur_cli","eur_cpi_12mo","eur_cpi_3mo"
             # ,"change_in_ecb_bs","change_in_usd_eur","change_in_dxy",
             # "change_in_industrial_materials","change_in_raw_materials",
             # "change_in_brent_crude","change_in_nat_gas"
             )

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Partition time series for training, testing, and validation
test_set <- yieldspread[yieldspread$date >= max(yieldspread$date) - years(4),]
yieldspread <- yieldspread[yieldspread$date < max(yieldspread$date) - years(4),]

#Calculate correlation coefficients between variables of interest and next-period return
cors <- corchecker(df = yieldspread,
                   ticker = ticker_to_study,
                   vars_to_test = varlist)

#Print correlation coefficients in readable format
cors %>%
  knitr::kable(caption = paste("Correlation coefficients for various variables and next-period",ticker_to_study,"returns"))

# #Winnow list of variables to study, keeping only the top 5
# varlist <- row.names(cors[1:5,])

#Delete correlation coefficients table from workspace
rm(cors)

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
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

#Forecast returns on test set and convert to tidy data frame
test_set <- tidify(test_set)

#Choose focus variable for charting, then create chart
focus_var <- varlist[1]
chartit(df = test_set,insample=F)

#See if our model forecast performed better than a naive average
compare_errors(df = test_set) %>% kable(caption = paste0("Comparing model forecast to naive forecast ",global_optimizer[[2]]))
# use_model <- case_when(compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),2] >= compare_errors(df = test_set)[nrow(compare_errors(df = test_set)),3]~F,
#                        T ~ use_model)

#Calculate model standard deviations
sds <- get_sds(df=test_set,use_model=use_model)

#Retrain yield spread model on full data set (without partitioning), chart
#historical forecast against historical data, and forecast returns and sharpe
#ratio for the current date

#Filter dataset to keep only the data we're interested in
yieldspread <- yieldspread_full %>%
  filter(symbol == ticker_to_study) %>%
  .[c("date","symbol",
      paste0("return_",timeframes_to_optimize,"_yr"),
      varlist)]

#Retrain the optimal model on the full training data set
models <- map(timeframes_to_optimize,function(t){
  lm(yieldspread,formula = paste0("return_",t,"_yr ~ ",model_type))
})

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
  mutate(`Asset class` = case_when(Ticker=="EMHY"~"Med-Term EM HY Bonds",
                                   Ticker=="GOVT"~"Med-Term US Govt Bonds",
                                   Ticker=="TLT"~"Long-Term US Govt Bonds",
                                   Ticker=="IEMG"~"EM Stocks",
                                   Ticker=="VCLT"~"Long-Term US Corp Bonds",
                                   Ticker=="VGK"~"EU Stocks",
                                   Ticker=="VOO"~"US Stocks",
                                   Ticker=="EMHY"~"EM HY Bonds",
                                   Ticker=="VWOB"~"EM Govt Bonds",
                                   Ticker=="BSV"~"Short-Term US Govt Bonds",
                                   Ticker=="SPAXX"~"Money-Market")) %>%
  select(`Asset class`,Ticker,Model,`Expected return`,`Expected standard deviation`,`Expected Sharpe`) %>%
  arrange(desc(`Expected Sharpe`)) %>%
  kable(caption = "Sharpe ratios, 1-year timeframe")

#Plot forecasted one-year returns and standard deviations
sharpes %>%
  mutate(`Index ETF` = case_when(Ticker=="EMHY"~"EMHY\nMed-Term\nEM HY\nBonds",
                            Ticker=="GOVT"~"GOVT\nMed-Term\nUS Govt\nBonds",
                            Ticker=="TLT"~"TLT\nLong-Term\nUS Govt\nBonds",
                            Ticker=="IEMG"~"IEMG\nEM Stocks",
                            Ticker=="VCLT"~"VCLT\nLong-Term\nUS Corp\nBonds",
                            Ticker=="VGK"~"VGK\nEU Stocks",
                            Ticker=="VOO"~"VOO\nUS Stocks",
                            Ticker=="EMHY"~"EMHY\nMed-Term\nEM HY\nBonds",
                            Ticker=="VWOB"~"VWOB\nMed-Term\nEM Govt\nBonds",
                            Ticker=="BSV"~"BSV\nShort-Term\nUS Govt\nBonds",
                            Ticker=="SPAXX"~"Money-\nMarket")) %>%
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



# Near-term development ideas:

# Quite a few FRED series update on a lag from the actual release (e.g., commodities
# prices). Try to figure out how to scrape the actual release to get a faster/
# fresher signal.

# Maybe just remove the VGK special dividend?
# Europe natural gas data series lags by 1-2 months, unfortunately. Either add a month
# to all dates, use Henry Hub spot price from FRED instead, or find a series of
# TTF gas spot prices that I can scrape with rvest and convert to US dollars

# Since Yahoo! Finance dividend data starts at 2013, go back to fetching from Nasdaq.com

# Try out the spreads between various dividend/bond yields?
# See if I can get data on dividend yield futures?

# I need to handle tuning the change variables at the same time that I do
# cross-validation for model building, so as to avoid overtraining. First train
# these variables, then train bivariate models. Maybe take the top few bivariate
# models and try multivariate models with those variables?

# My change variables all perform very poorly, which makes me a little suspicious
# of how they're being calculated. Make sure they're being calculated correctly.

# Explore using P/E or earnings yield rather than div yield to predict index returns?
# (Dividend yield is problematic because it's a poor proxy for shareholder yield)
# Can I find a higher-frequency P/E series than multpl.com? Can I find series for
# Europe & EM? Earnings yield preferable to P/E.

# Try reverse repo, maybe with a stochastic transformation

# Future development ideas:

# Try optimizing a rebalance model, maybe using decision trees?
# Would be interesting to see how different US sectors react to yields and yield spreads
