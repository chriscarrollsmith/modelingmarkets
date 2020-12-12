#Scrape S&P 500 tickers and sectors from Wikipedia
#Download Yahoo Finance price data since beginning of Covid-19 pandemic
#Chart average (equal weight) returns by sector

#for scraping
library(rvest)
#tidy charts and tables
library(tidyverse)
#tidy financial analysis 
library(tidyquant)
#tidy data cleaning functions
library(janitor)

#Save February 19, 2020 to a date variable
start_date = as.Date("2020/02/19","%Y/%m/%d")

#scrape Wikipedia page for table of S&P500 tickers
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
tickers <- as.data.frame(url %>%
  read_html() %>% #pull all HTML from the webpage
  html_nodes(xpath = '//*[@id="constituents"]') %>% 
  html_table()) #get table using XPath copied from Chrome "inspect" of table HTML

#create a vector of tickers
sp500tickers <- tickers[["Symbol"]]
sp500tickers = case_when(
  #replace dots with dashes in select ticker symbols
  sp500tickers == "BRK.B" ~ "BRK-B",
  sp500tickers == "BF.B" ~ "BF-B",
  #TRUE is equivalent to "else" statement
  TRUE ~ as.character(sp500tickers)
  )

#Define function that returns a data frame of 
#daily closing prices for a ticker
get_prices <- function(ticker){
  #tidyquant function tq_get pulls price data 
  #for a ticker from Yahoo Finance
  df = tq_get(ticker, from = start_date, to = Sys.Date()) %>% 
  #Add a column with ticker symbol repeated for 
  #number of items in data frame's "close" column
  mutate(symbol = rep(ticker, length(close)))
}

#Call function to create price data frame for each ticker, 
#and then bind all data frames together into one
tickers_df = map(sp500tickers, get_prices) %>%
  bind_rows()

#Combine info from Wikipedia with our price data
#by joining the two tables using symbol as reference point
#select desired columns and convert symbol to factor
tickers_df = tickers_df %>%
  left_join(tickers, by = c('symbol' = 'Symbol')) %>%
  clean_names() %>%
  select(symbol,security,date,open,high,low,close,gics_sector,gics_sub_industry) %>%
  mutate(symbol = as.factor(symbol)) %>%
  mutate(gics_sector = as.factor(gics_sector))

#adds a column to show percent change since period start
tickers_df <- tickers_df %>% 
  group_by(symbol) %>%
  mutate(pctchange = close/close[1])

#constructs vector of equal weight average percent change by sector by date
sector_df <- tickers_df %>%
  group_by(gics_sector,date) %>%
  summarize(pctchange = mean(pctchange))

#plots sector pctchange since period start
sector_df %>%
  ggplot(aes(x = date,y = pctchange,color=gics_sector)) +
  geom_line() + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(title="S&P 500 sector return (equal weight) during the Covid-19 pandemic",x="Date",y="Percent change",color="Sector") +
  theme_bw()

#Export chart as image file
ggsave(
  filename = "C19SectorReturns.jpg",
  plot = last_plot(),
  path = "C:/Users/chris/OneDrive/Pictures/Infographics",
  scale = 1,
  width = 1920/144,
  height = 1080/144,
  units = "in"
)