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
sp500tickers <- case_when(
  #replace dots with dashes in select ticker symbols
  sp500tickers == "BRK.B" ~ "BRK-B",
  sp500tickers == "BF.B" ~ "BF-B",
  #TRUE is equivalent to "else" statement
  TRUE ~ as.character(sp500tickers)
  )

#Get historical price data for all tickers
df <- tq_get(tickers$Symbol, from = start_date, to = Sys.Date())

#Combine info from Wikipedia with our price data
#by joining the two tables using symbol as reference point
#select desired columns and convert symbol to factor
df <- df %>%
  left_join(tickers, by = c('symbol' = 'Symbol')) %>%
  clean_names() %>%
  select(symbol,security,date,open,high,low,close,gics_sector,gics_sub_industry) %>%
  mutate(symbol = as.factor(symbol)) %>%
  mutate(gics_sector = as.factor(gics_sector))

#add a column to show percent change since period start
df <- df %>%
  group_by(symbol) %>%
  mutate(pctchange = close/close[1])

#construct vector of equal weight average percent change by sector by date
sector_df <- df %>%
  group_by(gics_sector,date) %>%
  summarize(pctchange = mean(pctchange))

#Summarize total sector returns in a table
sector_df %>%
  summarize(return = last(pctchange)-1) %>%
  arrange(desc(return)) %>%
  mutate(return = scales::percent(return)) %>%
  knitr::kable()

#Pull vector of sectors in order of final returns for use in reordering factor
refactorer <- sector_df %>%
  summarize(return = last(pctchange)-1) %>%
  arrange(desc(return)) %>%
  mutate(return = scales::percent(return)) %>%
  pull(gics_sector) %>%
  as.character()

#plot sector pct change since period start
sector_df %>%
  mutate(pctchange = pctchange - 1) %>%
  mutate(gics_sector = factor(gics_sector,levels=refactorer)) %>%
  ggplot(aes(x = date,y = pctchange,color=gics_sector)) +
  geom_line(linewidth=1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y") +
  labs(title="S&P 500 sector return (equal weight) during the Covid-19 pandemic",x="Date",y="Percent change",color="Sector") +
  theme_bw()

#Export chart as image file
ggsave(
  filename = "C19SectorReturns.jpg",
  plot = last_plot(),
  scale = 1,
  width = 1920/144,
  height = 1080/144,
  units = "in"
)
