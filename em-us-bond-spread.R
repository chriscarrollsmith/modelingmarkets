#Load libraries
library(tidyquant)
library(tidyverse)
library(lubridate)

#Get historical price data for VWOB and GOVT from Yahoo! Finance
df <- tq_get(c("GOVT","VWOB"))

#Get and clean today's price data for VWOB and GOVT from Yahoo! Finance
today <- getQuote(c("GOVT","VWOB"))
today <- today %>%
  mutate(date = Sys.Date(),
         close=Last)
today$symbol <- rownames(today)
today <- today %>%
  select(symbol,date,close)

#Combine historical data with today's data
df <- bind_rows(df,today)

#Reorganize data frame so GOVT and VWOB are side-by-side
df_1 <- df %>%
  filter(symbol=="GOVT") %>%
  select(date,govt_close = close)
df_2 <- df %>%
  filter(symbol=="VWOB") %>%
  select(date,vwob_close = close)
df <- inner_join(df_1,df_2,by="date")

#Calculate spread between them
df <- df %>% 
  mutate(spread = vwob_close/govt_close)

#Plot the spread
df %>% ggplot(aes(date,spread)) +
  geom_line() +
  ggtitle("VWOB (EM bonds) / GOVT (US bonds) price ratio") +
  labs(x="Date",y="Price ratio",caption="Data courtesy of Yahoo! Finance API. Copyright Wall Street Petting Zoo, 2022.\n
        Non-adjusted price data through 2/24/2022. Average duration of both ETFs is ~8.3 years. ")

#Save the plot
ggsave("vwob-govt.jpg",
       plot = last_plot(),
       scale = 1,
       width = 1920/300,
       height = 1080/300,
       units = "in")
