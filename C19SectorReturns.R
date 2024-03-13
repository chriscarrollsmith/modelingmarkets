#Scrape S&P 500 tickers and sectors from Wikipedia
#Download Yahoo Finance price data since beginning of Covid-19 pandemic
#Chart average (equal weight) returns by sector

#tidy charts and tables
library(tidyverse)
#tidy financial analysis
library(tidyquant)
library(plotly)

#Get historical price data for all S&P 500 tickers starting at beginning of pandemic
df <- tq_get(tq_index("SP500"), from = as.Date("2020-02-19"), to = Sys.Date())

#Combine info from Wikipedia with our price data
#by joining the two tables using symbol as reference point
#select desired columns and convert symbol to factor
#construct vector of equal weight average percent change by sector by date
sector_df <- df %>%
  select(date,symbol,sector,adjusted,weight) %>%
  mutate(sector = as.factor(sector)) %>%
  group_by(symbol) %>%
  mutate(pctchange = adjusted/first(adjusted)) %>%
  group_by(sector,date) %>%
  mutate(weight = weight/sum(weight)) %>%
  summarize(`Equal weight` = mean(pctchange),
            `Market weight` = sum(pctchange*weight)) %>%
  gather(key = weight,value=return,`Equal weight`,`Market weight`)

#Pull vectors of sectors in order of final returns for use in reordering factor
refactorer <- list()
refactorer[[1]] <- sector_df %>%
  filter(weight == 'Equal weight') %>%
  summarize(return = last(return)-1) %>%
  bind_cols(data.frame(color_code = RColorBrewer::brewer.pal(n=11,name="Paired"))) %>%
  arrange(desc(return)) %>%
  select(sector,color_code)
refactorer[[2]] <- sector_df %>%
  filter(weight == 'Market weight') %>%
  summarize(return = last(return)-1) %>%
  bind_cols(data.frame(color_code = RColorBrewer::brewer.pal(n=11,name="Paired"))) %>%
  arrange(desc(return)) %>%
  select(sector,color_code)

#Save data for pulling into a Shiny app
saveRDS(refactorer,paste0(getwd(),"/c19-return-comparer/data/refactorer.rds"))
saveRDS(sector_df,paste0(getwd(),"/c19-return-comparer/data/sector_df.rds"))

#Read data from file
# refactorer <- readRDS(paste0(getwd(),"/c19-return-comparer/data/refactorer.rds"))
# sector_df <- readRDS(paste0(getwd(),"/c19-return-comparer/data/sector_df.rds"))

#plot sector equal weight return since period start
sector_df %>%
  mutate(return = return - 1) %>%
  left_join(refactorer[[1]]) %>%
  mutate(sector = factor(sector,levels=refactorer[[1]][[1]])) %>%
  ggplot(aes(x = date,y = return,color=sector)) +
  geom_line(linewidth=1) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b%y") +
  scale_color_manual(values = refactorer[[1]][[2]]) +
  labs(title="S&P 500 sector return during the Covid-19 pandemic",x="Date",y="Percent change",color="Sector") +
  theme_bw() +
  facet_wrap(vars(weight),nrow=2)

#Export chart as image file
ggsave(
  filename = "C19SectorReturns.jpg",
  plot = last_plot(),
  scale = 1,
  width = 1920/200,
  height = 1080/200,
  units = "in"
)

#plot sector market weight return since period start
sector_df %>%
  filter(weight == "Equal weight") %>%
  filter(sector %in% c("Energy","Materials","Real Estate","Utilities","Communication Services")) %>%
  mutate(sector = factor(sector,levels=refactorer[[1]][[1]][refactorer[[1]][[1]] %in% c("Energy","Materials","Real Estate","Utilities","Communication Services")])) %>%
  mutate(return = return - 1) %>%
  plot_ly(x=~date,y=~return,type="scatter",mode="lines",color=~sector,
          hoverinfo="x+y+text",text=~sector,
          colors=refactorer[[1]][[2]][refactorer[[1]][[1]] %in% c("Energy","Materials","Real Estate","Utilities","Communication Services")]) %>%
  layout(title="S&P 500 sector return (equal weight) during the Covid-19 pandemic",
         xaxis=list(title="Date",tickformat="%b%y",tickmode="auto",tickfont=list(size=8),dtick="M3"),
         yaxis=list(title="Percent change",tickformat=".2%"))

#Export chart as image file
ggsave(
  filename = "C19SectorReturns-focus.jpg",
  plot = last_plot(),
  scale = 1,
  width = 1920/200,
  height = 1080/200,
  units = "in"
)
