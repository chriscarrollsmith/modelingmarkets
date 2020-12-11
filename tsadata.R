#TSA throughput

# for scraping
library(rvest)
# blanket import for core tidyverse packages
library(tidyverse)
#scales library for axis scaling
library(scales)

url_1 <- "https://www.tsa.gov/coronavirus/passenger-throughput"
url_2 <- "https://www.tsa.gov/coronavirus/passenger-throughput?page=1"

scrape_table <- function(url){
tsa_raw <- as.data.frame(url %>%
                           read_html() %>% #pull all HTML from the webpage
                           html_nodes(xpath = '//*[@id="block-mainpagecontent"]/div/div/div[2]/table') %>% 
                           html_table()) #get table using XPath copied from Chrome "inspect" of table HTML %>%
Throughput <- as.numeric(gsub(",","",tsa_raw$Total.Traveler.Throughput)) #remove commas from numbers in the table
Year_Ago_Throughput <- as.numeric(gsub(",","",tsa_raw$Total.Traveler.Throughput..1.Year.Ago...Same.Weekday.)) #remove commas from numbers in the table
Dates <- as.Date(tsa_raw$Date,"%m/%d/%Y") #format date column as a date variable
tsa_data <- data.frame(Date = Dates,Throughput,Year_Ago_Throughput) #return cleaned table as a data frame
}

#Get both pages of TSA throughput data and row bind into a single data frame
tsa_data <- rbind(scrape_table(url_1), scrape_table(url_2))

tsa_data %>%
  ggplot() +
  geom_line(aes(x=Date,y=Throughput)) +
  geom_smooth(aes(x=Date,y=Throughput),color="Black", span = 0.3) +
  geom_line(aes(x=Date,y=Year_Ago_Throughput),color="Blue") +
  geom_smooth(aes(x=Date,y=Year_Ago_Throughput),color="Blue",span = 0.3) +
  scale_y_continuous(labels = comma) +
  scale_x_date(date_breaks = "1 month",date_labels = "%b") +
  labs(title = "2020 TSA throughput (black) vs. 2019 TSA throughput (blue)",caption = paste("Data courtesy of https://www.tsa.gov/coronavirus/passenger-throughput. Updated ",as.character(Sys.Date()),".",sep="")) +
  theme_classic()

ggsave(
  filename = "tsadata.jpg",
  plot = last_plot(),
  path = "", #Insert path to which to save the image output file
  scale = 1,
  width = 1920/300,
  height = 1080/300,
  units = "in"
)
