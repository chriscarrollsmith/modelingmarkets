#Load libraries for API access
library(httr)
library(jsonlite)
#Load libraries for visualization
library(tidyverse)
library(scales)

#Access Healthdata.gov API to retrieve today's hospital utilization data
apiurl <- "https://healthdata.gov/api/action/datastore/search.json?resource_id=38e07d07-40a2-4749-bfd3-68fefa42a61f&limit=50"
req <- GET(apiurl)

#Convert relevant portion of JSON file to a data frame
convertreq = fromJSON(rawToChar(req$content))
data <- convertreq$result$records
data <- data %>% select(state,inpatient_beds_utilization,adult_icu_bed_utilization)

#Order states by ICU bed utilization and format percentages for visualization
data <- data %>% 
  mutate(adult_icu_bed_utilization = as.numeric(adult_icu_bed_utilization)) %>%
  mutate(state = fct_reorder(state, adult_icu_bed_utilization)) %>%
  mutate(ICUpercent = paste(as.character(round(adult_icu_bed_utilization*100)),"%",sep=""))

#Create column chart of ICU bed utilization colored with a yellow-to-red gradient
data %>%
  ggplot(aes(x=state,y=adult_icu_bed_utilization)) +
  geom_bar(position = 'dodge',stat="identity",aes(fill=adult_icu_bed_utilization),show.legend = FALSE) +
  scale_y_continuous(labels = scales::percent,limits = c(0,1),expand = c(0,0)) +
  scale_fill_gradient(low = "yellow", high = "red") +
  geom_text(aes(label=ICUpercent), position=position_dodge(width=0.9), vjust=-0.25, size=3) +
  labs(title = "How close is your US state to ICU bed capacity?", 
       caption = paste("Data from U.S. Department of Health & Human Services website healthdata.gov. Updated ",as.character(Sys.Date()),".",sep="")) +
  xlab("US state") +
  ylab("ICU bed utilization") +
  theme_bw()

#Export chart as image file
ggsave(
  filename = "ICUutilization.jpg",
  plot = last_plot(),
  path = "C:/Users/chris/OneDrive/Pictures/Infographics",
  scale = 1,
  width = 1920/144,
  height = 1080/144,
  units = "in"
)
