#Scrape S&P 500 tickers and sectors from Wikipedia
#Download Yahoo Finance price data since beginning of Covid-19 pandemic
#Chart average (equal weight) returns by sector

#tidy charts and tables
library(tidyverse)
library(shiny)

#Save data for pulling into a Shiny app
refactorer <- readRDS(paste0(getwd(),"/data/refactorer.rds"))
sector_df <- readRDS(paste0(getwd(),"/data/sector_df.rds"))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  tags$head(
    tags$style(
      HTML(
        ".checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    )
  ),
  # App title ----
  titlePanel("S&P 500 sector returns since the start of the Covid-19 pandemic"),
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    mainPanel(
      # Input: Group of checkboxes to select sectors to show ----
      selectInput(
        inputId="dropdown",
        label = "Component weighting",
        choices = c("Equal weight","Market weight"),
        selected = "Equal weight",
        multiple = FALSE,
        selectize = TRUE,
        width = '100%'
      ),
      # Input: Group of checkboxes to select sectors to show ----
      checkboxGroupInput(
        inputId = "checkboxes",
        label = "S&P 500 sectors",
        choices = unique(sector_df$sector),
        selected = unique(sector_df$sector),
        inline = TRUE,
        width = '100%'
      ),
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "sectorsPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Reactive line chart of sector returns since beginning of Covid-19 pandemic
  output$sectorsPlot <- renderPlot({
    sector_df %>%
      filter(weight == input$dropdown) %>%
      filter(sector %in% input$checkboxes) %>%
      mutate(return = return - 1) %>%
      mutate(sector = factor(sector,levels=case_when(input$dropdown == "Equal weight"~refactorer[[1]],
                                                     T~refactorer[[2]]))) %>%
      ggplot(aes(x = date,y = return,color=sector)) +
      geom_line(linewidth=1) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_date(date_breaks = "3 months", date_labels = "%b%y") +
      labs(title=paste0("S&P 500 sector return (",tolower(input$dropdown),") during the Covid-19 pandemic"),x="Date",y="Percent change",color="Sector") +
      theme_bw()
  })
}

shinyApp(ui = ui, server = server)
