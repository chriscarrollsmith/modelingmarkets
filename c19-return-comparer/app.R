#Scrape S&P 500 tickers and sectors from Wikipedia
#Download Yahoo Finance price data since beginning of Covid-19 pandemic
#Chart average (equal weight) returns by sector

#tidy charts and tables
library(tidyverse)
library(shiny)
library(plotly)

#Read data from file for displaying in Shiny app
refactorer <- readRDS(paste0(getwd(),"/data/refactorer.rds"))
sector_df <- readRDS(paste0(getwd(),"/data/sector_df.rds"))
refactorer[[1]]$sector <- stringr::str_replace_all(refactorer[[1]]$sector," ","\n")
refactorer[[2]]$sector <- stringr::str_replace_all(refactorer[[2]]$sector," ","\n")

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
      plotlyOutput(outputId = "sectorsPlot")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Reactive line chart of sector returns since beginning of Covid-19 pandemic
  output$sectorsPlot <- renderPlotly({
    inputs <- stringr::str_replace_all(input$checkboxes," ","\n")
    print(inputs)
    sector_df %>%
      filter(weight == input$dropdown) %>%
      mutate(sector = stringr::str_replace_all(sector," ","\n")) %>%
      filter(sector %in% inputs) %>%
      mutate(sector = factor(sector,levels=case_when(input$dropdown == "Equal weight"~refactorer[[1]][[1]][refactorer[[1]][[1]] %in% inputs],
                                                     T~refactorer[[2]][[1]][refactorer[[2]][[1]] %in% inputs]))) %>%
      mutate(return = return - 1) %>%
      plot_ly(x=~date,y=~return,type="scatter",mode="lines",color=~sector,
              hoverinfo="x+y+text",text=~sector,
              colors=case_when(input$dropdown == "Equal weight"~refactorer[[1]][[2]][refactorer[[1]][[1]] %in% inputs],
                               T~refactorer[[2]][[2]][refactorer[[2]][[1]] %in% inputs])) %>%
      layout(title=list(text=paste0("S&P 500 sector return (",tolower(input$dropdown),") during the Covid-19 pandemic"),x=0),
             xaxis=list(title="Date",tickformat="%b%y",tickmode="auto",tickfont=list(size=8),dtick="M3"),
             yaxis=list(title="Percent change",tickformat=".2%")
             #,legend=list(x=0,y=1,font = list(size = 10) #For legend inside chart
             #,legend=list(y=-0.5,orientation="h") #For legend under chart
             )
  })
}

shinyApp(ui = ui, server = server)
