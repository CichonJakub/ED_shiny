#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)
library(ggpubr)

######### small data from small CSV ############

Twitch_global_data_1 <- read_csv("Twitch_global_data (1).csv")
View(Twitch_global_data_1)


twitch_overall <- Twitch_global_data_1 %>%
  mutate(Month_name = case_when(Month == 1 ~ "January",
                                Month == 2 ~ "February",
                                Month == 3 ~ "March",
                                Month == 4 ~ "April",
                                Month == 5 ~ "May",
                                Month == 6 ~ "June",
                                Month == 7 ~ "July",
                                Month == 8 ~ "August",
                                Month == 9 ~ "September",
                                Month == 10 ~ "October",
                                Month == 11 ~ "November",
                                Month == 12 ~ "December"
                                ))

View(twitch_overall)
# dropdown na to jakie dane chce wysietlic, 
# miesaice z numerków zamien na wpisane



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Twitch Stats ;)"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 2016,
                  max = 2021,
                  value = 2018)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  
  selectInput("stats", "Choose a stats to display:",
              choices = c("Hours_watched", "Avg_viewers", "Peak_viewers", "Streams", "Avg_channels", "Games_streamed"),
  ),
  textOutput("result")
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$result <- renderText({
    paste("You chose this stat", input$stats)
  })
  
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins)
    bins <- input$bins
    stats <- input$stats
    # Twitch_global_data_per_year <- Twitch_global_data_1 %>% subset(year == bins)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    tw <- twitch_overall %>%
      subset(year == bins)
    
    y_name <- input$stats
    
    ggplot(tw, aes(x=reorder(Month_name, Month), y=get(stats))) + geom_col(fill="#debb2f") + 
      labs(title = "( . Y . ) +  8======B ---- <-", x = "Month", y = y_name)
    

    
  })
  
  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins)
    bins <- input$bins
    stats <- input$stats
    # Twitch_global_data_per_year <- Twitch_global_data_1 %>% subset(year == bins)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    tw <- twitch_overall %>%
      subset(year == bins)
    
    y_name <- input$stats
    
    ggplot(tw, aes(x=reorder(Month_name, Month), y=get(stats))) + geom_col(fill="#debb2f") + 
      labs(title = "Wykresiki z mniejszego excela", x = "Month", y = y_name)
    
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

