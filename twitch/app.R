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

Twitch_global_data_1 <- read_csv("Twitch_global_data (1).csv")
View(Twitch_global_data_1)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

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
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins)
        bins <- input$bins
        
        # Twitch_global_data_per_year <- Twitch_global_data_1 %>% subset(year == bins)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        ggplot(subset(Twitch_global_data_1, year == bins), aes(x=Month, y=`Streams`)) + geom_col(fill="#debb2f") + 
          labs(title = "Sumaryczna powierzchnia gruntów w poszczególnych województwach", x = "Województwo", y = "Powierzchnia [ha]")
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
