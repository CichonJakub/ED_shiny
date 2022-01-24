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




# Twitch_global_data_1 <- read_csv("Twitch_game_data.csv")
# View(Twitch_global_data_1)

Twitch_game_data <- read_csv("Twitch_game_data.csv", 
                             col_types = cols(Month = col_integer()))

Twitch_game_data <- Twitch_game_data %>%
  mutate(Hours_Streamed = str_replace(Hours_Streamed, " hours",""))

Twitch_game_data <- Twitch_game_data %>% 
  mutate(Hours_Streamed = as.numeric(Hours_Streamed))


Twitch_game_yearly_hours_watched <- Twitch_game_data %>%
  select(Game, Month, Year, Hours_watched)

newcsv <- Twitch_game_yearly_hours_watched %>%
  group_by(Game, Year) %>%
  summarise(
    hours_sum = sum(Hours_watched)
  )
View(newcsv)
View(Twitch_game_data)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year:",
                        min = 2016,
                        max = 2021,
                        value = 2016)
        ),sliderInput("month",
                      "Month:",
                      min = 1,
                      max = 12,
                      value = 1)
    ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"), plotOutput("distPlot2")
        ),
    selectInput("stats", "Choose a stats to display:",
                c("Hours_watched","Hours_Streamed", "Peak_viewers", "Peak_channels", "Streamers", "Avg_viewers", "Avg_channels", "Avg_viewer_ratio"),
    ),
    selectInput("game", "Choose a game:",
                c("League of Legends","Counter-Strike: Global Offensive", "Dota 2", "Hearthstone", "Call of Duty: Black Ops III", "Grand Theft Auto V", "VALORANT", "Fortnite"),
    )
    )

    # sidebarLayout(
    #   sidebarPanel(sliderInput("year",
    #                            "Year:",
    #                            min = 2016,
    #                            max = 2021,
    #                            value = 2016)
    #   ), sidebarPanel(sliderInput("month",
    #                               "Month:",
    #                               min = 1,
    #                               max = 12,
    #                               value = 1)
    #     
    #   ), mainPanel(plotOutput("distPlot"))
    # )


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins)
        year <- input$year
        month <- input$month
        stats <- input$stats
        
        Twitch_global_data_per_year <- Twitch_game_data %>% subset(Year == year) %>% subset(Month == month) %>% subset(Rank < 6)
        # View(Twitch_global_data_per_year)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        # ggplot(subset(Twitch_global_data_1, year == bins), aes(x=Game, y=`Hours_watched`)) + geom_col(fill="#debb2f") + 
        #   labs(title = "Sumaryczna powierzchnia gruntów w poszczególnych województwach", x = "Województwo", y = "Powierzchnia [ha]")
        
        ggplot(Twitch_global_data_per_year, aes(x=reorder(Game, Rank), y=get(stats))) + geom_col(fill="#debb2f") +
          labs(title = "Sumaryczna powierzchnia gruntów w poszczególnych województwach", x = "Game", y = stats)
        
        # ggplot(Twitch_global_data_per_year, aes_string(x="Game", y=stats)) + geom_col(fill="#debb2f") + 
        #   labs(title = "Sumaryczna powierzchnia gruntów w poszczególnych województwach", x = "Województwo", y = "Powierzchnia [ha]")
        
        
    })
    
    output$distPlot2 <- renderPlot({
      # generate bins based on input$bins from ui.R
      # x    <- faithful[, 2]
      # bins <- seq(min(x), max(x), length.out = input$bins)
      game <- input$game
      
      Twitch_global_data_per_year <- Twitch_game_data %>% subset(Game == game)
      # View(Twitch_global_data_per_year)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      
      # ggplot(subset(Twitch_global_data_1, year == bins), aes(x=Game, y=`Hours_watched`)) + geom_col(fill="#debb2f") + 
      #   labs(title = "Sumaryczna powierzchnia gruntów w poszczególnych województwach", x = "Województwo", y = "Powierzchnia [ha]")
      
      ggplot(Twitch_global_data_per_year, aes(x=reorder(Year, Year), y=Hours_watched)) + geom_col(fill="#debb2f") +
        labs(title = "Sumaryczna powierzchnia gruntów w poszczególnych województwach", x = "Game", y = "Hours_watched")
      
      # ggplot(Twitch_global_data_per_year, aes_string(x="Game", y=stats)) + geom_col(fill="#debb2f") + 
      #   labs(title = "Sumaryczna powierzchnia gruntów w poszczególnych województwach", x = "Województwo", y = "Powierzchnia [ha]")
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
