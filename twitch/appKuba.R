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
library(wordcloud)
library(RColorBrewer)


# thematic_shiny(font = "auto")


######### small data from small CSV ############

Twitch_global_data_1 <- read_csv("Twitch_global_data (1).csv")
#View(Twitch_global_data_1)


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

#View(twitch_overall)

#do 2
Twitch_game_data <- read_csv("Twitch_game_data.csv", 
                             col_types = cols(Month = col_integer()))

Twitch_game_data <- Twitch_game_data %>%
  mutate(Hours_Streamed = str_replace(Hours_Streamed, " hours",""))

Twitch_game_data <- Twitch_game_data %>% 
  mutate(Hours_Streamed = as.numeric(Hours_Streamed))


#do 3
Twitch_game_yearly_data <- Twitch_game_data %>%
  select(-Rank)

Twitch_game_yearly_data <- Twitch_game_yearly_data %>%
  group_by(Game, Year)  %>%
  summarise(
    hours_watched_sum = sum(Hours_watched), 
    hours_streamed_sum = sum(Hours_Streamed),
    peak_viewers_max = max(Peak_viewers),
    peak_channels_max = max(Peak_channels),
    streamers_sum = sum(Streamers),
    avg_viewers_sum = mean(Avg_viewers),
    avg_channels_sum = mean(Avg_channels),
    avg_viewer_ratio_sum = mean(Avg_viewer_ratio)
  )

#View(Twitch_game_yearly_data)

twitch_cloud <- Twitch_game_yearly_data


# Define UI for application that draws a histogram
ui <- fluidPage(
  #adding theme
  theme = bslib::bs_theme(
    bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198",
    base_font = bslib::font_google("Dongle"),
    font_scale = 2
  ),
  
  
  # Application title
  titlePanel("Twitch statistics"),
  
  tabsetPanel(
    tabPanel("Tab 1",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins",
                             "Choose year:",
                             min = 2016,
                             max = 2021,
                             value = 2018),
                 selectInput("stats", "Choose parameter to display:",
                             choices = c("Hours_watched", "Avg_viewers", "Peak_viewers", "Streams", "Avg_channels", "Games_streamed"),
                 ),
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot")
               )
             ),
             
             
    ),
    
    
    tabPanel("Tab 2",
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year2",
                             "Choose year:",
                             min = 2016,
                             max = 2021,
                             value = 2016),
                 sliderInput("month2",
                             "Choose month:",
                             min = 1,
                             max = 12,
                             value = 1),
                 selectInput("stats2", "Choose parameter to display:",
                             c("Hours_watched","Hours_Streamed", "Peak_viewers", "Peak_channels", "Streamers", "Avg_viewers", "Avg_channels", "Avg_viewer_ratio"),
                 ),
               ),
               mainPanel(
                 plotOutput("distPlot2"),
               ),
               
             )
             
             
             
             
    ),
    
    
    
    tabPanel("Tab 3",
             sidebarLayout(
               sidebarPanel(
                 selectInput("game3", "Choose a game:",
                             c("League of Legends","Counter-Strike: Global Offensive", "Dota 2", "Hearthstone", "Call of Duty: Black Ops III", "Grand Theft Auto V", "VALORANT", "Fortnite"),
                 ),
               ),
               mainPanel(
                 
                 fluidRow(
                   column(6,
                          plotOutput("distPlot3_1"),
                   ),
                   column(6,
                          plotOutput("distPlot3_2"),
                   )
                 ),
                 fluidRow(
                   column(6,
                          plotOutput("distPlot3_3"),
                   ),
                   column(6,
                          plotOutput("distPlot3_4"),
                   )
                 ),
                 fluidRow(
                   column(6,
                          plotOutput("distPlot3_5"),
                   ),
                   column(6,
                          plotOutput("distPlot3_6"),
                   )
                 ),
                 fluidRow(
                   column(6,
                          plotOutput("distPlot3_7"),
                   ),
                   column(6,
                          plotOutput("distPlot3_8"),
                   )
                 )
               ),
               
             )
             
    ),
    
    tabPanel("Tab 4",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year4",
                             "Choose year:",
                             min = 2016,
                             max = 2021,
                             value = 2016),

               ),
               
               
               mainPanel(
                 "Visualisation of most popular games (according to sum of hours watched per year)",
                 plotOutput("distPlot4")
               ),
               
             )
             
    ),
    
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  thematic::thematic_shiny()
  
  
  
  output$distPlot <- renderPlot({
    bins <- input$bins
    stats <- input$stats
    
    tw <- twitch_overall %>%
      subset(year == bins)
    
    y_name <- input$stats
    
    ggplot(tw, aes(x=reorder(Month_name, Month), y=get(stats))) + geom_col(fill="#debb2f") + 
      labs(title = "General statistics for whole twitch", x = "Month", y = y_name)
    
    
    
  })
  
  output$distPlot2 <- renderPlot({
    year2 <- input$year2
    month2 <- input$month2
    stats2 <- input$stats2
    
    Twitch_global_data_per_year <- Twitch_game_data %>% subset(Year == year2) %>% subset(Month == month2)
    #View(Twitch_global_data_per_year)
    Twitch_global_data_per_year <- Twitch_global_data_per_year %>% arrange(desc(stats2)) %>% head(5)
    #View(Twitch_global_data_per_year)
    
    
    ggplot(Twitch_global_data_per_year, aes(x=reorder(Game, -get(stats2)), y=get(stats2))) + geom_col(fill="#debb2f") +
      labs(title = "Top 5 games", x = "Game", y = stats2) 
    
    
    
  })
  
  
  output$distPlot3_1 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=hours_watched_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sum of hours watched in each year", x ="Year", y = "Hours watched")
  })
  
  output$distPlot3_2 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=hours_streamed_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sum of hours streamed in each year", x = "Year", y = "Hours streamed")
  })
  
  output$distPlot3_3 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=peak_viewers_max)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Peak number of viewers in each year", x = "Year", y = "Peak number of viewers")
  })
  
  
  output$distPlot3_4 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=peak_channels_max)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Peak number of channels in each year", x = "Year", y = "Peak number of channels")
  })
  
  output$distPlot3_5 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=streamers_sum)) +
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sum of number of streamers in each year", x = "Year", y = "Number of streamers")
  }) 
  
  output$distPlot3_6 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=avg_viewers_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Average number of viewers in each year", x = "Year", y = "Average number of viewers")
  }) 
  
  
  output$distPlot3_7 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=avg_channels_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Average number of channels in each year", x = "Year", y = "Average number of channels")
  })
  
  output$distPlot3_8 <- renderPlot({
    game3 <- input$game3

    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=avg_viewer_ratio_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Average viewer ratio in each year", x = "Year", y = "Average viewer ratio")
  })
  
  
  mycloud <- repeatable(wordcloud)
  
  output$distPlot4 <- renderPlot({
    year4 <- input$year4

    twitch_cloud <- twitch_cloud %>% subset(Year == year4) %>% arrange(-hours_watched_sum) %>% head(30)
    # View(twitch_cloud)
    mycloud(words = twitch_cloud$Game, freq = twitch_cloud$hours_watched_sum,random.order=FALSE, rot.per=0.35,   colors=brewer.pal(8, "Dark2"))
    
    
    
  })
 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

