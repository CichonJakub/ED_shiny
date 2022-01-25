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
# dropdown na to jakie dane chce wysietlic, 
# miesaice z numerk?w zamien na wpisane

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

View(Twitch_game_yearly_data)

twitch_cloud <- Twitch_game_yearly_data


# Define UI for application that draws a histogram
ui <- fluidPage(
  #adding theme
  #theme = bslib::bs_theme(bootswatch = "darkly"),
  theme = bslib::bs_theme(
    bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198",
    # bslib also makes it easy to import CSS fonts
    base_font = bslib::font_google("Dongle"),
    font_scale = 2
  ),
  
  
  # Application title
  titlePanel("Twitch Stats ;)"),
  
  tabsetPanel(
    tabPanel("Zbiorecze",
             
             # Sidebar with a slider input for number of bins 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("bins",
                             "Number of bins:",
                             min = 2016,
                             max = 2021,
                             value = 2018),
                 selectInput("stats", "Choose a stats to display:",
                             choices = c("Hours_watched", "Avg_viewers", "Peak_viewers", "Streams", "Avg_channels", "Games_streamed"),
                 ),
                 
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("distPlot")
               )
             ),
             
             
    ),
    
    
    tabPanel("Szczegolowe",
             
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year2",
                             "Year:",
                             min = 2016,
                             max = 2021,
                             value = 2016),
                 sliderInput("month2",
                             "Month:",
                             min = 1,
                             max = 12,
                             value = 1),
                 selectInput("stats2", "Choose a stats to display:",
                             c("Hours_watched","Hours_Streamed", "Peak_viewers", "Peak_channels", "Streamers", "Avg_viewers", "Avg_channels", "Avg_viewer_ratio"),
                 ),
                 #to idezie do 3 dzialki
                 # selectInput("game", "Choose a game:",
                 #             c("League of Legends","Counter-Strike: Global Offensive", "Dota 2", "Hearthstone", "Call of Duty: Black Ops III", "Grand Theft Auto V", "VALORANT", "Fortnite"),
                 # ),
               ),
               mainPanel(
                 plotOutput("distPlot2"),
               ),
               
             )
             
             
             
             
    ),
    
    
    
    tabPanel("Bumbelki",
             sidebarLayout(
               sidebarPanel(
                 selectInput("game3", "Choose a game:",
                             c("League of Legends","Counter-Strike: Global Offensive", "Dota 2", "Hearthstone", "Call of Duty: Black Ops III", "Grand Theft Auto V", "VALORANT", "Fortnite"),
                 ),
                 #to idezie do 3 dzialki
                 # selectInput("game", "Choose a game:",
                 #             c("League of Legends","Counter-Strike: Global Offensive", "Dota 2", "Hearthstone", "Call of Duty: Black Ops III", "Grand Theft Auto V", "VALORANT", "Fortnite"),
                 # ),
               ),
               mainPanel(
                 
                 #plotOutput("distPlot3"),
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
    
    tabPanel("Population",
             sidebarLayout(
               sidebarPanel(
                 sliderInput("year4",
                             "Year:",
                             min = 2016,
                             max = 2021,
                             value = 2016),

               ),
               
               
               mainPanel(
                 plotOutput("distPlot4"),
               ),
               
             )
             
    ),
    
    
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  thematic::thematic_shiny()
  
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
      labs(title = "co? ", x = "Month", y = y_name)
    
    
    
  })
  
  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2]
    # bins <- seq(min(x), max(x), length.out = input$bins)
    year2 <- input$year2
    month2 <- input$month2
    stats2 <- input$stats2
    
    #Twitch_global_data_per_year <- Twitch_game_data %>% subset(Year == year2) %>% subset(Month == month2) %>% subset(Rank < 6)
    
    Twitch_global_data_per_year <- Twitch_game_data %>% subset(Year == year2) %>% subset(Month == month2)
    #View(Twitch_global_data_per_year)
    Twitch_global_data_per_year <- Twitch_global_data_per_year %>% arrange(desc(stats2)) %>% head(5)
    #View(Twitch_global_data_per_year)
    
    
    # bg = "#002B36", fg = "#EEE8D5", primary = "#2AA198" /////////// CBBF7A //////////#F4E87C
    # ggplot(Twitch_global_data_per_year, aes(x=reorder(Game, -get(stats2), y=get(stats2))) + geom_col(fill="#D1AD0A") +
    #   labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Game", y = stats2) +
    #   theme(text = element_text(size = 14)))
    
    ggplot(Twitch_global_data_per_year, aes(x=reorder(Game, -get(stats2)), y=get(stats2))) + geom_col(fill="#debb2f") +
      labs(title = "YYYYYYYYY", x = "Game", y = stats2) +
      theme(text = element_text(size = 14))
    
    
    # ggplot(Twitch_global_data_per_year, aes_string(x="Game", y=stats)) + geom_col(fill="#debb2f") + 
    #   labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Wojew?dztwo", y = "Powierzchnia [ha]")
    
    
  })
  
  
  output$distPlot3_1 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=hours_watched_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x ="Year", y = "1")
  })
  
  output$distPlot3_2 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=hours_streamed_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Year", y = "2d")
  })
  
  output$distPlot3_3 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=peak_viewers_max)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Year", y = "3")
  })
  
  
  output$distPlot3_4 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>% 
      mutate(Year = as.numeric(Year))
    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=peak_channels_max)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Year", y = "4")
  })
  
  output$distPlot3_5 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=streamers_sum)) +
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Year", y = "5")
  }) 
  
  output$distPlot3_6 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=avg_viewers_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Year", y = "6")
  }) 
  
  
  output$distPlot3_7 <- renderPlot({
    game3 <- input$game3
    
    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=avg_channels_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Year", y = "7")
  })
  
  output$distPlot3_8 <- renderPlot({
    game3 <- input$game3

    Twitch_game_yearly_data <- Twitch_game_yearly_data %>% subset(Game == game3)%>%
      mutate(Year = as.numeric(Year))

    
    
    ggplot(Twitch_game_yearly_data, aes(x=reorder(Year, Year), y=avg_viewer_ratio_sum)) + 
      geom_line(aes(group=1), color="#debb2f", size=1.5) +
      geom_point(shape=21, fill="#debb2f", size=5) +
      labs(title = "Sumaryczna powierzchnia grunt?w w poszczeg?lnych wojew?dztwach", x = "Year", y = "8")
  })
  
  
  mycloud <- repeatable(wordcloud)
  
  output$distPlot4 <- renderPlot({
    year4 <- input$year4

    twitch_cloud <- twitch_cloud %>% subset(Year == year4) %>% arrange(-hours_watched_sum) %>% head(30)
    View(twitch_cloud)
    mycloud(words = twitch_cloud$Game, freq = twitch_cloud$hours_watched_sum, colors=brewer.pal(8, "Dark2"))
    #mycloud(words = Twitch_game_yearly_4$Game, freq = get(stats4), colors=brewer.pal(8, "Dark2"))
    
    #wordcloud(words = Twitch_game_yearly_4$Game, freq = Twitch_game_yearly_4$hours_watched_sum, colors=brewer.pal(8, "Dark2"))
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

