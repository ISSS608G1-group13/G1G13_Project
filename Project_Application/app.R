library(shiny)
library(shinythemes)
library(sf)
library(tmap)
library(tidyverse)
library(rsconnect)
library(ggplot2)
library(igraph)
library(plotly)
library(tidyr)
library(RColorBrewer)

packages = c("tidyverse","tidyr","tidyverse","dplyr","sp","raster","sf",
             "vctrs","clock","tmap","rgdal","readr","ggplot2","plotly","tmap","gganimate","av","gifski",
             "igraph","tidygraph","ggraph","visNetwork","lubridate","DT","collapsibleTree",
             'tidytext','widyr', 'wordcloud','DT', 'ggwordcloud','textplot', 'lubridate',
             'hms','tidyverse','tidygraph', 'ggraph','igraph')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# Import dataset 
location_num <- read_csv("./data/location num.csv")
bgmap <- raster("./data/MC2-tourist.tif")
Abila_st <- st_read(dsn = "./data/Geospatial",
                    layer = "Abila")
gps <- read_csv("./data/gps.csv")
location_frequency <- read.csv("./data/location frequency.csv/")
carassign <- read_csv("./data/car-assignments.csv")
creditcard <- read.csv("./data/cc_data.csv")
loyaltycard <- read.csv("./data/loyalty_data.csv")

car_assignments <- read.csv("./data/car-assignments.csv")

car_assignments$Name <- paste(car_assignments$FirstName, car_assignments$LastName)
CarTrack <- car_assignments %>% 
  select(CarID,CurrentEmploymentTitle,CurrentEmploymentType,Name)

news <- "./data/News Articles/"

# Dataset Process


gps$Timestamp <- date_time_parse(gps$Timestamp,
                                 zone="",
                                 format="%m/%d/%Y %H:%M")
creditcard$timestamp <- date_time_parse(creditcard$timestamp,
                                        zone="",
                                        format="%m/%d/%Y %H:%M")

loyaltycard$timestamp <- date_time_parse(loyaltycard$timestamp,
                                         zone="",
                                         format="%m/%d/%Y")

# Build new columns for day and hour and change the data type
credit_hour <- creditcard
credit_hour$hour <- as.numeric(get_hour(credit_hour$timestamp))
credit_hour$day <- as.numeric(get_day(credit_hour$timestamp))

# Wrangle the dataset for the frequency of purchase for every day
credit_hour_num <- credit_hour %>% 
  group_by(location,day,hour) %>% 
  summarise(frequency=n())

# Word Cloud data processing
read_folder <- function(infolder){
  tibble(file = dir(infolder,
                    full.names = TRUE))%>%
    mutate(text = map(file,
                      read_lines))%>%
    transmute(id = basename(file),
              text)%>%
    unnest(text)
}

raw_text <- tibble(folder=
                     dir(news,
                         full.names=TRUE)) %>%
  
  mutate(folder_out = map(folder,read_folder))%>%
  unnest(cols = c(folder_out))%>%
  transmute(newsgroup = basename(folder),
            id,text)

raw_text %>%
  group_by(newsgroup)%>%
  summarize(messages = n_distinct(id))

cleaned_text <- raw_text %>%
  group_by(newsgroup, id)%>%
  filter(cumsum(text == "") > 0,
         cumsum(str_detect(
           text, "^--"))==0) %>%
  ungroup()

cleaned_text <- cleaned_text %>%
  filter(str_detect(text,"^[^>]+[A-Za-z\\d]")
         | text == "",
         !str_detect(text,
                     "writes(:|\\.\\.\\.)$"),
         !str_detect(text,
                     "^In article <")
  )
usenet_words <- cleaned_text %>%
  unnest_tokens(word, text) %>%
  filter(str_detect(word, "[a-z']$"),
         !word %in% stop_words$word)

usenet_words %>%
  count(word, sort = TRUE)

words_by_newsgroup <- usenet_words %>%
  count(newsgroup, word, sort = TRUE) %>%
  ungroup()



#Shiny Part
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(id = "intabset",
             title = "GAStech Employees Behavior Analysis",
             tabPanel(title = "Home", icon = icon("home"),
                      value = "tabpanel1",
                      mainPanel(width = 15, style="margin-left:0%; margin-right:4%",
                                fluidRow(column(7,(h3("Welcome to GAStech Employees Behavior Analysis Tool", style="margin-top:0px;"))),
                                         (column(4,actionButton("btn_landing",label="Help: User Guide",icon=icon('question-circle'),class="down")))),
                      )
             ),
             navbarMenu("Background Analysis",
                        tabPanel("Popular Locations",
                                 value = "tabpanel2",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("locationfre",
                                                  label = "Popular locations based on:",
                                                  choices = c("Credit Card"="num_credit",
                                                              "Loyalty Card"="num_loyalty"),
                                                  selected = "num_credit"
                                     ),
                                     submitButton("Update"),
                                     helpText("Based on consumption frequency of GAStech employees.")
                                   ),
                                   mainPanel(
                                     plotlyOutput("LocationPlot")
                                   )
                                 )),
                        tabPanel("Hotpoints of News Articles", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("news", 
                                                 label = "Choose news name:",
                                                 choices = unique(raw_text$newsgroup),
                                                 selected = "All News Today"),
                                     hr(),
                                     sliderInput("freq",
                                                 "Minimum Frequency:",
                                                 min = 1,  max = 212, value = 5),
                                     sliderInput("max",
                                                 "Maximum Number of Words:",
                                                 min = 1,  max = 300,  value = 100
                                     ),
                                     submitButton("update", "Update")
                                   ),
                                   mainPanel(
                                     plotOutput("hotpoints"),
                                     plotlyOutput("barhotpoints")
                                   )
                                 )
                        )),
             navbarMenu("Consumption Analysis",
                        tabPanel("Hotpoints of News Articles", 
                                 value = "tabpanel4",
                                 sidebarLayout(
                                   sidebarPanel(
                                     "inout"
                                   ),
                                   mainPanel(
                                     "output"
                                   )
                                 )
                        ),
                        tabPanel("Comsumption Time and Value of Location",
                                 value = "tabpanel5",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("location",
                                      label = "Location:",
                                      choices = unique(loyaltycard$location),
                                      selected = "Abila Airport"
                          ),
                          submitButton("Update"),
                          width = 3
                        ),
                        mainPanel(
                          plotlyOutput("consumption_hour"),
                          plotlyOutput("consumption_value")
                        )
                      ))),
             tabPanel("Path Visualization", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("carid",
                                      label = "Car ID of employee:",
                                      choices = c(1:35),
                                      selected = 1
                          ),
                          sliderInput("day",
                                      label = "Days of January, 2014:",
                                      min = 6,
                                      max = 19,
                                      value = c(6,7),
                                      step = 1),
                          submitButton("Update")
                        ),
                        
                        mainPanel(
                          plotlyOutput("gpspath")
                        )
                      )),
             tabPanel("Network",
                      selectInput("state", "Choose a state:",
                                  list(`East Coast` = list("NY", "NJ", "CT"),
                                       `West Coast` = list("WA", "OR", "CA"),
                                       `Midwest` = list("MN", "WI", "IA"))
                      ),
                      submitButton("Update"),
                      textOutput("result"))
  ))


server <- function(input, output, session) {
  
  output$LocationPlot <- renderPlotly({
    
    data <- location_frequency %>%
      filter(cardtype==input$locationfre) 
    
    
    ggplot(data = data, 
           aes(x = frequency, y= reorder(location, frequency))) + 
      geom_bar(stat = "identity", 
               color = "white", fill = "light blue")+
      ylab("Location") + 
      xlab("Frequency")+
      ggtitle("The Frequency of Each Location")
  })
  
  output$consumption_hour <- renderPlotly({
    #         Build new dataset for selected location
    data <- credit_hour_num %>% 
      filter(location==input$location) 
    
    
    #Draw the heatmap
    ggplot(data=data,
           aes(x=day,y=hour,fill=frequency))+
      geom_tile()+
      scale_y_continuous(limits = c(0,24),
                         breaks = c(0,2,4,6,8,10,12,14,16,18,20,22,24))+
      scale_x_continuous(limits = c(6,19),
                         breaks = c(6,7,8,9,10,11,12,13,14,15,16,17,18,19))+
      scale_fill_gradient2(low="blue",
                           high="red",
                           na.value = 'white')+
      labs(x="Day",
           y="Hour",
           title="Frequency of Credit Card Purchase")+
      theme(plot.title=element_text(hjust=0.5))
  })
  
  output$consumption_value <- renderPlotly({
    #         Build new dataset for selected location
    data <- credit_hour %>% 
      filter(location==input$location) %>% 
      group_by(day) %>% 
      summarise(value=sum(price))
    
    plot_ly(data = data,
            x = ~day, 
            y = ~value, 
            type = 'scatter', 
            mode = 'lines',
            text = ~paste("Day:", day,
                          "<br>Totol purchase value:", value),
            name="Trend") %>% 
      layout(yaxis = list(title = 'Purchase Value'),
             xaxis = list(title = 'Day'),
             title = "Daily Purchase Value of Credit Card") %>% 
      add_trace(mode = 'markers',
                name="Detail")
  })
  
  output$hotpoints <- renderPlot({
    data <- words_by_newsgroup %>% 
      filter(newsgroup==input$news)
    
    
    wordcloud(data$word,
              data$n,
              min.freq = input$freq,
              max.words = input$max,
              random.order = FALSE,
              rot.per = 0.35,
              colors = brewer.pal(8,"Dark2"))
    
  })
  
  output$barhotpoints <- renderPlotly({
    
    data <- words_by_newsgroup %>% 
      filter(newsgroup==input$news)
    
    top20 <- head(data, 20)
    top20$word <- reorder(top20$word, top20$n)
    
    ggplot(top20, aes(x = word, y = n, fill = word, label = n)) +
      geom_bar(stat="identity", show.legend = FALSE,
               color = "white", fill = "light blue") +
      coord_flip() +
      labs(title = "Top 20 Most Used Words in News", x = "Word", y = "Word Count") +
      geom_label(aes(fill = word),colour = "white", fontface = "bold", show.legend = FALSE)
  })
  
  observeEvent(input$btn_landing, {
    updateTabsetPanel(session,
                      "intabset" ,
                      selected = "tabpanel2")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
