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
bgmap <- raster("./data/MC2-tourist.tif/")
Abila_st <- st_read(dsn = "./data/Geospatial",
                    layer = "Abila")

# Dataset Process
tm_shape(bgmap) +
  tm_rgb(bgmap, r = 1,g = 2,b = 3,
         alpha = NA,
         saturation = 1,
         interpolate = TRUE,
         max.value = 255)

gps$Timestamp <- date_time_parse(gps$Timestamp,
                                 zone = "",
                                 format = "%m/%d/%Y %H:%M")
gps$weekday = wday(gps$Timestamp, 
                   label = TRUE, 
                   abbr = FALSE)
gps$id <- as_factor(gps$id)
gps_sf <- st_as_sf(gps, 
                   coords = c("long", "lat"),
                   crs= 4326)
gps_path <- gps_sf %>%
  group_by(id, weekday) %>%
  summarize(m = mean(Timestamp), 
            do_union=FALSE) %>%
  st_cast("LINESTRING")

creditcard$timestamp <- date_time_parse(creditcard$timestamp,
                                        zone="",
                                        format="%m/%d/%Y %H:%M")

loyaltycard$timestamp <- date_time_parse(loyaltycard$timestamp,
                                         zone="",
                                         format="%m/%d/%Y")

# Network part data process
# Transforming data
creditcard_final_net <- creditcard

creditcard_final_net$last4ccnum <- as.character(creditcard_final_net$last4ccnum)
creditcard_final_net$Day  <-  get_day(creditcard_final_net$timestamp)
creditcard_final_net$Hour <-  get_hour(creditcard_final_net$timestamp)

# Creating nodes list
sources <- creditcard_final_net %>%
  distinct(last4ccnum) %>%
  rename(label = last4ccnum)
destinations <- creditcard_final_net %>%
  distinct(location) %>%
  rename(label = location)
cc_nodes <- full_join(sources, 
                      destinations, 
                      by = "label")
cc_nodes <- cc_nodes %>% 
  rowid_to_column("id")

# Creating and tidying edges list
edges <- creditcard_final_net %>%  
  group_by(last4ccnum, location, Day, Hour) %>%
  summarise(weight = n()) %>% 
  ungroup()

cc_edges <- edges %>% 
  left_join(cc_nodes, 
            by = c("last4ccnum" = "label")) %>% 
  rename(from = id)

cc_edges <- cc_edges %>% 
  left_join(cc_nodes, 
            by = c("location" = "label")) %>% 
  rename(to = id)

cc_edges <- select(cc_edges, from, to, 
                   Day, Hour, weight)

cc_graph <- tbl_graph(nodes = cc_nodes, 
                      edges = cc_edges, 
                      directed = FALSE)

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
  theme = shinytheme("spacelab"),
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
                                                 min = 1,  max = 212, value = 2),
                                     sliderInput("max",
                                                 "Maximum Number of Words:",
                                                 min = 1,  max = 300,  value = 100
                                     ),
                                     submitButton("Update")
                                   ),
                                   mainPanel(
                                     plotlyOutput("barhotpoints"),
                                     plotOutput("hotpoints")
                                   )
                                 )
                        )),
             navbarMenu("Consumption Analysis",
                        tabPanel("Consumption Comparison", 
                                 value = "tabpanel4",
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     radioButtons("choosetype",
                                                  label = "Choose day or time:",
                                                  choices = c("Day"="day",
                                                              "Time(hour)"="hour")),
                                     submitButton("Update")
                                   ),
                                   mainPanel(
                                     plotlyOutput("comparison")
                                   )
                                 )
                        ),
                        tabPanel("Comsumption of Each Location",
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
                      )),
                      tabPanel("Consumption Locations of Credit Card", 
                               value = "tabpanel4",
                               sidebarLayout(
                                 sidebarPanel(
                                   selectInput("cc_card", 
                                               "Choose credit card:",
                                               choices = unique(creditcard_final_net$last4ccnum),
                                               selected = 3484),
                                   submitButton("Update")
                                 ),
                                 mainPanel(
                                   plotOutput("cc_locations")
                                 )
                               )
                      )),
             tabPanel("Path Visualization", 
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("carid",
                                      label = strong("Car ID of employee:"),
                                      choices = c(1:35),
                                      selected = 1
                          ),
                          radioButtons("weekday",
                                       label = strong("Weekday"),
                                       choices = unique(gps$weekday),
                                       selected = "Monday"),
                          submitButton("Update")
                          ),
                          mainPanel(
                            tmapOutput("mapPlot"),
                            DT::dataTableOutput(outputId = "aTable"),
                            plotlyOutput("gpspath")
                          )
                        )),
             tabPanel("Relationship",
                      
                      
                      plotOutput("relationship"))
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
    #Build new dataset for selected location
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
    #Build new dataset for selected location
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
    
    ggplot(top20, aes(x = word, y = n, label = n)) +
      geom_bar(stat="identity", show.legend = FALSE,
               color = "white", fill = "light blue") +
      coord_flip() +
      labs(title = "Top 20 Most Used Words in News", x = "Word", y = "Word Count") +
      geom_label(aes(fill = word),colour = "white", fontface = "bold", show.legend = FALSE)
  })
  
  output$comparison <- renderPlotly({
    data <- credit_hour_num %>%
      group_by(location,input$choosetype) %>% 
      summarise(frequency=sum(frequency))
    
    name <- names(data)
    
    ggplot(data=data,
           aes(x=name[2],y=location,fill=frequency))+
      geom_tile()+
      scale_fill_gradient2(low="blue",
                           high="red",
                           na.value = 'white')+
      labs(x="Day",
           title="Frequency of Credit Card Purchase")+
      theme(plot.title=element_text(hjust=0.5))
      
  }) 
  
  output$mapPlot <- renderTmap({
    gps_path_selected <- gps_path %>%
      arrange(desc(id)) %>% 
      filter(id==input$carid) %>% 
      filter(weekday==input$weekday)
    
    tm_shape(bgmap) +
      tm_rgb(bgmap, r = 1,g = 2,b = 3,
             alpha = NA,
             saturation = 1,
             interpolate = TRUE,
             max.value = 255) +
      tm_shape(gps_path_selected) +
      tm_facets(by = "id") +
      tm_layout(legend.show=FALSE) +
      tm_lines(col = "weekday", lwd = 7)
  })
  
  output$aTable <- DT::renderDataTable({
    data <- CarTrack %>% 
      filter(CarID==input$carid)

      DT::datatable(data = data,
                    options= list(pageLength = 10),
                    rownames = FALSE)
    
  })
  
  output$cc_locations <- renderPlot({
    data_node <- cc_nodes %>% 
      filter(label==input$cc_card)
    
    data_edges <- cc_edges %>% 
      filter(from==input$cc_card)
    
    cc_graph <- tbl_graph(nodes = data_node, 
                          edges = data_edges, 
                          directed = FALSE) 
    visNetwork(cc_nodes,
               cc_edges) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visOptions(highlightNearest = TRUE,
                 nodesIdSelection = TRUE) %>%
      visLegend() %>%
      visLayout(randomSeed = 123)
    
  })
  
  output$relationship <- renderPlot({
    collapsibleTree(CarTrack,
                    hierarchy = c("CurrentEmploymentType", "CurrentEmploymentTitle","Name"),
                    root = "GASTech",
                    width = 800,
                    fill = c("seashell",
                             rep("brown", length(unique(CarTrack$CurrentEmploymentType))),
                             rep("khaki", length(unique(paste(CarTrack$CurrentEmploymentType,
                                                              CarTrack$CurrentEmploymentTitle)))),
                             rep("forestgreen", length(unique(paste(CarTrack$Name,
                                                                    CarTrack$CurrentEmploymentType))))))
  })

  observeEvent(input$btn_landing, {
    updateTabsetPanel(session,
                      "intabset" ,
                      selected = "tabpanel2")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
