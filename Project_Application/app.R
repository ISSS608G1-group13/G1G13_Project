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

packages = c("tidyverse","tidyr","tidyverse","dplyr","sp","raster","sf",
             "vctrs","clock","tmap","rgdal","readr","ggplot2","plotly","tmap","gganimate","av","gifski",
             "igraph","tidygraph","ggraph","visNetwork","lubridate","DT","collapsibleTree")
for (p in packages){
    if(!require(p, character.only = T)){
        install.packages(p)
    }
    library(p, character.only = T)
}

# Import dataset 
#location_num <- read.csv("./location num.csv/")
#bgmap <- raster("./MC2-tourist.tif")
#Abila_st <- st_read(dsn = "./Geospatial",
 #                   layer = "Abila")
#gps <- read_csv("./gps.csv")
#location_frequency <- read.csv("./location frequency.csv/")
#carassign <- read_csv("./car-assignments.csv")
#creditcard <- read.csv("./cc_data.csv")
#loyaltycard <- read.csv("./loyalty_data.csv")

# Dataset Process
#gps$Timestamp <- date_time_parse(gps$Timestamp,
#                                 zone="",
#                                 format="%m/%d/%Y %H:%M")
#creditcard$timestamp <- date_time_parse(creditcard$timestamp,
#                                        zone="",
#                                        format="%m/%d/%Y %H:%M")

#loyaltycard$timestamp <- date_time_parse(loyaltycard$timestamp,
#                                         zone="",
#                                         format="%m/%d/%Y")

# Build new columns for day and hour and change the data type
credit_hour <- creditcard
credit_hour$hour <- as.numeric(get_hour(credit_hour$timestamp))
credit_hour$day <- as.numeric(get_day(credit_hour$timestamp))

# Wrangle the dataset for the frequency of purchase for every day
credit_hour_num <- credit_hour %>% 
    group_by(location,day,hour) %>% 
    summarise(frequency=n())


ui <- fluidPage(
    theme = shinytheme("cerulean"),
    navbarPage(
        "GAStech Employees Behavior Analysis",
        tabPanel("Introduction", 
                 "This Shiny application is built for analyzing social situation and behaviors of GAStech employees."),
        navbarMenu("Background Analysis",
                   tabPanel("Popular Locations", 
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
                   tabPanel("Hotpoints of Social News", 
                            sidebarLayout(
                                sidebarPanel(
                                    "input"
                                ),
                                mainPanel(
                                    "output"
                                )
                            )
                            )),
        tabPanel("Consumption Analysis",
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
               aes(x = frequency, y= location)) + 
            geom_bar(stat = "identity", 
                     color = "grey", fill = "light blue")+
            ylab("Location") + 
            xlab("Frequency")+
            ggtitle("The Frequency of Each Location")
    })
    
    output$consumption_hour <- renderPlotly({
        # Build new dataset for selected location
        data <- credit_hour_num %>% 
            filter(location==input$location) 
        
        # Draw the heatmap
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
        # Build new dataset for selected location
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

}

# Run the application 
shinyApp(ui = ui, server = server)
