library(shiny)
library(shinythemes)
library(sf)
library(tmap)
library(tidyverse)
library(rsconnect)
library(ggplot2)
library(igraph)
library(plotly)

# Import dataset 
#location_num <- read.csv("./data/location num.csv/")

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    navbarPage(
        "Records and Analysis of GAStech Employees",
        tabPanel("Introduction", 
                 "This Shiny application is built for analyzing social situation and behaviors of GAStech employees."),
        navbarMenu("Background Analysis",
                   tabPanel("Popular Locations", 
                            sidebarLayout(
                                sidebarPanel(
                                    radioButtons(
                                        inputId = "location_fre",
                                        label = "Popular locations based on:",
                                        choices = c("Credit Card"="num_credit",
                                                    "Loyalty Card"="num_loyalty"),
                                        selected = "num_credit"
                                        ),
                                    helpText("Data from AT&T (1961) The World's Telephones.")
                                    ),
                                mainPanel(
                                    plotlyOutput("LocationPlot")
                                    )
                                )),
                   tabPanel("Hotpoints of Social News", 
                            sidebarLayout(
                                sidebarPanel("input","one"),
                                mainPanel("output","one")))),
        tabPanel("Consumption Analysis", "one"),
        tabPanel("Path Visualization", "two"),
        tabPanel("Network","three")
        ))


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$LocationPlot <- renderPlotly({
        ggplot(data = location_num,
               aes(x=location,y=input$location_fre))+
            geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
