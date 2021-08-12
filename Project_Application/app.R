library(shiny)
library(shinythemes)
library(sf)
library(tmap)
library(tidyverse)
library(rsconnect)
library(ggplot2)

# Import dataset
location_frequency <- read_csv("./data/location frequency.csv")

# Module for popular locations
histogramUI <- function(id) {
    tagList(
        checkboxGroupInput(
            NS(id,"cardtype"),
            label = "Popular locations based on:",
            choices = c("Credit Card"="num_credit",
                        "Loyalty Card"="num_loyalty"),
            selected = "num_credit"
            ),
        plotOutput(NS(id,"hist"))
    )
}

histogramServer <- function(id) {
    moduleServer(id, function(input, output, session) {
        output$hist <- renderPlot({
            ggplot(location_frequency, 
                   aes(input$location,input$num_credit)) +
                geom_bar(bins = 20,
                               color="black",
                               fill="light blue")
        }, res = 96)
    })
}

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
                                    histogramUI()  
                                )),
                                mainPanel("output")
                            )),
                   tabPanel("Hotpoints of Social News", 
                            sidebarLayout(
                                sidebarPanel("input"),
                                mainPanel("output")))),
        tabPanel("panel 1", "one"),
        tabPanel("panel 2", "two"),
        tabPanel("panel 3", "three")
        )

# Define server logic required to draw a histogram
server <- function(input, output) {
    histogramServer("hist")
}

# Run the application 
shinyApp(ui = ui, server = server)
