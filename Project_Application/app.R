library(shiny)
library(shinythemes)
library(sf)
library(tmap)
library(tidyverse)

# Import dataset

# Dataset processing


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel("Multi-row layout vertically in"),
    navlistPanel(
        id = "tabset",
        "Heading 1",
        tabPanel("panel 1", "Panel one contents"),
        "Heading 2",
        tabPanel("panel 2", "Panel two contents"),
        tabPanel("panel 3", "Panel three conten")
                   )
        )

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
