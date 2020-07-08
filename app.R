#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(tidyverse)
source('mod_REDCap.R')

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    ),
    actionButton(inputId = 'stop',label = 'Pause for reflection'),
    redcap_setup_ui('redcap-setup-namespace'),
    selectInput(inputId = 'subject_id',label = 'Subject ID',choices = c('922873','922874')), ## test subject id provider
    redcap_instrument_ui('redcap-instrument-namespace')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    subject_id <- reactive({ input$subject_id })
    redcap_setup_values <- callModule(redcap_setup_server,'redcap-setup-namespace')
    redcap_instrument_values <- callModule(redcap_instrument_server, 'redcap-instrument-namespace', redcap_setup_values, subject_id)
    observeEvent(input$stop,{
        browser()
    })
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
