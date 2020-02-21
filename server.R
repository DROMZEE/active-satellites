#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)

server <- function(input, output) {
    
    output$preview <-  renderDataTable({
        
        req(input$dataFile)
        
        df <- read.delim(input$dataFile$datapath,
                       header = as.logical(input$header),
                       sep = input$sep,
                       quote = input$quote,
                       nrows=10
        ) 
    },  options = list(scrollX = TRUE , dom = 't'))
    
    npathf <- eventReactive(input$goButton, {
        input$dataFile$datapath
    })
    nheader <- eventReactive(input$goButton, {
        as.logical(input$header)
    })
    nsep <- eventReactive(input$goButton, {
        input$sep
    })
    nquote <- eventReactive(input$goButton, {
        input$quote
    })
    output$datafinal <- renderDataTable( {
        
        req(input$dataFile)
        
        dfinal <- read.delim(npathf(),
                         header = nheader(),
                         sep = nsep(),
                         quote = nquote()) 
    },  options = list(scrollX = TRUE , dom = 't'))
    
    
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addTiles %>%
            setView(lng = -1.0259234,15, lat = 43.3273815, zoom = 12) %>% 
            addMarkers(lng = -1.0259234,15, lat = 43.3273815, popup = "HERRIKO") 
    })
    
    
    
    output$c1 <-  renderDataTable({Culdev[input$varx]})
    
    output$text_choice <- renderPrint({
        return(paste0("You have chosen the choice ",input$varx))})
    
    
    
    
    
}
