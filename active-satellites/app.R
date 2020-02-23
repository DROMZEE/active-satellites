library(shinyWidgets)
library(leaflet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(hrbrthemes)
library(tidyr)

# Tables
#https://www.kaggle.com/ucsusa/active-satellites/download
df <- read.delim('../data/database.csv', 
                 sep =',', 
                 header = TRUE, 
                 na.strings=c('','NA'))

todayDate <- format(Sys.time(), "%a %d %b %Y")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI for application that draws a histogram
ui <- navbarPage(
  'Satellites',
  theme = shinytheme('cerulean'),
  
  tabPanel('Carte Interactive',
           sidebarLayout(
             sidebarPanel(
               img(src = 'user_icon.png', width = 70, align = 'center'),
               h4('B'),
               tags$hr(),
             ),
             mainPanel(
               
               h2('Carte Interactive'),
               h4('Visualisation des pays'),
               leafletOutput("mymap"),
               p(),
               #actionButton("recalc", "Nouveau point")
             )
           )
  ),

  
  # Footer
  tags$footer("Kaggle satellites -  
              Dashboard By Fatima - Davy - Laurent - Cédric DROMZEE - 2020",
              align = "center", 
              style = "
              position:absolute;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: #3A9AD1;
              z-index: 1000;")
  )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  ########################################
  ##### Cartes
  

  villes <- data.frame(Ville = c("Bayonne"),
                       Latitude = c(43.483333),
                       Longitude = c(-1.483333),
                       Population = c(4115))
  
 
  couleurs <- colorNumeric("YlOrRd", villes$Population, n = 5)
  
  output$mymap <- renderLeaflet({
    leaflet(villes) %>% 
      addTiles() %>%
      addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
                 radius = ~sqrt(Population) * 50, popup = ~paste(Ville, ":", Population),
                 color = ~couleurs(Population), fillOpacity = 0.9) %>%
      addLegend(pal = couleurs, values = ~Population, opacity = 0.9)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)