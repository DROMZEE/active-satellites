library(shinyWidgets)
library(leaflet)
library(dplyr)
library(shiny)
#library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(plotly)
library(hrbrthemes)
library(tidyr)
library(DT)
#library(sf)

# Tables
#https://www.kaggle.com/ucsusa/active-satellites/download
df_as <- read.delim('data/active-satellites.csv', 
                    sep =';', 
                    header = TRUE,
                    encoding="UTF-8",
                    na.strings=c('','NA'))

#gps <- st_read("data/doc.kml")
sites <- read.csv("data/sites.csv",
                  sep =';',
                  header = TRUE,
                  encoding="UTF-8")

todayDate <- format(Sys.time(), "%a %d %b %Y")

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI for application that draws a histogram
ui <- navbarPage(
    'Satellites',
    theme = shinytheme('cerulean'),
    #shinythemes::themeSelector(),
    
    
    # Titre de l’application 
    #titlePanel("Data exploration"),
    
    # header <- dashboardHeader(title = "Mon application Shinydashboard", 
    #                           titleWidth = 350) ,
    
    #Onglet 1 : graphique interactif
    tabPanel('Graphique interactif',
             sidebarLayout(
                 sidebarPanel(
                         
                         helpText("Modifier les variables"),
                         uiOutput(outputId = "numSelectorx"),
                         uiOutput(outputId = "numSelectory")
                         # selectInput("varX", 
                         #             label = "sélectionnez la variables sur l'axe X",
                         #             choices = c("Opérateur.propriétaire" = "Opérateur.propriétaire", 
                         #                         "Utilisateurs" = "Utilisateurs",
                         #                         "fournisseur" = "fournisseur", 
                         #                         "Site.de.lancement" = "Site.de.lancement"),
                         #             selected = "Site.de.lancement"),
                         # 
                         # selectInput("varY", 
                         #             label = "sélectionnez la variables sur l'axe Y",
                         #             choices = c("Opérateur.propriétaire" = "Opérateur.propriétaire", 
                         #                         "Utilisateurs" = "Utilisateurs",
                         #                         "fournisseur" = "fournisseur", 
                         #                         "Site.de.lancement" = "Site.de.lancement"),
                         #             selected = "Utilisateurs"),
                         # 
                         # actionButton("update", "afficher")
                         # #mainPanel("Plot",  plotOutput("Plot1"))
                         
                     
                     #mainPanel("Plot",  plotOutput("Plot1"))
                 ),
                 mainPanel("Plot", 
                           h3("ausecours"),plotOutput("Plot1"))
                 
                 
             )
    ),
    
    # onglet test plotlyOutput("plot"))
    # 
    # tabPanel("Graphiques", 
    #          h1("Graphiques"), 
    #          h4("Choisissez deux variables à représenter :"), 
    #          selectInput("variable_x", "Variable X:", 
    #                      # list("libellé = "nom.variable") 
    #                      list("Opérateur.propriétaire" = "Opérateur.propriétaire", 
    #                           "Utilisateurs" = "Utilisateurs",
    #                           "fournisseur" = "fournisseur", 
    #                           "Site.de.lancement" = "Site.de.lancement")), 
    #          selectInput("variable_y", "Variable Y:", 
    #                      list("Opérateur.propriétaire" = "Opérateur.propriétaire", 
    #                           "Utilisateurs" = "Utilisateurs",
    #                           "fournisseur" = "fournisseur", 
    #                           "Site.de.lancement" = "Site.de.lancement")),
    # plotlyOutput("plot")),
    
    
    
    # onglet 2 : Résumé statistique
    tabPanel( 
      "Résumé statistique", 
      h1("Tableau descriptif"), 

      #rendu fonction summary 
      verbatimTextOutput("summary"), 
      h2("Structure des données"), 
      #rendu fonction str 
      verbatimTextOutput("str") 
    ),
    
    
    #Onglet 2 : tableau
    tabPanel('Données',
             sidebarLayout(
                 sidebarPanel(
                     img(src = 'user_icon.png', width = 70, align = 'center'),
                     #h4('Benoit Jean'),
                     tags$hr(),
                     helpText('Filtrez les données en sélectionnant les critères qui vous intéressent.'),
                     
                     selectInput('sel_sat',
                                 'satellite :',
                                 c('Tout',
                                   unique(as.character(df_as$Nom.officiel.du.satellite)))),
                     
                     selectInput('sel_type',
                                 'Utilisateurs :',
                                 c('Tout',
                                   unique(as.character(df_as$Utilisateurs)))),
                     
                     selectInput('sel_objt',
                                 'Objectif:',
                                 c('Tout',
                                   unique(as.character(df_as$Objectif)))),
                     
                     selectInput('sel_site',
                                 'Site.de.lancement :',
                                 c('Tout',
                                   unique(as.character(df_as$Site.de.lancement)))),
                     tags$hr()
                     
                 ),
                 
                 mainPanel(
                     h2('Satellite'),
                     DT::dataTableOutput('satellite')
                 )
             )
    ),
    
    #Onglet carte
    tabPanel('Carte Interactive',
             sidebarLayout(
                 sidebarPanel(
                     #img(src = 'user_icon.png', width = 70, align = 'center'),
                     h4('B'),
                     tags$hr()
                 ),
                 mainPanel(
                     
                     h2('Carte Interactive'),
                     h4('Visualisation des sites de lancements'),
                     leafletOutput("mymap"),
                     p()
                 )
             )
    )
    
    
)
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    output$numSelectorx <- renderUI({
        selectInput("varx", h3("choisir une variable X"), 
                    choices = names(df_as), selected = 1)
    })
    
    
    
    output$numSelectory<- renderUI({
        selectInput("vary", h3("choisir une variable y"), 
                    choices = names(df_as), selected = 1)
    })
    
 
    
    output$Plot1 <- renderPlot({
        
        plot(x=df_as[[input$varx]],y=df_as[[input$vary]],xlim = input$varx,ylim = input$vary)
        
    })
    
    # # Onglet Graphiques, deux variables 
    # output$plot <- renderPlotly({ 
    #   g <- ggplot(df_as, aes_string(x=input$variable_x, 
    #                                y=input$variable_y, 
    #                                colour="Utilisateurs")) + 
    #     #point() + 
    #     #geom_smooth(method = "lm") 
    #     geom_histogram()
    #   ggplotly(g) 
    # }) 
    
    ### Onglet 2
    
    
    # résumé des données 
    output$summary <- renderPrint({ 
      summary(df_as) 
    }) 
    
    # structure des données 
    output$str <- renderPrint({ 
      str(df_as) 
    })
    
    #### Tab
    #df_as$Nom.officiel.du.satellite
    
    output$satellite <- DT::renderDataTable(DT::datatable({
        data <- df_as
        if (input$sel_sat != 'Tout'){
            data <- data[data$Nom.officiel.du.satellite == input$sel_sat, ]
        }
        if (input$sel_type != 'Tout'){
            data <- data[data$Utilisateurs == input$sel_type, ]
        }
        if (input$sel_objt != 'Tout'){
            data <- data[data$Objectif == input$sel_objt, ]
        }
        if (input$sel_site != 'Tout'){
            data <- data[data$Site.de.lancement == input$sel_site, ]
        }
        data
    }))
    
    ########################################
    ##### Cartes
    
    
    # villes <- data.frame(Ville = c("Bayonne","kourou"),
    #                      Latitude = c(43.483333,5.1694),
    #                      Longitude = c(-1.483333,-52.6832),
    #                      Population = c(4115,5115))
    
    # sites <- data.frame(Site = c(sites$Launch.Site),
    #                      Latitude = c(geoLat),
    #                      Longitude = c(sites$geoLon),
    #                      Population = c(sites$number))
    
    
    couleurs <- colorNumeric("YlOrRd", sites$number, n = 5)
    
    output$mymap <- renderLeaflet({
        leaflet(sites) %>% 
            addTiles() %>%
            addCircles(lng = ~geoLon, lat = ~geoLat, weight = 1,
                       radius = ~sqrt(number) * 50000, popup = ~paste(number),
                       color = ~couleurs(number), fillOpacity = 0.9) %>%
            addLegend(pal = couleurs, values = ~number, opacity = 0.9)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)