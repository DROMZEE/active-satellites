options(shiny.maxRequestSize = 30*1024^2)

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("leaflet")

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)



r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

df <- read.delim('data/database.csv', sep =',', header = TRUE)
df <- na.omit(df) 

# Culdev <- read.delim("C:/perso/ble/data/FDS_DEVELOPPE_2017.csv", header=TRUE, sep=";")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "satellite"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Lecture des données", tabName = "lecture", icon = icon("readme")),
            menuItem("Visualisation des données",tabName = "visu"),
            menuItem("carte", tabName = "carte"),
            
            menuItem("courbe",tabName = "courbes",
                     icon = icon("dashboard"),
                     menuSubItem("colonnes",icon("list")),
                     checkboxGroupInput(inputId ="varx","choisir les colonnes:",
                                        choiceNames = names(Culdev),
                                        choiceValues = names(Culdev)))
        )
    ),
    dashboardBody(
        tabItems(
            # Read data
            tabItem(tabName = "lecture",
                    h1("Lecture des données"),
                    fileInput("dataFile",label = NULL,
                              buttonLabel = "Browse...",
                              placeholder = "pas de fichier selectionne"),
                    
                    h3("Parameters"),
                    
                    # choisir header ---
                    radioButtons(inputId = "header", 
                                 label = "Header",
                                 choices = c("Oui" = TRUE,
                                             "Non" = FALSE),
                                 selected = TRUE, inline=T),
                    
                    # choisir sep  ----
                    radioButtons(inputId = "sep", 
                                 label = "Separator",
                                 choices = c(Virgule = ",",
                                             pointVirgule = ";",
                                             Tab = "\t"),
                                 selected = "t", inline=T),
                    
                    # choisir----
                    radioButtons(inputId = "quote", 
                                 label= "Quote",
                                 choices = c(None = "",
                                             guillemet = '"',
                                             apostrophe = "'"),
                                 selected = "", inline=T),
                    #boutonpour valider
                    h3("valider l'integration du data"),
                    actionButton("action", label = "Action"),
                    
                    h3("File preview"),
                    dataTableOutput(outputId = "preview")
            ),
            
            
            
            # visualization carte
            tabItem(tabName = "carte",
                    h1("Visualisation des cartes" ),
                    fluidPage(
                        leafletOutput("mymap"),
                        p()
                    )
                    
            ),
            
            # visuel
            tabItem(tabName = "visu",
                    h1("au secours"),
                        dataTableOutput(outputId = "datafinal")
            ),
            
            # courbes
            tabItem(tabName = "courbes",
                    h1("au secours"),
                    fluidPage(
                        verbatimTextOutput("text_choice"))
                        )
        )
    )
    )

