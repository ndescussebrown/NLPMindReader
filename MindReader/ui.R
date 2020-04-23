#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# This app displays a list of ParkRun location within the UK and ranks them according to the distance from
# a particular postcode and enables selection of the ParkRun that start with a particular letter. The list of ParkRusn in the UK is scraped from 
# https://en.wikipedia.org/wiki/List_of_Parkruns_in_the_United_Kingdom. Before scraping I ensured I was
# authorised by using the paths_allowed function in the robotstxt package.
# paths_allowed(
# paths = "/wiki/List_of_Parkruns_in_the_United_Kingdom",
# domain = "https://en.wikipedia.org", 
# bot    = "*")

library(shiny)
library(shinycssloaders)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("cyborg"),
    

    tags$head(
        tags$style(HTML("
                @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                   
                h1 {font-family: 'Magneto', cursive;
                color: red;
                }
        ")))
    ,
    
    headerPanel("MindReader"),
    
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            "This app can guess what you're going to say next!",
            br(),br(),
            textInput("sentence","Enter your sentence and wait for MindReader to predict your next word:",value = "", width = NULL,
                      placeholder = NULL)

        ),
        
        # Show the list of ParkRuns requested
        mainPanel(
            
            tags$style("#nextword {font-size:50px;
               color:red;
               display:block;  align='center'}"),
            # textOutput("nextword")
            # html("<p align='center'>x2</p>"),
            # h1(textOutput("nextword"), align='center')
            # html("<p align='center'>previous week: A</br>")
            # withSpinner(h1(textOutput("nextword"), align='center'),color="#0dc5c1")
            div(style="text-align:center;padding: 100px 0;",
            withSpinner(textOutput("nextword"),color="#0dc5c1"))
        )
        
    )
))
