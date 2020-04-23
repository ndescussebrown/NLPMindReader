#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(data.table)
source('textpredictbitrigram.R')
DTngram <- fread("data/DTbitrisplit.csv",quote="")  

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    data <- reactive({
        req(input$sentence)
        nextword <- textpredictbitrigram(input$sentence,DTngram)
        nextword
    })
    output$nextword <- renderText((data()))
    
})

