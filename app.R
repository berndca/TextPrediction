library(dplyr)
library(tidytext)
library(data.table)
library(shiny)

fourgrams <- readRDS("data/fourgramsIndex.RDS")
trigrams <- readRDS("data/trigramsIndex.RDS")
bigrams <- readRDS("data/bigramsIndex.RDS")
words <- readRDS("data/wordIndex.RDS")

source("model.R", local = TRUE)

# Define server logic for random distribution app ----
server <- function(input, output) {
  output$prediction <- renderText({ model(input$line) })
}

shinyApp(ui = htmlTemplate("www/index.html"), server)

