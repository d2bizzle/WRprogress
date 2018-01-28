library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(plotly)
library(quantreg)
library(e1071)

theme = shinytheme('yeti')

ui <- fluidPage(
  titlePanel("World Record Progress and Potentially Novel Ways to Detect Doping"),
  
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("event", label = h3("Select race for analysis:"), 
                  choices = list("100m" = "hundred", "1500m" = 'fift', "10000m" = 'Tenk'), 
                  selected = 'hundred'),
      
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      
      radioButtons("mode", "Select either world record or all-time performances",
                   c("World Record" = "WR",
                     "All-Time" = "AT"
                   )),
      checkboxInput("annul", "Include annulled performance? (For 100m only.)", T)
    ),
    
    
    mainPanel(
      plotlyOutput("plot"),
      
      hr(),
      
      htmlOutput("video")
                  ))
)

