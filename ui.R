library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(plotly)
library(quantreg)
library(e1071)

ui <- fluidPage(
  titlePanel("World Record Progress and Potentially Novel Ways to Detect Doping"),
  theme = shinytheme('superhero'),
  
  
  
  sidebarLayout(
    sidebarPanel(
      tags$h4(
      selectInput("event", label = h3("Select race for analysis:"), 
                  choices = list("100m" = "hundred", "1500m" = 'fift', "10000m" = 'Tenk'), 
                  selected = 'hundred'),
      
      hr(),
      fluidRow(column(3)),
      
      radioButtons("mode", "Select either world record or all-time performances",
                   c("World Record" = "WR",
                     "All-Time" = "AT"
                   )),
      checkboxInput("annul", "Include annulled performance? (For 100m only.)", F)
      
      )
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Scatter Plots", 
                 plotlyOutput("plot1", height = 406,
                              width = 650),
                 
                 hr(),
                 
                 htmlOutput("video")),
        tabPanel("Box Plots", plotlyOutput("plot2")),
        
        tabPanel("Density Plots", plotlyOutput("plot3")),
        tabPanel("Table", tableOutput("table"))
      )
      
                  ))
)

