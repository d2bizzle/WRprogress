library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(plotly)
library(quantreg)
library(e1071)
library(timevis)

ui <- fluidPage(
  titlePanel("World Record Progress and Potentially Novel Ways to Detect Doping"),
  theme = shinytheme('superhero'),
  
  
  
  sidebarLayout(
    sidebarPanel(
      selectInput("event", label = h2("Select race for analysis:"), 
                  choices = list("100m" = "hundred", "1500m" = 'fift', "10000m" = 'Tenk'), 
                  selected = 'hundred'),
      
      hr(),
      fluidRow(column(3)),
      
      radioButtons("mode", label = h3("Select either world record or all-time performances"),
                   c("World Record" = "WR",
                     "All-Time" = "AT"
                   )),
      checkboxInput("annul", "Include annulled performance? (For 100m only.)", F)
      
    ),
    
    
    mainPanel(
      
      tabsetPanel(
        tabPanel("Scatter Plots", 
                 plotlyOutput("plot1", height = 406,
                              width = 650),
                 
                 hr(),
                 
                 htmlOutput("video")),
        tabPanel("Box Plots", plotlyOutput("plot2", height = 750,
                                           width = 650)),
        
        tabPanel("Density Plots", plotlyOutput("plot3", height = 750,
                                               width = 650)),
        tabPanel("Timeline", timevisOutput("timeline", height = 750,
                                           width = 650)),
        tabPanel("100 Analysis", textOutput("hundredTest"),
          textOutput("hundredDope"), textOutput("hundredRandom")),
        tabPanel("10000 Analysis", plotlyOutput("plot4", height = 750,
                                                width = 650))
      )
      
                  ))
)

