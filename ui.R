library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(plotly)
library(quantreg)
library(e1071)
library(timevis)
library(car)

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
        tabPanel("10000 Analysis", plotlyOutput("plot5", height = 406,
                                                width = 650),
                 
                 hr(),
                 plotlyOutput("plot6", height = 406,
                             width = 650)),
        tabPanel("1500 Analysis", plotlyOutput("plot7", height = 750,
                                               width = 650)),
        tabPanel("Discussion",
                 tags$iframe(style="height:700px; width:100%; scrolling=yes", 
                             src="https://www.dropbox.com/s/3rx5ta78c4qd2kx/WRprogress.pdf"))
      )
      
                  ))
)

