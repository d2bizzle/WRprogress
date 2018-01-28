server <- function(input, output) {
  
  a = "https://www.youtube.com/embed/wE8NDuzt8eg"
  b = "https://www.youtube.com/embed/Q_9AAy7yZTc"
  c = "https://www.youtube.com/watch?v=9-gOCOu_KGU"
  
  hundred_WR <- WR %>% filter(event == 100, notes == 'WR') %>% select(digitalTime, error, year)
  hundred_WRplus <- WR %>% filter(event == 100, notes != 'AT') %>% select(digitalTime, error, year)
  fift_WR <- WR %>% filter(event == 1500, notes == 'WR') %>% select(digitalTime, time, year)
  Tenk_WR <- WR %>% filter(event == 10000, notes == 'WR') %>% select(digitalTime, time, year)
  
  hundred_AT <- WR %>% filter(event == 100, notes == 'AT') %>% select(digitalTime, error, year)
  hundred_ATplus <- WR %>% filter(event == 100, notes != 'WR') %>% select(digitalTime, error, year)
  fift_AT <- WR %>% filter(event == 1500, notes == 'AT') %>% select(digitalTime, time, year)
  Tenk_AT <- WR %>% filter(event == 10000, notes == 'AT') %>% select(digitalTime, time, year)

  
  hundred_WR_plot_plus <-  ggplot(hundred_WRplus, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'year', y = 'Time') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1)
  
  hundred_WR_plot <-  ggplot(hundred_WR, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'year', y = 'Time') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1)
  
  fift_WR_plot = ggplot(fift_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'year', y = 'Time')
  
  Tenk_WR_plot = ggplot(Tenk_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'year', y = 'Time')
  
  hundred_AT_plot_plus = ggplot(hundred_ATplus, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'year', y = 'Time') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1)
  
  hundred_AT_plot = ggplot(hundred_AT, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'year', y = 'Time') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1)
  
  fift_AT_plot = ggplot(fift_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'year', y = 'Time')
  
  Tenk_AT_plot = ggplot(Tenk_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'year', y = 'Time')
  
  observe({
    
    if (input$event == 'hundred') {
      link = a
    } else if (input$event == 'fift') {
      link = b
    } else {
      link = c
    }
    
    if (input$mode == 'WR') {
      Event_plot <- switch(input$event,
                           hundred = hundred_WR_plot,
                           fift = fift_WR_plot,
                           Tenk = Tenk_WR_plot,
                           hundred_WR_plot_plus)
    } else {
    Event_plot <- switch(input$event,
                 hundred = hundred_AT_plot,
                 fift = fift_AT_plot,
                 Tenk = Tenk_AT_plot,
                 hundred_AT_plot_plus)}
    
    if (input$annul && input$event == 'hundred') {
      Event_plot <- switch(input$mode,
                           "AT" = hundred_AT_plot_plus,
                           "WR" = hundred_WR_plot_plus)
    }
    output$plot <- renderPlotly({
      
      Event_plot
    })
    
    output$video <- renderUI({
      tags$iframe(src = link, width = 600, height = 400)
    })
  })
  
}

