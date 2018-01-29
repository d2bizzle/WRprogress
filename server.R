server <- function(input, output) {
  
  a = "https://www.youtube.com/embed/9e6Cfq_YchM?autoplay=1"
  b = "https://www.youtube.com/embed/XvCsj7eJKKA?start=213&autoplay=1"
  c = "https://www.youtube.com/embed/ujhIQBJpIag?autoplay=1&start=27"
  
  red.bold.italic.text <- element_text(face = "bold.italic", color = "red")
  blue.bold.italic.text <- element_text(face = "bold.italic", color = "blue", size = 12)
  
  hundred_WR <- WR %>% filter(event == 100, notes == 'WR') %>% select(digitalTime, error, year)
  hundred_WRplus <- WR %>% filter(event == 100, notes != 'AT') %>% select(digitalTime, error, year, notes)
  fift_WR <- WR %>% filter(event == 1500, notes == 'WR') %>% select(digitalTime, time, year)
  Tenk_WR <- WR %>% filter(event == 10000, notes == 'WR') %>% select(digitalTime, time, year)
  
  hundred_AT <- WR %>% filter(event == 100, notes == 'AT') %>% select(digitalTime, error, year)
  hundred_ATplus <- WR %>% filter(event == 100, notes != 'WR') %>% select(digitalTime, error, year, notes)
  fift_AT <- WR %>% filter(event == 1500, notes == 'AT') %>% select(digitalTime, time, year)
  Tenk_AT <- WR %>% filter(event == 10000, notes == 'AT') %>% select(digitalTime, time, year)

  
  hundred_WR_plot_plus <-  ggplot(hundred_WRplus, aes(x = year, y = digitalTime, color = notes)) + geom_point() + labs(x = 'Year', y = 'Time (s)') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1) + ggtitle("100m WR Over Time (Incl. Annulled Performances)") +
    theme_tufte() + theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
                                                                        axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
  hundred_WR_plot <-  ggplot(hundred_WR, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'Year', y = 'Time (s)') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1) + ggtitle("100 m WR Over Time") +
    theme_tufte() + theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
                                                                         axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
  fift_WR_plot = ggplot(fift_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') +
    theme_tufte() + theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
                          axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
  Tenk_WR_plot = ggplot(Tenk_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + 
    theme_tufte() + theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
                axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
  hundred_AT_plot_plus = ggplot(hundred_ATplus, aes(x = year, y = digitalTime, color = notes)) + geom_point() + labs(x = 'Year', y = 'Time(s)') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1) + theme_tufte() + 
    theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
              axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
  hundred_AT_plot = ggplot(hundred_AT, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'Year', y = 'Time(s)') + 
    geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1) + theme_tufte() + 
    theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
          axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
  fift_AT_plot = ggplot(fift_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + theme_tufte() + 
    theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
          axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
  Tenk_AT_plot = ggplot(Tenk_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + theme_tufte() + 
    theme(title = red.bold.italic.text, axis.title = red.bold.italic.text, 
          axis.text.x = blue.bold.italic.text, axis.text.y = blue.bold.italic.text)
  
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
      tags$iframe(src = link, width = 650, height = 433)
    })
  })
  
}

