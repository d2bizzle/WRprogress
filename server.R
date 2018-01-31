server <- function(input, output) {
  
  a = "https://www.youtube.com/embed/9e6Cfq_YchM?autoplay=1"
  b = "https://www.youtube.com/embed/XvCsj7eJKKA?start=213&autoplay=1"
  c = "https://www.youtube.com/embed/ujhIQBJpIag?autoplay=1&start=27"
  
  hundred_WR <- WR %>% filter(event == 100, notes == 'WR') %>% select(digitalTime, error, year)
  hundred_WRplus <- WR %>% filter(event == 100, notes != 'AT') %>% select(digitalTime, error, year, notes)
  fift_WR <- WR %>% filter(event == 1500, notes == 'WR') %>% select(digitalTime, time, year)
  Tenk_WR <- WR %>% filter(event == 10000, notes == 'WR') %>% select(digitalTime, time, year)
  
  hundred_AT <- WR %>% filter(event == 100, notes == 'AT') %>% select(digitalTime, notes, error, year)
  hundred_ATplus <- WR %>% filter(event == 100, notes != 'WR') %>% select(digitalTime, error, year, notes)
  fift_AT <- WR %>% filter(event == 1500, notes == 'AT') %>% select(digitalTime, time, year)
  Tenk_AT <- WR %>% filter(event == 10000, notes == 'AT') %>% select(digitalTime, time, year)
  
  hundred_random <- hundred_AT %>% select(digitalTime, notes, year, -error) %>% subset(year > 1984) %>% sample_n(121)
  hundred_dope <- WR %>% filter(event == 100, notes == 'doping') %>% select(digitalTime, notes, year)
  hundred_analysis = full_join(hundred_random, hundred_dope)
  effective100 <- wilcox.test(hundred_analysis$digitalTime~hundred_analysis$notes)
  dope100 <- hundred_dope %>% summarise(mean = mean(digitalTime, na.rm=T), sd = sd(digitalTime, na.rm=T))
  random100 <- sample_n(hundred_random, 121) %>% summarise(mean = mean(digitalTime, na.rm=T), sd = sd(digitalTime, na.rm=T))
  
  TenkTop = Tenk_AT %>% group_by(year) %>% arrange(digitalTime) %>% top_n(-30, wt = digitalTime)
  Top30Tenk = TenkTop %>% group_by(year) %>% summarise(meanTime = mean(digitalTime), sdTime = sd(digitalTime),
                                                       skewTime = skewness(digitalTime), kurtTime = kurtosis(digitalTime))
  
  Top30TenkPlot = ggplot(Top30Tenk, aes(year,sdTime)) + geom_point() + geom_smooth(method = "lm") +
    labs(x = 'Year', y = 'sd(Time (s))') + ggtitle("10000m Standard Deviation Over Time") + theme_economist() + scale_color_economist()

  
hundred_WR_plot_plus <- ggplot(hundred_WRplus, aes(x = year, y = digitalTime, color = notes)) + geom_point() + labs(x = 'Year', y = 'Time (s)') + 
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="blue", width=.1) + ggtitle("100m WR Over Time (Incl. Annulled)") + theme_economist() + scale_color_economist()
  
hundred_WR_plot <-  ggplot(hundred_WR, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'Year', y = 'Time (s)') + 
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="blue", width=.1) + ggtitle("100m WR Over Time") +
theme_economist() + scale_color_economist()
  
fift_WR_plot = ggplot(fift_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + 
ggtitle("1500m WR Over Time") + theme_economist() + scale_color_economist()
  
Tenk_WR_plot = ggplot(Tenk_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + 
ggtitle("10000m WR Over Time") + theme_economist() + scale_color_economist()
  
hundred_AT_plot_plus = ggplot(hundred_ATplus, aes(x = year, y = digitalTime, color = notes)) + geom_point() + labs(x = 'Year', y = 'Time(s)') + 
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1) + theme_economist() + scale_color_economist() + ggtitle("100m All-Time Performances (Inc. Annulled)")
  
hundred_AT_plot = ggplot(hundred_AT, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'Year', y = 'Time(s)') + ggtitle("100m All-Time Performances") +
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1) + theme_economist() + scale_color_economist()

fift_AT_plot = ggplot(fift_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + theme_economist() + scale_color_economist() + ggtitle("1500m All-Time Performances")
  
Tenk_AT_plot = ggplot(Tenk_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + theme_economist() + scale_color_economist() + ggtitle("10000m All-Time Performances")
  
hundred_Boxplot = ggplot(hundred_AT, aes(x = year, y = digitalTime)) + geom_boxplot() + labs(x = 'Year', y = 'Time(s)')  + 
theme_economist() + scale_color_economist() + ggtitle("100m All-Time Performances") 
  
fift_Boxplot = ggplot(fift_AT, aes(year, digitalTime, label = time)) + geom_boxplot() + labs(x = 'Year', y = 'Time(m)') + 
theme_economist() + scale_color_economist() + ggtitle("1500m All-Time Performances")
  
Tenk_Boxplot = ggplot(Tenk_AT, aes(year, digitalTime, label = time)) + geom_boxplot() + labs(x = 'Year', y = 'Time(m)') + 
theme_economist() + scale_color_economist() + ggtitle("10000m All-Time Performances")
  
  hundred_density = ggplot(hundred_ATplus, aes(x = digitalTime)) + geom_density(aes(group = notes, colour = notes, fill = notes), alpha = 0.3) + 
    labs(x = "Time (s)", y = "Frequency", title = "World's All-Time 100m Performance List")
  
  fift_density = ggplot(fift_AT, aes(x = digitalTime, label = time)) + geom_density(aes(fill="red"), alpha = 0.3) + 
    labs(x = "Time (s)", y = "Frequency", title = "World's All-Time 1500m Performance List")
  
  Tenk_density = ggplot(Tenk_AT, aes(x = digitalTime, label = time)) + geom_density(aes(fill="red"), alpha = 0.3) + 
    labs(x = "Time (m)", y = "Frequency", title = "World's All-Time 10000m Performance List")
  
  Top30TenkPlot = ggplot(Top30Tenk, aes(year,sdTime)) + geom_point() + geom_smooth(method = "lm") +
    labs(x = 'Year', y = 'sd(Time (s))') + ggtitle("10000m Standard Deviation Over Time") + theme_economist() + scale_color_economist()
  
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
    
      Event_Boxplot <- switch(input$event,
                           hundred = hundred_Boxplot,
                           fift = fift_Boxplot,
                           Tenk = Tenk_Boxplot)
      
      Event_density <- switch(input$event,
                              hundred = hundred_density,
                              fift = fift_density,
                              Tenk = Tenk_density)
    
    output$plot1 <- renderPlotly({
      
      Event_plot
    })
    
    output$plot2 <- renderPlotly({
      
      Event_Boxplot
    
    })
    
    output$plot3 <- renderPlotly({
      
      Event_density
      
    })
    
    output$timeline <- renderTimevis({
      
      timevis(timeData)
      
    })
    
    output$video <- renderUI({
      tags$iframe(src = link, width = 650, height = 406)
    })
    output$hundredTest <- renderText({
      sprintf("The result of the Wilcoxon test gives a p-value of %0.3f", effective100[3])
    })
    output$hundredDope <- renderText({
      sprintf("The mean of the doped samples is %2.3f and the standard deviation is %0.3f", dope100[1],dope100[2])
    })
    output$hundredRandom <- renderText({
      sprintf("The mean of the doped samples is %2.3f and the standard deviation is %0.3f", random100[1],random100[2])
    })
    output$plot4 <- renderPlotly({
      
      Top30TenkPlot
      
    })
  })
  
}

