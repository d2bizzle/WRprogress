server <- function(input, output) {
  
  # links that will display on each event tab
  a = "https://www.youtube.com/embed/9e6Cfq_YchM?autoplay=1"
  b = "https://www.youtube.com/embed/XvCsj7eJKKA?start=213&autoplay=1"
  c = "https://www.youtube.com/embed/ujhIQBJpIag?autoplay=1&start=27"
  
  #parceling data out to individual frames for plots, in the future would really like to have this reactive
  
  hundred_WR <- WR %>% filter(event == 100, notes == 'WR') %>% select(digitalTime, error, year, notes)
  hundred_WRplus <- WR %>% filter(event == 100, notes != 'AT') %>% select(digitalTime, error, year, notes)
  fift_WR <- WR %>% filter(event == 1500, notes == 'WR') %>% select(digitalTime, time, year)
  Tenk_WR <- WR %>% filter(event == 10000, notes == 'WR') %>% select(digitalTime, time, year)
  
  hundred_AT <- WR %>% filter(event == 100, notes == 'AT') %>% select(digitalTime, notes, error, year)
  hundred_ATplus <- WR %>% filter(event == 100, notes != 'WR') %>% select(digitalTime, error, year, notes)
  fift_AT <- WR %>% filter(event == 1500, notes == 'AT') %>% select(digitalTime, time, notes, year)
  Tenk_AT <- WR %>% filter(event == 10000, notes == 'AT') %>% select(digitalTime, time, notes, year)
  
  hundred_random <- hundred_AT %>% select(digitalTime, notes, year, -error) %>% subset(year > 1984) %>% sample_n(121)
  hundred_dope <- WR %>% filter(event == 100, notes == 'doping') %>% select(digitalTime, notes, year)
  hundred_analysis = full_join(hundred_random, hundred_dope)
  levene100 <- leveneTest(digitalTime~notes, hundred_analysis)
  wilcox100 <- wilcox.test(digitalTime~notes, hundred_analysis)
  dope100 <- hundred_dope %>% summarise(medianTime = median(digitalTime), sd = sd(digitalTime, na.rm=T))
  random100 <- sample_n(hundred_random, 121) %>% summarise(medianTime = median(digitalTime),sd = sd(digitalTime, na.rm=T))
  
  fift_elite <- WR %>% filter(event == 1500, notes != "WR", digitalTime < 3.52) %>% select(digitalTime, time, year, notes) %>% 
    mutate(notes = case_when(year < 1990 ~ 'pre', year > 1990 & year < 2006 ~ 'epo', year > 2005 ~ 'pst'))
  fift_elite <- fift_elite %>% mutate(notes = as.factor(notes))
  fift_elite1990 <- fift_elite %>% filter(year > 1989)
  
  kruskal1500 <-  kruskal.test(digitalTime~notes, fift_elite)
  wilcox1500 <- wilcox.test(digitalTime~notes, fift_elite1990)
  
  TenkTop = Tenk_AT %>% group_by(year) %>% arrange(digitalTime) %>% top_n(-30, wt = digitalTime)
  Top30Tenk = TenkTop %>% group_by(year) %>% filter(year > 1971) %>% summarise(meanTime = mean(digitalTime), sdTime = sd(digitalTime),
                                                       skewTime = skewness(digitalTime), kurtTime = kurtosis(digitalTime))
  TenkTop = Tenk_AT %>% group_by(year) %>% arrange(digitalTime) %>% top_n(-30, wt = digitalTime)
  
  TenkTopPre = Tenk_AT %>% filter(year > 1973 & year < 1990) %>% group_by(year) %>% arrange(digitalTime) %>% top_n(-30, wt = digitalTime) %>% mutate(notes = "pre")
  TenkTopEPO = Tenk_AT %>% filter(year > 1990 & year < 2006) %>% group_by(year) %>% arrange(digitalTime) %>% top_n(-30, wt = digitalTime) %>% mutate(notes = "epo")
  TenkTopPst = Tenk_AT %>% filter(year > 2005) %>% group_by(year) %>% arrange(digitalTime) %>% top_n(-30, wt = digitalTime) %>% mutate(notes = "pst")
  
  Tenk_Analysis = rbind(TenkTopPre, TenkTopEPO, TenkTopPst) 
  Tenk_Analysis$notes <- as.factor(Tenk_Analysis$notes)
  
  kruskal10k <-  kruskal.test(digitalTime~notes, Tenk_Analysis)
  
  Tenk_summary = Tenk_Analysis %>% group_by(notes) %>% summarise(Median = median(digitalTime),SD = sd(digitalTime),
                                                                 Skew = skewness(digitalTime), Kurtosis = kurtosis(digitalTime))
  
  sub27_10k <- Tenk_Analysis %>% arrange(digitalTime) %>% filter(digitalTime < 27.0)
  
  elite10k <- sub27_10k %>% group_by(notes) %>% summarise(Median = median(digitalTime),SD = sd(digitalTime),
                                                          Skew = skewness(digitalTime), Kurtosis = kurtosis(digitalTime))
  sub27_test <- wilcox.test(digitalTime~notes, sub27_10k)
  
hundred_WR_plot_plus <- ggplot(hundred_WRplus, aes(x = year, y = digitalTime, color = notes)) + geom_point() + labs(x = 'Year', y = 'Time (s)') + 
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="blue", width=.1) + ggtitle("100m WR Over Time (Incl. Annulled)") + scale_color_economist()
  
hundred_WR_plot <-  ggplot(hundred_WR, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'Year', y = 'Time (s)') + 
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="blue", width=.1) + ggtitle("100m WR Over Time") +
  scale_color_economist()
  
fift_WR_plot = ggplot(fift_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + 
ggtitle("1500m WR Over Time") + scale_color_economist()
  
Tenk_WR_plot = ggplot(Tenk_WR, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + 
ggtitle("10000m WR Over Time") + scale_color_economist()
  
hundred_AT_plot_plus = ggplot(hundred_ATplus, aes(x = year, y = digitalTime, color = notes)) + geom_point() + labs(x = 'Year', y = 'Time(s)') + 
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1) + scale_color_economist() + ggtitle("100m All-Time Performances (Inc. Annulled)")
  
hundred_AT_plot = ggplot(hundred_AT, aes(x = year, y = digitalTime)) + geom_point() + labs(x = 'Year', y = 'Time(s)') + ggtitle("100m All-Time Performances") +
geom_errorbar(aes(x = year, ymin=digitalTime - error, ymax=digitalTime + error), colour="red", width=.1)  + scale_color_economist()

fift_AT_plot = ggplot(fift_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + scale_color_economist() + ggtitle("1500m All-Time Performances")
  
Tenk_AT_plot = ggplot(Tenk_AT, aes(year, digitalTime, label = time)) + geom_point() + labs(x = 'Year', y = 'Time(m)') + scale_color_economist() + ggtitle("10000m All-Time Performances")
  
hundred_Boxplot = ggplot(hundred_AT, aes(x = year, y = digitalTime)) + geom_boxplot(fill = 'red', colour = 'blue', alpha = 0.7, notch = T,
                                                                                    outlier.colour = "#1F3552", outlier.shape = 20) + labs(x = 'Year', y = 'Time(s)')  + 
  scale_color_economist() + ggtitle("100m All-Time Performances") 
  
fift_Boxplot = ggplot(fift_AT, aes(year, digitalTime, label = time)) + geom_boxplot(fill = 'red', colour = 'blue', alpha = 0.7,
                                                                                    outlier.colour = "#1F3552", outlier.shape = 20) + labs(x = 'Year', y = 'Time(m)') + 
  scale_color_economist() + ggtitle("1500m All-Time Performances")
  
Tenk_Boxplot = ggplot(Tenk_AT, aes(year, digitalTime, label = time)) + geom_boxplot(fill = 'red', colour = 'blue', alpha = 0.7,
                                                                                    outlier.colour = "#1F3552", outlier.shape = 20) + labs(x = 'Year', y = 'Time(m)') + scale_color_economist() + ggtitle("10000m All-Time Performances")
  
  hundred_density = ggplot(hundred_ATplus, aes(x = digitalTime)) + geom_density(aes(group = notes, colour = notes, fill = notes), alpha = 0.3) + 
    labs(x = "Time (s)", y = "Frequency", title = "World's All-Time 100m Performance List") + 
    annotate('text', x = 9.7, y = 9, label = sprintf("The result of the \n Wilcoxon test gives \n a p-value of %0.3f", wilcox100[3])) + 
    annotate('text', x = 9.7, y = 6, label = sprintf("The median of the doped \n samples  is %2.3f, and \n the standard deviation is %0.3f.", dope100[1],dope100[2])) + 
    annotate('text', x = 9.7, y = 3, label = sprintf("The median of the randomly \n selected samples is %2.3f, \n and the standard deviation \n is %0.3f.", random100[1],random100[2]))
  
  fift_density = ggplot(fift_AT, aes(x = digitalTime, label = time)) + geom_density(aes(fill="red"), alpha = 0.3) + 
    labs(x = "Time (s)", y = "Frequency", title = "World's All-Time 1500m Performance List")
  
  fift_sub332_density = ggplot(fift_elite, aes(digitalTime, label = time)) + geom_density(aes(group = notes, colour = notes, fill = notes), alpha = 0.3) + 
    annotate('text', x = 3.462, y = 35, label = sprintf("The result of a \n Wilcoxon Sum Rank performed \n on the 'epo' and 'post' \n groups had a p-value of %0.3f", wilcox1500[3])) + 
    annotate('text', x = 3.462, y = 25, label = sprintf("The result of a \n Kruskal-Willas Sum Rank test \n had a p-value of %0.3f", kruskal1500[3])) +
    labs(x = "Time (m)", y = "Frequency", title = "Distribution of sub-3:32 1500m times")
  fift_sub332_density = ggplotly(fift_sub332_density) 
  fift_sub332_density
  
  Tenk_density = ggplot(Tenk_AT, aes(x = digitalTime, label = time)) + geom_density(aes(fill="red"), alpha = 0.3) + 
    labs(x = "Time (m)", y = "Frequency", title = "World's All-Time 10000m Performance List")
  
  Top30TenkPlot = ggplot(Top30Tenk, aes(year,sdTime, label = time)) + geom_point() + geom_smooth(method = "lm", se = F) +
    labs(x = 'Year', y = 'sd(Time (s))') + ggtitle("10000m Standard Deviation Over Time")
  
  Tenk_density_ERA = ggplot(Tenk_Analysis, aes(digitalTime, label = time)) + geom_density(aes(group = notes, colour = notes, fill = notes), alpha = 0.3) + 
    labs(x = "Time (m)", y = "Frequency", title = "World's All-Time 10000m Grouped by Era") + 
    annotate('text', x = 27.0, y = 2.1, label = sprintf("The result of the Kruskal-Wallis Rank Sum Test \n yields a p-value of %0.3f.", kruskal10k[3]))
                                                              
  Tenk_density_ERA <- ggplotly(Tenk_density_ERA)
  
  sub27_10kPlot = ggplot(sub27_10k, aes(digitalTime, label = time)) + geom_density(aes(group = notes, colour = notes, fill = notes), alpha = 0.3) + 
    annotate('text', x = 26.5, y = 4.3, label = sprintf("The result of the \n Wilcoxon Sum Rank test gives \n a p-value of %0.3f.", sub27_test[3])) + 
    annotate('text', x = 26.5, y = 3, label = sprintf("The median of the 'epo-' samples \n is %2.3f while the standard deviation, \n  skewness and kurtosis are:\n %0.3f, %0.3f, and %0.3f.", elite10k[1,2], elite10k[1,3],elite10k[1,4], elite10k[1,5])) + 
    annotate('text', x = 26.5, y = 1.7, label = sprintf("The median of the 'post-' samples \n is %2.3f while the standard deviation, \n  skewness and kurtosis are: \n%0.3f, %0.3f, and %0.3f.", elite10k[2,2], elite10k[2,3],elite10k[2,4], elite10k[2,5])) +
    labs(x = "Time (m)", y = "Frequency", title = "Distribution of sub-27 min 10,000 m times")
  sub27_10kPlot <- ggplotly(sub27_10kPlot)

  
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
    
    output$plot4 <- renderPlotly({
      
      Top30TenkPlot
      
    })
    output$plot5 <- renderPlotly({
      
      Tenk_density_ERA
      
    })
    output$plot6 <- renderPlotly({
      
      sub27_10kPlot
      
    })
    output$plot7 <- renderPlotly({
      
      fift_sub332_density
      
    })
  })
  
}

