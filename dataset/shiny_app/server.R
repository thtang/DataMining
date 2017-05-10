library(ggplot2)
library(dplyr)
shinyServer(function(input, output) {
  
  # You can access the values of the widget (as a vector of Dates)
  # with input$dates, e.g.
  output$value <- renderPrint({ input$dates})
  output$distPlot <- renderPlot({
    MyDatesTable <- readRDS(paste0(input$radio,".rds"))
    
    
    freq_non_zero <- subset(MyDatesTable,MyDatesTable$Freq != 0)
    ggplot(data=freq_non_zero , aes(as.factor(time),Freq,fill = as.factor(time))) +
      geom_boxplot()+
      guides(fill=FALSE)
  })
  output$seriesPlot <- renderPlot({
    MyDatesTable <- readRDS(paste0(input$radio,".rds"))
    CutDatesTable <-subset(MyDatesTable,MyDatesTable$Var1<=as.POSIXct(input$dates[2]) &
                             MyDatesTable$Var1>=as.POSIXct(input$dates[1]))
    ggplot(CutDatesTable, aes(as.POSIXct(Var1), Freq)) + geom_line() +
      xlab("") + ylab("Daily Views")
  })
})
