library(ggplot2)
library(dplyr)
library(xts)
library(forecast)
library(arulesViz)
shinyServer(function(input, output) {
  
  # You can access the values of the widget (as a vector of Dates)
  # with input$dates, e.g.
  output$value <- renderPrint({ input$dates})
  
  values <- reactive({readRDS(paste0(input$radio,".rds"))})
  values2 <- reactive({readRDS(paste0(input$radio2,".rds"))})
  output$table <- renderDataTable(values(),options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  output$distPlot <- renderPlot({
    #MyDatesTable <- readRDS(paste0(input$radio,".rds"))
    
    freq_non_zero <- subset(values(),values()$Freq != 0)
    ggplot(data=freq_non_zero , aes(as.factor(time),Freq,fill = as.factor(time))) +
      geom_boxplot()+
      guides(fill=FALSE)+
      ggtitle("Customer flow distribution")+
      theme(plot.title = element_text(size=20,face = "bold"))+
      xlab("Hour")+
      ylab("Customer flow")
  })
  output$seriesPlot <- renderPlot({
    #MyDatesTable <- readRDS(paste0(input$radio,".rds"))
    CutDatesTable <-subset(values(),values()$Var1<=as.POSIXct(input$dates[2]) &
                             values()$Var1>=as.POSIXct(input$dates[1]))
    ggplot(CutDatesTable, aes(as.POSIXct(Var1), Freq)) + geom_line() +
      ggtitle("Customer flow time series")+
      theme(plot.title = element_text(size=20,face = "bold"))+
      xlab("") + ylab("Customer flow")
  })
  output$forecastPlot <- renderPlot({
    #MyDatesTable <- readRDS(paste0(input$radio,".rds"))
    CutDatesTable2 <-subset(values2(),values2()$Var1<=as.POSIXct(input$training_p[2]) &
                             values2()$Var1>=as.POSIXct(input$training_p[1]))
    
    xts_obj<- xts(CutDatesTable2$Freq, order.by = as.POSIXct(CutDatesTable2$Var1))
    attr(xts_obj, 'frequency') <- 24
    #attr(xts_obj, 'start') <- as.POSIXct(input$training_p[1])
    autoplot(forecast(ets(xts_obj,model="ZZZ"),h=as.numeric(input$h)))+
      theme(plot.title = element_text(size=20,face = "bold"))
    
    #axis(1, at = as.POSIXct(CutDatesTable2$Var1), labels = as.POSIXct(CutDatesTable2$Var1), cex.axis=0.6)
    
    })
  
  
  ###### Association rules ######
  AS_frame <- reactive({
    transac <- readRDS(paste0(input$radio3,".rds")) %>% select(invoice_uuid, item_name)
    transac$invoice_uuid <- as.factor(transac$invoice_uuid)
    transac$item_name <- as.factor(transac$item_name)
    transac <- as(split(transac$item_name,transac$invoice_uuid),"transactions")
    rules <- apriori(transac, parameter=list(support=as.numeric(input$support), confidence=input$confidence))
    sorted_lift=sort(rules,by= 'lift' )
    subset.matrix <- is.subset(sorted_lift,sorted_lift)
    redundant <- colSums(subset.matrix) > 1
    rulepruned <- sorted_lift[!redundant]
    print(as.numeric(input$sortN))
    sortN <- head(sort(rulepruned,by ="lift"),as.numeric(input$sortN))
    }
  )


  output$consupPlot <- renderPlot({

    plot(AS_frame(), method='scatterplot')
    })
  output$rulesMatrix <- renderPlot({

    plot(AS_frame(), method = 'grouped')

  })
  output$graph <- renderPlot(({

    plot(AS_frame(), method='graph', control = list(type='items'))

  }))
  
})
