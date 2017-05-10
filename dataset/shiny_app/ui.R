library(shiny)
library(ggplot2)
shinyUI(fluidPage(
  titlePanel("Restaurant data summary"),
  sidebarPanel(
    radioButtons("radio", label = "Choices", choices = list("6d0ebab3-edf8-4e04-a947-1973e76ab11f" = "res1", 
                                                            "535b23c0-728f-4ced-8ad6-c8ecd8ae379d" = "res2",
                                                            "95a94f0c-638d-4f45-9483-353d889e046c" = "res3", 
                                                            "74381003-bd11-4b50-86fc-a27f4ddf1a4e" = "res4",
                                                            "3bf185da-5b44-46a4-b6ec-343ded8e01e9" = "res5"),
                 selected = ),
    dateRangeInput("dates", label = h3("Date range"),start = "2016-03-29",end = "2016-07-01"),
    verbatimTextOutput("value")
  ),
  
  
  mainPanel(plotOutput("distPlot"),
            plotOutput("seriesPlot"))
))
