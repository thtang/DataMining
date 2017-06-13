library(shiny)
library(ggplot2)
library(shinythemes)
shinyUI(fluidPage(theme = shinytheme("united"),
  titlePanel("Saley-Take care of your stuff "),
  tabsetPanel(
    
    tabPanel("Raw Data",
             br(),
             column(12,
                  selectInput("radio4", label = "Restaurant choices", choices = list("6d0ebab3-edf8-4e04-a947-1973e76ab11f" = "tran1", 
                                                                                  "535b23c0-728f-4ced-8ad6-c8ecd8ae379d" = "tran2",
                                                                                  "95a94f0c-638d-4f45-9483-353d889e046c" = "tran3", 
                                                                                  "74381003-bd11-4b50-86fc-a27f4ddf1a4e" = "tran4",
                                                                                  "3bf185da-5b44-46a4-b6ec-343ded8e01e9" = "tran5")
                            
               )
            ,hr(),
             
             
           
                 div(dataTableOutput("table1"), style = "font-size:100%")
               
             )),
    tabPanel("Summary",
  titlePanel("Restaurant data summary"),
  sidebarPanel(
    radioButtons("radio", label = "Restaurant choices", choices = list("6d0ebab3-edf8-4e04-a947-1973e76ab11f" = "res1", 
                                                            "535b23c0-728f-4ced-8ad6-c8ecd8ae379d" = "res2",
                                                            "95a94f0c-638d-4f45-9483-353d889e046c" = "res3", 
                                                            "74381003-bd11-4b50-86fc-a27f4ddf1a4e" = "res4",
                                                            "3bf185da-5b44-46a4-b6ec-343ded8e01e9" = "res5")
                 ),
    dateRangeInput("dates", label = h3("Date range"),start = "2016-03-29",end = "2016-07-01"),
    verbatimTextOutput("value")
  ),
  
  
  mainPanel(
    
    fluidRow(
      div(dataTableOutput("table2"), style = "font-size:100%")
    ),hr(),
    fluidRow(
      plotOutput("distPlot")),hr(),
    
    fluidRow(
      plotOutput("seriesPlot")
      )
    )
    ),
  
  tabPanel("Association Ruls",
           titlePanel("Rules for food combination"),
           sidebarPanel(
             radioButtons("radio3", label = "Restaurant choices", choices = list("6d0ebab3-edf8-4e04-a947-1973e76ab11f" = "tran1", 
                                                                                 "535b23c0-728f-4ced-8ad6-c8ecd8ae379d" = "tran2",
                                                                                 "95a94f0c-638d-4f45-9483-353d889e046c" = "tran3", 
                                                                                 "74381003-bd11-4b50-86fc-a27f4ddf1a4e" = "tran4",
                                                                                 "3bf185da-5b44-46a4-b6ec-343ded8e01e9" = "tran5")
                          ),
             textInput("support",label = "Support",value = 0.001),
             sliderInput("confidence","Confidence",min = 0,max =1,value = 0.1),
             textInput("sortN",label = "Prune the number of rules to:",value = 30)
             
           ),
           mainPanel(
             plotOutput("consupPlot"),hr(),
             plotOutput("rulesMatrix"),hr(),
             plotOutput("graph")
           )
           ),
  tabPanel("Forecasting",
           titlePanel("Customer count forecasting"),
           sidebarPanel(
             radioButtons("radio2", label = "Restaurant choices", choices = list("6d0ebab3-edf8-4e04-a947-1973e76ab11f" = "res1", 
                                                                                 "535b23c0-728f-4ced-8ad6-c8ecd8ae379d" = "res2",
                                                                                 "95a94f0c-638d-4f45-9483-353d889e046c" = "res3", 
                                                                                 "74381003-bd11-4b50-86fc-a27f4ddf1a4e" = "res4",
                                                                                 "3bf185da-5b44-46a4-b6ec-343ded8e01e9" = "res5")),
             dateRangeInput("training_p", label = h3("Date range for training"),start = "2016-03-29",end = "2016-04-5"),
             textInput("h",label = "Hours for forecasting",value = 12)
           ),
           mainPanel(
             plotOutput("forecastPlot")
             
           )),
  tabPanel("Premium Plans & Features",
           HTML('<center><a href = "https://www.shinyapps.io/admin/#/account/billing/plan/starter/change" ><img src="pricing-changeplan.png" width=1100 height = 500></center>')
           )
  
  
  )
))
