library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Fitbit and Hexoskin Plots"),
  
  sidebarLayout(
    
    fixedRow(
      column(6,
             sidebarPanel(
      selectInput('id',
                  'Participant ID', 
                  c("300n", "308n", "501n", "601n", "203q"))
    ))),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Heart Rate", plotOutput("heartrate")), 
        tabPanel("Step Counts", plotOutput("steps")), 
        tabPanel("Breathing Rate", plotOutput("breathing"))
      )
    )
  )
))