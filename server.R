library(shiny)
library(dygraphs)
source("~/Documents/gs/fitbit_hexoskin/app.R")

shinyServer(function(input, output) {
  
  output$heartrate <- renderPlot({

   hr_plot(input$id)
    
  })
  
  output$steps <- renderPlot({
    
    stepcount_plot(input$id)
    
  })
  
  output$breathing <- renderPlot({
    
    br_plot(input$id)
    
  })

})


