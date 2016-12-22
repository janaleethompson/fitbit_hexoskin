library(shiny)
library(dygraphs)
source("helpers.R")

shinyServer(function(input, output) {
  
  output$heartrate <- renderPlot({

   hr_plot(input$id, fb_hr_breaks = input$avg, h_hr_breaks = input$avg, 
           date_break = input$date)
    
  })
  
  output$steps <- renderPlot({
    
    stepcount_plot(input$id)
    
  })
  
  output$breathing <- renderPlot({
    
    br_plot(input$id)
    
  })

})


