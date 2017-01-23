library(shiny)
library(dygraphs)
source("helpers.R")

shinyServer(function(input, output) {
  
  id <- reactive({
    
    validate(
      need(input$hr_id != "", 'Choose at least one ID.')
      )
    input$hr_id
    
  })
  
  output$heartrate <- renderPlot({

      hr_plot_mult(ids = id(), fb_hr_breaks = input$hr_avg, 
                        h_hr_breaks = input$hr_avg, date_break = input$hr_date,
                        fb = input$fb, h = input$hex)
    
  })
  
  output$breathing <- renderPlot({
    
    br_plot(input$br_id, br_breaks = input$br_avg, date_break = input$br_date)
    
  })
  
  output$steps <- renderPlot({
    
    stepcount_plot(input$st_id)
    
  })
  
})
  
 


