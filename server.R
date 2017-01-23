library(shiny)
source("helpers.R")

shinyServer(function(input, output) {
  
  id <- reactive({
    
    validate(
      need(input$hr_id != "", 'Choose at least one ID, or multiple IDs from the same shift.')
      )
    input$hr_id
    
  })

     output$heartrate <- renderPlot({
       
       if (input$min_hr != "" & input$max_hr != "") {
         hr_plot_mult(ids = id(), fb_hr_breaks = input$hr_avg, 
                      h_hr_breaks = input$hr_avg, date_break = input$hr_date,
                      fb = input$fb, h = input$hex, min = input$min_hr, 
                      max = input$max_hr)
       } else {
         hr_plot_mult(ids = id(), fb_hr_breaks = input$hr_avg, 
                      h_hr_breaks = input$hr_avg, date_break = input$hr_date,
                      fb = input$fb, h = input$hex, min = NULL, 
                      max = NULL)
       }
       
       
   })

 
  id_br <- reactive({
    
    validate(
      need(input$br_id != "", 'Choose at least one ID, or multiple IDs from the same shift.')
    )
    input$br_id
    
  })
  
  output$breathing <- renderPlot({
    
    if(input$min_br != "" & input$max_br != "") {
      
      br_plot_mult(ids = id_br(), br_breaks = input$br_avg, date_break = input$br_date, 
                   min = input$min_br, max = input$max_br)
    } else {
      br_plot_mult(ids = id_br(), br_breaks = input$br_avg, date_break = input$br_date, 
                   min = NULL, max = NULL)
    }
    
    
    
  })
  
  id_st <- reactive({
    
    validate(
      need(input$steps_id != "", '')
    )
    input$steps_id
  })
  
  output$steps <- renderPlot({
    
    stepcount_plot(id_st())
    
  })
  
})
  
 


