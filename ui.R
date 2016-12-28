library(shiny)

#shinyUI(fluidPage(
  
  navbarPage(
    title = 'Fitbit and Hexoskin Plots', 
    
    tabPanel('Heart Rate', plotOutput('heartrate'), br(), br(), 
             fluidRow(
               column(3, 
                      selectInput('hr_id', 
                                  'Participant ID', 
                                  c('300n', '308n', '501n', 
                                    '601n', '203q'))), 
               column(3, selectInput('hr_date', 'Date Breaks', 
                                     c('30 min', 
                                       '60 min', 
                                       '2 hours'))), 
               column(3,
                      selectInput('hr_avg', 
                                  'Aggregation', 
                                  c('30 sec', 
                                    '1 min', 
                                    '5 min', 
                                    '10 min')) 
                      ), 
               
               column(3, tags$h5('Display Devices'), 
                      checkboxInput('fb', 'Fitbit', value = TRUE), 
                      checkboxInput('hex', 'Hexoskin', value = TRUE))
               
               
               
               
             )),
    
    tabPanel('Breathing Rate', plotOutput('breathing'), br(), br(),
             fluidRow(
               column(3, 
                      selectInput('br_id', 
                                  'Participant ID', 
                                  c('300n', '308n', '501n', 
                                    '601n', '203q'))), 
               column(3, selectInput('br_date', 'Date Breaks', 
                                     c('30 min', 
                                       '60 min', 
                                       '2 hours'))), 
               column(3,
                      selectInput('br_avg', 
                                  'Aggregation', 
                                  c('30 sec', 
                                    '1 min', 
                                    '5 min', 
                                    '10 min')) 
               )
             )),
    
    tabPanel('Steps',          plotOutput('steps'), 
             fluidRow(
               column(3, 
                      selectInput('st_id', 
                                  'Participant ID', 
                                  c('300n', '308n', '501n', 
                                    '601n', '203q')))
             ))
    
)
#))
  
