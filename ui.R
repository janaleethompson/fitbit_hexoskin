library(shiny)

shinyUI(fluidPage(
  
navbarPage(
    title = 'Fitbit and Hexoskin Plots', 
    
    tabPanel('Heart Rate', 
             
      fluidRow(column(3, selectizeInput('hr_id', 'Participant ID', choices = list(
        Nov1am = c('204n', '208n', '207n', '203n', '202n', '206n', '205n', '200n'), 
        Nov8am = c('303n', '306n', '302n', '305n', '300n', '301n', '304n'), 
        Nov10pm = c('501n', '501n', '500n', '402n', '403n', '405n', '404n', '400n', '401n', '700n'), 
        Nov12pm = c('504n', ""), 
        Nov14pm = c('307n', '308n', '505n', '602n', '600n', '601'), 
        Nov15pm = c('701n', ""), 
        Nov15am = c('710n', '707n', '706n', '705n'), 
        Nov18am = c('406n', '704n', '709n', '702n', '708n', '309n', '703n', '710n'), 
        Nov18pm = c('503n', '506n'), 
        Nov19am = c('800n', '507n'), 
        Nov29am = c('208q', '209q', '201q', '200q', '207q', '202q', '203q', '210q', '205q', '204q'), 
        Nov29pm = c('214q', ""), 
        Nov30am = c('213q', '211q', '212q'), 
        Nov30pm = c('215q', ""), 
        Dec5am = c('902n', '910n', '901n', '900n'), 
        Dec6am = c('904n', '905n', '906n', '911n'), 
        Dec7am = c('908n', '909n', '912n')),
            multiple = TRUE, options = list(maxItems = 10, placeholder = 'select one or multiple IDs'))),
            
      column(3, selectInput('hr_date', 'Date Breaks', c('30 min', '60 min', '2 hours'))), 
      
      column(3, selectInput('hr_avg', 'Aggregation', c('30 sec', '1 min', '5 min', '10 min'))),
      
      column(3, tags$h5('Display Devices'), 
             checkboxInput('fb', 'Fitbit', value = TRUE), 
             checkboxInput('hex', 'Hexoskin', value = TRUE))), 
      
      plotOutput('heartrate'), 
      
      br(), 
      br(), 
      
      column(2, textInput('min_hr', "Min time", "")), 
      column(2, textInput('max_hr', "Max time", "")), 
      
      br(),
      
      p("Enter minimum and maximum times to zoom in the format hour:minute am/pm (e.g., 12:30 pm). 
        If either field is blank, the plot will reset to its original 
      range.")),
      
    
    tabPanel('Breathing Rate', 

             fluidRow(column(3, selectizeInput('br_id', 'Participant ID', choices =
                                                 list(
                                                   Nov1am = c('204n', '208n', '207n', '203n', '202n', '206n', '205n', '200n'), 
                                                   Nov8am = c('303n', '306n', '302n', '305n', '300n', '301n', '304n'), 
                                                   Nov10pm = c('501n', '501n', '500n', '402n', '403n', '405n', '404n', '400n', '401n', '700n'), 
                                                   Nov12pm = c('504n'), 
                                                   Nov14pm = c('307n', '308n', '505n', '602n', '600n', '601'), 
                                                   Nov15pm = c('701n'), 
                                                   Nov15am = c('710n', '707n', '706n', '705n'), 
                                                   Nov18am = c('406n', '704n', '709n', '702n', '708n', '309n', '703n', '710n'), 
                                                   Nov18pm = c('503n', '506n'), 
                                                   Nov19am = c('800n', '507n'), 
                                                   Nov29am = c('208q', '209q', '201q', '200q', '207q', '202q', '203q', '210q', '205q', '204q'), 
                                                   Nov29pm = c('214q'), 
                                                   Nov30am = c('213q', '211q', '212q'), 
                                                   Nov30pm = c('215q'), 
                                                   Dec5am = c('902n', '910n', '901n', '900n'), 
                                                   Dec6am = c('904n', '905n', '906n', '911n'), 
                                                   Dec7am = c('908n', '909n', '912n')),
                                               multiple = TRUE, options = list(maxItems = 10, placeholder = 'select one or multiple IDs'))),                          
                      
                      
                
                      column(3, selectInput('br_date', 'Date Breaks', c('30 min', '60 min', '2 hours'))), 
                      
                      column(3, selectInput('br_avg', 'Aggregation', c('30 sec', '1 min', '5 min', '10 min')))), 
             
             plotOutput('breathing'), 
             
             br(), 
             br(), 
             
             column(2, textInput('min_br', "Min time", "")), 
             column(2, textInput('max_br', "Max time", "")), 
             
             br(), 
             
             p("Enter minimum and maximum times to zoom in the format hour:minute am/pm (e.g., 12:30 pm). 
              If either field is blank, the plot will reset to its original range.")), 
    
      tabPanel('Steps', 
               
               fluidRow(column(3, selectizeInput('steps_id', 'Participant ID',
                                                   choices =
                                                   list(
                                                     Nov1am = c('204n', '208n', '207n', '203n', '202n', '206n', '205n', '200n'), 
                                                     Nov8am = c('303n', '306n', '302n', '305n', '300n', '301n', '304n'), 
                                                     Nov10pm = c('501n', '501n', '500n', '402n', '403n', '405n', '404n', '400n', '401n', '700n'), 
                                                     Nov12pm = c('504n'), 
                                                     Nov14pm = c('307n', '308n', '505n', '602n', '600n', '601'), 
                                                     Nov15pm = c('701n'), 
                                                     Nov15am = c('710n', '707n', '706n', '705n'), 
                                                     Nov18am = c('406n', '704n', '709n', '702n', '708n', '309n', '703n', '710n'), 
                                                     Nov18pm = c('503n', '506n'), 
                                                     Nov19am = c('800n', '507n'), 
                                                     Nov29am = c('208q', '209q', '201q', '200q', '207q', '202q', '203q', '210q', '205q', '204q'), 
                                                     Nov29pm = c('214q'), 
                                                     Nov30am = c('213q', '211q', '212q'), 
                                                     Nov30pm = c('215q'), 
                                                     Dec5am = c('902n', '910n', '901n', '900n'), 
                                                     Dec6am = c('904n', '905n', '906n', '911n'), 
                                                     Dec7am = c('908n', '909n', '912n')),
                                                 multiple = TRUE, options = list(maxItems = 1, placeholder = 'select an ID'))),               
               
               plotOutput('steps')
               
               )
    
  
  )

)))
