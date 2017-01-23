library(shiny)

shinyUI(fluidPage(
  
navbarPage(
    title = 'Fitbit and Hexoskin Plots', 
    
    tabPanel('Heart Rate', 
             
      fluidRow(column(3, selectizeInput('hr_id', 'Participant ID', choices = 
            c("200n", "200q", "201q", "202n", "202q", "203n", "203q", "204n", "204q", "205n", "205q",
            "206n", "207n", "207q", "208n", "208q", "209q", "210n", "210q", "211q", "212q", "213q",
            "214q", "215q", "300n", "301n", "302n", "303n", "304n", "305n", "306n", "307n", "308n",
            "309n", "400n", "401n", "402n", "403n", "404n", "405n", "406n", "500n", "501n", "502n",
            "503n", "504n", "505n", "506n", "507n", "600n", "601n", "602n", "700n", "701n", "702n",
            "703n", "704n", "705n", "706n", "707n", "708n", "709n", "710n", "800n", "900n", "901n",
            "902n", "903n", "904n", "905n", "906n", "908n", "909n", "910n", "911n", "912n"), 
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
                                                 c("200n", "200q", "201q", "202n", "202q", "203n", "203q", "204n", "204q", "205n", "205q",
                                                   "206n", "207n", "207q", "208n", "208q", "209q", "210n", "210q", "211q", "212q", "213q",
                                                   "214q", "215q", "300n", "301n", "302n", "303n", "304n", "305n", "306n", "307n", "308n",
                                                   "309n", "400n", "401n", "402n", "403n", "404n", "405n", "406n", "500n", "501n", "502n",
                                                   "503n", "504n", "505n", "506n", "507n", "600n", "601n", "602n", "700n", "701n", "702n",
                                                   "703n", "704n", "705n", "706n", "707n", "708n", "709n", "710n", "800n", "900n", "901n",
                                                   "902n", "903n", "904n", "905n", "906n", "908n", "909n", "910n", "911n", "912n"), 
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
               
               fluidRow(column(3, selectizeInput('st_id', 'Participant ID', choices = 
                                                   c("200n", "200q", "201q", "202n", "202q", "203n", "203q", "204n", "204q", "205n", "205q",
                                                     "206n", "207n", "207q", "208n", "208q", "209q", "210n", "210q", "211q", "212q", "213q",
                                                     "214q", "215q", "300n", "301n", "302n", "303n", "304n", "305n", "306n", "307n", "308n",
                                                     "309n", "400n", "401n", "402n", "403n", "404n", "405n", "406n", "500n", "501n", "502n",
                                                     "503n", "504n", "505n", "506n", "507n", "600n", "601n", "602n", "700n", "701n", "702n",
                                                     "703n", "704n", "705n", "706n", "707n", "708n", "709n", "710n", "800n", "900n", "901n",
                                                     "902n", "903n", "904n", "905n", "906n", "908n", "909n", "910n", "911n", "912n"), 
                                                 multiple = FALSE, options = list(placeholder = 'select an ID'))),               
               
               plotOutput('steps')
               
               )
    
  
  )

)))
