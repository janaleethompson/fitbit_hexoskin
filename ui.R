library(shiny)

shinyUI(fluidPage(
  
navbarPage(
    title = 'Fitbit and Hexoskin Plots', 
    
    tabPanel('Heart Rate', 
             
      fluidRow(column(3, selectizeInput('hr_id', 'Participant ID', choices = list(
        Nov1am = c('204n', '208n', '207n', '203n', '202n', '206n', '205n', '200n'), 
        Nov8am = c('303n', '306n', '302n', '305n', '300n', '301n', '304n'), 
        Nov9am = c('402n', '403n', '405n', '400n', '401n', '700n'), 
        Nov9pm = c('500n', ""), 
        Nov10pm = c('501n', ""),
        Nov12am = c('504n', ""),
        Nov14am = c('602n', "505n", '600n', '601n'),
        Nov14pm = c('307n', '308n'), 
        Nov15am = c('707n', '706n', '705n'), 
        Nov15pm = c('701n', ''), 
        Nov16am = c('710n', ""),
        Nov17am = c('309n', '703n', '702n', '708n'),
        Nov18am = c('406n', '704n', '709n'), 
        Nov18pm = c('503n', '506n'), 
        Nov19am = c('800n', '507n'), 
        Nov29am = c('208q', '209q', '201q', '200q', '207q', '202q', '203q', '210q', '205q', '204q'), 
        Nov29pm = c('214q', ""), 
        Nov30am = c('213q', '211q', '212q'), 
        Nov30pm = c('215q', ""), 
        Dec5am = c('902n', '910n', '901n', '900n'), 
        Dec6am = c('904n', '905n', '906n', '911n'), 
        Dec7am = c('908n', '909n', '912n'), 
      #  Jan17am = c('101w', ''), fitbit data here looks messed up.
        Jan19am = c('104w', '107w', '105w', '109w', '100w', '108w', '103w', '106w', '110w'), 
        Jan27am = c('114w', '115w', '120w', '121w', '113w', '118w', '116w', '117w', '119w', '111w', '112w'), 
        Feb9am = c('913n', ''), 
        Feb10am = c('122w', '123w')),
            multiple = TRUE, options = list(maxItems = 11, placeholder = 'select one or multiple IDs'))),
            
      column(3, selectInput('hr_date', 'Date Breaks', c('30 min', '60 min', '2 hours'))), 
      
      column(3, selectInput('hr_avg', 'Aggregation', c('15 sec', '30 sec', '45 sec', 
                                                       '1 min', '2 min', '3 min', '4 min',
                                                       '5 min', '10 min'), 
                            selected = '1 min')),
      
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
                                                   Nov9am = c('402n', '403n', '405n', '400n', '401n', '700n'), 
                                                   Nov9pm = c('500n', ""), 
                                                   Nov10pm = c('501n', ""),
                                                   Nov12am = c('504n', ""),
                                                   Nov14am = c('602n', "505n", '600n', '601n'),
                                                   Nov14pm = c('307n', '308n'), 
                                                   Nov15am = c('707n', '706n', '705n'), 
                                                   Nov15pm = c('701n', ''), 
                                                   Nov16am = c('710n', ""),
                                                   Nov17am = c('309n', '703n', '702n', '708n'),
                                                   Nov18am = c('406n', '704n', '709n'), 
                                                   Nov18pm = c('503n', '506n'), 
                                                   Nov19am = c('800n', '507n'), 
                                                   Nov29am = c('208q', '209q', '201q', '200q', '207q', '202q', '203q', '210q', '205q', '204q'), 
                                                   Nov29pm = c('214q', ""), 
                                                   Nov30am = c('213q', '211q', '212q'), 
                                                   Nov30pm = c('215q', ""), 
                                                   Dec5am = c('902n', '910n', '901n', '900n'), 
                                                   Dec6am = c('904n', '905n', '906n', '911n'), 
                                                   Dec7am = c('908n', '909n', '912n'), 
                                                   #  Jan17am = c('101w', ''), fitbit data here looks messed up.
                                                   Jan19am = c('104w', '107w', '105w', '109w', '100w', '108w', '103w', '106w', '110w'), 
                                                   Jan27am = c('114w', '115w', '120w', '121w', '113w', '118w', '116w', '117w', '119w', '111w', '112w'), 
                                                   Feb9am = c('913n', ''), 
                                                   Feb10am = c('122w', '123w')),
                                               multiple = TRUE, options = list(maxItems = 11, placeholder = 'select one or multiple IDs'))),                          
                      
                      
                
                      column(3, selectInput('br_date', 'Date Breaks', c('30 min', '60 min', '2 hours'))), 
                      
                      column(3, selectInput('br_avg', 'Aggregation', c('15 sec', '30 sec', '45 sec', 
                                                                       '1 min', '2 min', '3 min', '4 min',
                                                                       '5 min', '10 min'), 
                                            selected = '1 min'))), 
             
             plotOutput('breathing'), 
             
             br(), 
             br(), 
             
             column(2, textInput('min_br', "Min time", "")), 
             column(2, textInput('max_br', "Max time", "")), 
             
             br(), 
             
             p("Enter minimum and maximum times to zoom in the format hour:minute am/pm (e.g., 12:30 pm). 
              If either field is blank, the plot will reset to its original range.")), 
    
      tabPanel('Steps', 
               
               fluidRow(column(3, selectizeInput('steps_id', 'Participant ID', choices = list(
                 Nov1am = c('204n', '208n', '207n', '203n', '202n', '206n', '205n', '200n'), 
                 Nov8am = c('303n', '306n', '302n', '305n', '300n', '301n', '304n'), 
                 Nov9am = c('402n', '403n', '405n', '400n', '401n', '700n'), 
                 Nov9pm = c('500n', ""), 
                 Nov10pm = c('501n', ""),
                 Nov12am = c('504n', ""),
                 Nov14am = c('602n', "505n", '600n', '601n'),
                 Nov14pm = c('307n', '308n'), 
                 Nov15am = c('707n', '706n', '705n'), 
                 Nov15pm = c('701n', ''), 
                 Nov16am = c('710n', ""),
                 Nov17am = c('309n', '703n', '702n', '708n'),
                 Nov18am = c('406n', '704n', '709n'), 
                 Nov18pm = c('503n', '506n'), 
                 Nov19am = c('800n', '507n'), 
                 Nov29am = c('208q', '209q', '201q', '200q', '207q', '202q', '203q', '210q', '205q', '204q'), 
                 Nov29pm = c('214q', ""), 
                 Nov30am = c('213q', '211q', '212q'), 
                 Nov30pm = c('215q', ""), 
                 Dec5am = c('902n', '910n', '901n', '900n'), 
                 Dec6am = c('904n', '905n', '906n', '911n'), 
                 Dec7am = c('908n', '909n', '912n'), 
                 #  Jan17am = c('101w', ''), fitbit data here looks messed up.
                 Jan19am = c('104w', '107w', '105w', '109w', '100w', '108w', '103w', '106w', '110w'), 
                 Jan27am = c('114w', '115w', '120w', '121w', '113w', '118w', '116w', '117w', '119w', '111w', '112w'), 
                 Feb9am = c('913n', ''), 
                 Feb10am = c('122w', '123w')),
                 multiple = TRUE, options = list(maxItems = 11, placeholder = 'select one or multiple IDs'))),               
               
               plotOutput('steps')
               
               )
    
  
  )

)))
