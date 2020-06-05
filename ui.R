require(tidyverse)
#setwd('./Q Course/classFolders/Shiny - Day 7')
#itsm <- read_csv('ITSM.csv')
source('exercisePlot.R')

shinyUI(
  fluidPage(
    
    titlePanel('NEC Ticket Performance'),
    
    sidebarLayout(
      
      sidebarPanel(
        # file input
        fileInput('inputFile', 'Load your data'),
        # date range inputs
        dateRangeInput(
          'dateRange',
          'Date Range',
          start='2018-01-01',
          end='2019-01-01',
          min='2016-01-04',
          max='2019-07-12'),
        # list of NECs, use unique function to create choices
        selectInput(
          'necSelect',
          #'Which NECs?',
          'Need a file first!',
          choices = c('None','Still none'),
          multiple=TRUE),
        # organize the checkboxes
        fluidRow(
          column(6,
                 checkboxInput('countCheck', 'Display ticket counts', value=TRUE)),
          column(6,
                 checkboxInput('priorityCheck', 'Display by priority'))
        ),
        # button to run the code
        actionButton('plotAction', 'Plot it')
      ),
      # output the objects of interest
      mainPanel(
        plotOutput('boxPlot'),
        p(),
        tags$hr(style="border-color: purple;"),
        tableOutput('dataTable')
      )
    )
  )
)