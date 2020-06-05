require(leaflet)
require(shiny)
require(tidyverse)
require(httr)
require(jsonlite)
require(lubridate)
require(plotly)
require(geosphere)


source('./weatherFunctions.R')

stationTib <- getWeatherGeo()

ui <- navbarPage(title='Weather Map',
  # page one
  tabPanel('Maps',
    fillPage(
      tags$style(type = 'text/css', '#mymap {height: calc(100vh - 80px) !important;cursor: crosshair}'),
      leafletOutput('mymap', width='100%', height='100%')),
  tags$div(id='chartUI') # placeholder for the UI
  ),
  # page two
  tabPanel('Observation Explorer',
           tags$style(type = 'text/css', 'html, body {overflow-y:scroll}'),
           tableOutput('dataOut')
  )
)

server <- function(input, output, session){
  # generate the initial plot with all the stations
  output$mymap <- renderLeaflet({
    leaflet(data=stationTib) %>% 
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      addCircleMarkers(lng = ~ lon,
                       lat = ~ lat,
                       label = ~paste0('station:', stationIdentifier),
                       #label = ~(hoverText),
                       labelOptions = labelOptions(textOnly = FALSE),
                       radius = 3,
                       color = 'green',
                       stroke=FALSE,
                       fillOpacity = 0.7) %>%
      leafem::addMouseCoordinates() %>% 
      addLegend('bottomleft',
                colors='green',
                labels='Stations') %>% 
      setView(-98.5795, 39.8283, zoom=3)
  })
  # placeholder for the reactive object created later
  # this is one method to use objects created inside of an observe function outside of the function
  obPlot <- reactiveValues()
  # when the event fires (here when the map is clicked) do things
  observe({
    event <- input$mymap_click
    # if the event is empty, quit
    if (is.null(event)){
      return()
    }
    # remove the UI if its present
    removeUI(selector = '#subPanel')
    # popup message
    showModal(modalDialog(
      title = 'Connecting',
      'Waiting for API response'
    ))
    # create the panel for the charts when the map is clicked
    insertUI(
      selector = '#chartUI', # send it to the placeholder tags 
      # standard inputs for the panel
      ui = tags$div(
        id='subPanel',
        absolutePanel(id = 'plots', class = 'panel panel-default', fixed = TRUE,
                         draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
                         width = 280, height = 'auto', style = 'opacity: 0.8',
                         # header information
                         h3('Quick Plots'),
                         # places for plots
                         plotlyOutput('forecastPlot', height = 200),
                         plotlyOutput('observationPlot', height = 200),
                         # put the controls in rows to manage placements
                         fluidRow(
                           column(
                             12, align='center',
                             sliderInput('dateSlider',
                                         'Plot Range',
                                         min=today()-30,
                                         max=today(),
                                         value=c(today()-14, today()),
                                         step = 1,
                                         timeFormat = "%Y-%m-%d",
                                         width='80%'),
                             )
                           ),
                         fluidRow(
                           column(
                             6, align='center',
                             actionButton('redrawPlot','Redraw')
                             ),
                           column(
                             6, align='center',
                             actionButton('closePanel', 'Close')
                             )
                           )
        )
      ),
    )
    # create the reactive object for recent observations using external function
    obPlot$p <- getRecentObservations(event$lat,event$lng)
    # the things in isolate are not reactive, do not change
    isolate({
      # get the forcast and observations and plot
      forPlot <- getWeatherForcast(event$lat,event$lng)
      output$forecastPlot <- renderPlotly({forPlot})
      output$observationPlot <- renderPlotly({obPlot$p$plot})
      output$dataOut <- renderTable(obPlot$p$obs)
    })
    removeModal()
  })
  observeEvent(input$redrawPlot, {
    # redraw button clicked
    # does not rerun the data pull function, uses the reactive object
      newPlot <- plotRecentObservations(obPlot$p$obs, obPlot$p$station, input$dateSlider[1], input$dateSlider[2])
      output$observationPlot <- renderPlotly({newPlot})
    })
  # remove the panel if the user clicks
  observeEvent(input$closePanel, 
               removeUI(
                 selector = '#subPanel'
                 )
               )
}

shinyApp(ui, server)
