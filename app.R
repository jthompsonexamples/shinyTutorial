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

ui <- navbarPage(title='Shiny Tutorial',
  tabPanel(
    'Lesson 1',
    leafletOutput('mymap1'),
  ),
  tabPanel(
    'Lesson 2',
    leafletOutput('mymap2', width=600, height=600),
    textOutput('latlon2')
  ),
  tabPanel(
    'Lesson 3',
    leafletOutput('mymap3', width=600, height=600),
    plotOutput('forecastPlot3')
  ),
  tabPanel(
    'Lesson 4',
    tags$style(type = 'text/css', '#mymap4 {height: calc(100vh - 80px) !important;cursor: crosshair}'),
    leafletOutput('mymap4', width='100%', height='100%'),
    absolutePanel(id = "controls4", class = "panel panel-default", fixed = TRUE,
                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                  width = 280, height = "auto", style = 'opacity: 0.8',
                  
                  h3('Quick Plots'),
                  
                  plotOutput("forecastPlot4", height = 200),
                  plotOutput("observationPlot4", height = 200)
    )
  ),
  tabPanel(
    'Lesson 5',
    tabsetPanel(
      tabPanel(
        'Maps',
        fillPage(
          tags$style(type = "text/css", "#mymap5 {height: calc(100vh - 80px) !important;cursor: crosshair}"),
          leafletOutput('mymap5', width='100%', height='100%')),
        absolutePanel(
          id = "controls5", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
          width = 280, height = "auto", style = 'opacity: 0.8',
          h3('Quick Plots'),
          plotOutput("forecastPlot5", height = 200),
          plotOutput("observationPlot5", height = 200),
          fluidRow(
            column(
              12, align='center',
              sliderInput(
                'dateSlider5',
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
              12, align='center',
              actionButton('redrawPlot5','Redraw')
            )
          )
        ),
      ),
      tabPanel(
        'Observation Explorer',
        tags$style(type = "text/css", "html, body {overflow-y:scroll}"),
        tableOutput('dataOut5')
      )
    )
  ),
  tabPanel(
    'Lesson 6',
    tabsetPanel(
      # page one
      tabPanel(
        'Maps',
        fillPage(
          tags$style(type = 'text/css', '#mymap6 {height: calc(100vh - 80px) !important;cursor: crosshair}'),
          leafletOutput('mymap6', width='100%', height='100%')),
        tags$div(id='chartUI') # placeholder for the UI
      ),
      # page two
      tabPanel(
        'Observation Explorer',
        tags$style(type = 'text/css', 'html, body {overflow-y:scroll}'),
        tableOutput('dataOut6')
      )
    )
  )
)

server <- function(input, output, session){
  #### 1
  output$mymap1 <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>% 
      setView(-98.5795, 39.8283, zoom=1)
  })
  #### 2
  output$mymap2 <- renderLeaflet({
    leaflet() %>% 
      addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
               attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
      setView(-98.5795, 39.8283, zoom=3)
  })
  observe({
    event2 <- input$mymap2_click
    if (is.null(event2)){
      return()
    }
    isolate({
      output$latlon2 <- renderText(paste0('lat=',event2$lat,', long=',event2$lng))
    })
  })
  #### 3
  output$mymap3 <- renderLeaflet({
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
      setView(-98.5795, 39.8283, zoom=3)
  })
  observe({
    event3 <- input$mymap3_click
    if (is.null(event3)){
      return()
    }
    isolate({
      m <- getWeatherForcast(event3$lat,event3$lng)
      output$forecastPlot3 <- renderPlot({m})
    })
  })
  #### 4
  output$mymap4 <- renderLeaflet({
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
      addLegend('bottomleft',
                colors='green',
                labels='Stations') %>% 
      setView(-98.5795, 39.8283, zoom=3)
  })
  observe({
    event4 <- input$mymap4_click
    if (is.null(event4)){
      return()
    }
    isolate({
      forPlot4 <- getWeatherForcast(event4$lat,event4$lng)
      output$forecastPlot4 <- renderPlot({forPlot4})
      obPlot4 <- getRecentObservations(event4$lat,event4$lng)
      output$observationPlot4 <- renderPlot({obPlot4})
    })
  })
  #### 5
  output$mymap5 <- renderLeaflet({
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
      addLegend('bottomleft',
                colors='green',
                labels='Stations') %>% 
      setView(-98.5795, 39.8283, zoom=3)
  })
  obPlot5 <- reactiveValues()
  observe({
    event5 <- input$mymap5_click
    if (is.null(event5)){
      return()
    }
    showModal(modalDialog(
      title = 'Connecting',
      'Waiting for API response'
    ))
    obPlot5$p <- getRecentObservations(event5$lat,event5$lng)
    isolate({
      forPlot5 <- getWeatherForcast(event5$lat,event5$lng)
      output$forecastPlot5 <- renderPlot({forPlot5})
      #obPlot <- getRecentObservations(event$lat,event$lng)
      output$observationPlot5 <- renderPlot({obPlot5$p$plot})
      output$dataOut5 <- renderTable(obPlot5$p$obs)
    })
  })
  observeEvent(input$redrawPlot5, {
    # redraw button clicked
    newPlot5 <- plotRecentObservations(obPlot5$p$obs, obPlot5$p$station, input$dateSlider5[1], input$dateSlider5[2])
    output$observationPlot5 <- renderPlot({newPlot5})
  })
  #### 6
  output$mymap6 <- renderLeaflet({
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
  obPlot6 <- reactiveValues()
  # when the event fires (here when the map is clicked) do things
  observe({
    event6 <- input$mymap6_click
    # if the event is empty, quit
    if (is.null(event6)){
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
        absolutePanel(
          id = 'plots', class = 'panel panel-default', fixed = TRUE,
          draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
          width = 280, height = 'auto', style = 'opacity: 0.8',
          # header information
          h3('Quick Plots'),
          # places for plots
          plotlyOutput('forecastPlot6', height = 200),
          plotlyOutput('observationPlot6', height = 200),
          # put the controls in rows to manage placements
          fluidRow(
            column(
              12, align='center',
              sliderInput(
                'dateSlider6',
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
              actionButton('redrawPlot6','Redraw')
              ),
            column(
              6, align='center',
              actionButton('closePanel6', 'Close')
              )
            )
          )
        ),
      )
    # create the reactive object for recent observations using external function
    obPlot6$p <- getRecentObservations(event6$lat,event6$lng)
    # the things in isolate are not reactive, do not change
    isolate({
      # get the forcast and observations and plot
      forPlot6 <- getWeatherForcast(event6$lat,event6$lng)
      output$forecastPlot6 <- renderPlotly({forPlot6})
      output$observationPlot6 <- renderPlotly({obPlot6$p$plot})
      output$dataOut6 <- renderTable(obPlot6$p$obs)
    })
    removeModal()
  })
  observeEvent(input$redrawPlot6, {
    # redraw button clicked
    # does not rerun the data pull function, uses the reactive object
    newPlot6 <- plotRecentObservations(obPlot6$p$obs, obPlot6$p$station, input$dateSlider6[1], input$dateSlider6[2])
    output$observationPlot6 <- renderPlotly({newPlot6})
  })
  # remove the panel if the user clicks
  observeEvent(
    input$closePanel6, 
    removeUI(
      selector = '#subPanel'
      )
    )
}

shinyApp(ui, server)
