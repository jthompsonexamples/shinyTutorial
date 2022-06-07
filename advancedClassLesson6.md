Lesson 6
--------

Our final iteration (feel free to do your own modifications though).
Here we modify the display so that the panel is not visible when the
page loads, it only appears when the user clicks a location. And we give
the user a button to close the panel. We also change the library used to
create the plots to introduce the functionality of `plotly`.

### Setup

Add the `plotly` and `leafem` libraries.

    require(leaflet)
    require(shiny)
    require(tidyverse)
    require(httr)
    require(jsonlite)
    require(lubridate)
    require(plotly)
    require(geosphere)
    require(leafem)
    source('./weatherFunctions v2.R')
    stationTib <- getWeatherGeo()

### UI

The UI looks much smaller and simpler now. The absolute panel is gone,
now we only have the tab definitions and output slots. We add a tag
(HTML code placeholder) for where we’ll put the panel later.

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

### Server

The server is now very complicated. As it should be, its the brain after
all. When you look closely, however, we just moved the absolute panel
information down into the server. We use `insertUI` to create the
absolute panel and place it into the placeholder tag we created in the
UI. Notice the `id` of the panel, we use that as a reference to also
`removeUI` on button click or when the user clicks a new location
without removing the panel first.

We also change the plot render function from `renderPlot` to
`renderPlotly`. The `plotly` functions use javascript and have many nice
features we can take advantage of like mouse-over text, in panel zoom,
and a print feature. Just for fun, we add `addMouseCoordinates` to the
`leaflet` map to give the coordinates of the current mouse position in
the upper left corner. This requires the `leafem` package.

    server <- function(input, output, session){
      # generate the initial plot with all the stations
      output$mymap <- renderLeaflet({
        leaflet(data=stationTib) %>% 
        # addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        #          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
        addTiles() %>% 
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
          leafem::addMouseCoordinates() %>% 
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
            absolutePanel(
              id = 'plots', class = 'panel panel-default',
              fixed = TRUE, draggable = TRUE, top = 60,
              left = 'auto', right = 20, bottom = 'auto',
              width = 280, height = 'auto', style = 'opacity: 0.8',
              h3('Quick Plots'),
              plotlyOutput('forecastPlot', height = 200),
              plotlyOutput('observationPlot', height = 200),
              fluidRow(
                column(
                  12, align='center',
                  sliderInput(
                    'dateSlider',
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
      observeEvent(
        input$redrawPlot, {
        # redraw button clicked
        # does not rerun the data pull function, uses the reactive object
          newPlot <- plotRecentObservations(
            obPlot$p$obs,
            obPlot$p$station,
            input$dateSlider[1],
            input$dateSlider[2])
          output$observationPlot <- renderPlotly({newPlot})
        })
      # remove the panel if the user clicks
      observeEvent(
        input$closePanel, 
        removeUI(
          selector = '#subPanel'
          )
        )
    }

### Apply the changes

    shinyApp(ui, server, options=list(height=600))

When the page loads you’ll see just the map with two tabs on top. The
charts appear inside the panel after a click on the map as they did
before. Mouse-over the charts, notice the difference? The display area
for these charts is a little small, but you see their capabilities.

Summary
-------

We created a shiny app from scratch, going from a very basic app to a
very complicated app by the end. This method of slowly adding
capabilities is a great way to build your skills and get comfortable
with the program. Keys to success:

-   Do your analysis first, then make the app.
-   Keep analysis code in outside functions and scripts and use shiny to
    access them.
-   Have a well-thought-out design first, then start creating the app.
-   Debug, debug, debug, then deploy.
