Lesson 4
--------

Next we’ll add some additional features and change the display to make
it more eye-catching. We’ll add a chart to get the historical
observations of the closest weather station and add an opaque panel on
the right side of the page where we’ll place our charts.

### Setup

The only change to the setup is the addition of the `geosphere` package
to help us find the closest weather station to a clicked location.

    require(leaflet, quietly = TRUE)
    require(shiny, quietly = TRUE)
    require(tidyverse, quietly = TRUE)
    require(httr, quietly = TRUE)
    require(jsonlite, quietly = TRUE)
    require(lubridate, quietly = TRUE)
    require(geosphere, quietly = TRUE)
    source('./weatherFunctions.R')

    stationTib <- getWeatherGeo()

### UI

Big changes to the UI. We’ve added a new function call to our
`bootstrapPage` called `absolutePanel`. This panel is “pinned” in place
on the page so its position appears absolute regardless of how the page
is re-sized. This function has a lot of settings we can adjust. This
panel is pinned on the top-right corner (to start) and automatically
adjusts the left and bottom positions. The user can drag the panel
around the screen as desired, but this un-pins to panel.

The `plotOutput` display slots are moved to the panel also. Depending on
you machine’s display setting you may need to play with the dimensions
of the plots to give it the best look (always a struggle).

    ui <- bootstrapPage( # change from fluidPage to bootstrap to add css tags
      tags$style(type = 'text/css', 'html, body {width:100%;height:100%;cursor: crosshair}'),
      leafletOutput('mymap', width='100%', height='100%'),
      absolutePanel(id = 'plots', class = 'panel panel-default', fixed = TRUE,
                    draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
                    width = 280, height = 'auto', style = 'opacity: 0.8',
                    
                    h3(' Quick Plots'),
                    
                    plotOutput("forecastPlot", height = 200),
                    plotOutput("observationPlot", height = 200)
      )
    )

### Server

The only change to the server is the addition of a function call to get
the recent observations of the closest weather station,
`getRecentObservations`.

    server <- function(input, output, session){
      

      output$mymap <- renderLeaflet({
        leaflet(data=stationTib) %>% 
          addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                   attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
          addCircleMarkers(lng = ~ lon,
                           lat = ~ lat,
                           label = ~paste0('station:', stationIdentifier),
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
        event <- input$mymap_click
        if (is.null(event)){
          return()
        }
        isolate({
          forPlot <- getWeatherForcast(event$lat,event$lng)
          output$forecastPlot <- renderPlot({forPlot})
          obPlot <- getRecentObservations(event$lat,event$lng)
          output$observationPlot <- renderPlot({obPlot})
        })
      })
    }

### Apply the changes

    shinyApp(ui, server, options=list(height=600))

When the page loads you’ll see an opaque white box on the right. That’s
the absolute panel. Now when you click the map a new chart appears with
the forecast for that area and a plot for the recent observations of the
nearest station. Both of these plots appear in the panel.
