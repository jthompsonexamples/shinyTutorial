Lesson 3
--------

Now we get technical. We’ll get the locations of all of the weather
stations in the US and plot each location on the map. We’ll use the
National Weather Services’ (NWS) API along with custom functions in the
`weatherFunctions` file. You can find the API documentation \[here\]
(<a href="https://www.weather.gov/documentation/services-web-api" class="uri">https://www.weather.gov/documentation/services-web-api</a>).
Using the map-click information and the API we’ll then get and plot the
temperature forecast for the closest weather station.

### Setup

We’re going to change our Setup block to include some data at rest and a
file with the external functions we need to get our information (Note
these functions require an active internet connection). First, we add
additional packages required to make the API work and read in the
function file using `source` (make sure the file is in your working
directory.

    require(leaflet, quietly = TRUE)
    require(shiny, quietly = TRUE)
    require(tidyverse, quietly = TRUE)
    require(httr, quietly = TRUE)
    require(jsonlite, quietly = TRUE)
    require(lubridate, quietly = TRUE)
    source('./weatherFunctions v2.R')

Next we’ll build our weather station data using the `getWeatherGeo()`
function.

    stationTib <- getWeatherGeo()

Now we have information on each of the ~2500 NWS weather stations,
including the latitude and longitude.

### UI

We’ve changed from a `fluidPage` to a `bootstrapPage` to allow inclusion
of CSS tags in the page. This gives greater control over how the page
displays. We also add an output slot for the forecast plot.

    ui <- bootstrapPage(
      tags$style(type = 'text/css', 'html, body {width:100%;height:100%;cursor: crosshair}'),
      leafletOutput('mymap', width='100%', height='100%'),
      plotOutput('forecastPlot')
    )

### Server

The server will get more and more complicated, as is standard when
making a `shiny` app. Remember, the server is the brains.

We replaced the basic map background with a nice-looking template from
“Mapbox” (always remember to give attribution). We also added data to
our map with markers for the weather stations. To help the user we put
the station identifier as the pop-up label.

In the `observe` function we added a function call to the
`getWeatherForcast` function. This call uses the latitude and longitude
stored from the map click as inputs, sends the info to the API to get
the weather data, and returns a `ggplot` object of the forecast.

    server <- function(input, output, session){
      output$mymap <- renderLeaflet({
        leaflet(data=stationTib) %>% 
        # addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        #          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>') %>% 
        addTiles() %>% 
          addCircleMarkers(lng = ~ lon,
                           lat = ~ lat,
                           label = ~ paste0('Station: ', stationIdentifier),
                           labelOptions = labelOptions(textOnly = FALSE),
                           radius = 3,
                           color = 'green',
                           stroke=FALSE,
                           fillOpacity = 0.7) %>% 
          setView(-98.5795, 39.8283, zoom=3)
      })
      observe({
        event <- input$mymap_click
        if (is.null(event)){
          return()
        }
        isolate({
          m <- getWeatherForcast(event$lat,event$lng)
          output$forecastPlot <- renderPlot({m})
        })
      })
    }

### Apply the changes

    shinyApp(ui, server, options=list(height=600))

Now when you click the map a new chart appears with the forecast for
that area (API errors may occur, try clicking somewhere else if this
happens to you). You may need to scroll down in the map panel, the
forecast plot may be below the map.
