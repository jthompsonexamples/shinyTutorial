Lesson 2
--------

There are a few ways we could select the location to get the weather,
but to be as interactive as possible we’ll let the user click on the map
and use that click to pick the location. Much more interesting than
typing in a zip code or lat-long.

### UI

First we’ll modify our UI to give us a display slot for the lat-long.
This helps in debugging, we want to make sure the mechanics of getting
the information works before attaching other complicated processes.

    ui <- fluidPage(
      leafletOutput('mymap', width=800, height=350),
      textOutput('latlon'),
      p()
    )

We added a simple text output for the UI to display the information the
server gets for us.

### Server

Now modify the server to get the information. This is more complicated,
it requires a knowledge of reactivity in `shiny` and how `leaflet`
works. A mouse click on the map creates an object with the latitude,
longitude of the location clicked along with additional information
about the layers if there are any. We can access that information by
capturing the information in the `event` object. We use `observe` to
watch for that specific action and send the text to the UI using the
`renderText` function. The `isolate` function prevents creating a
reactive object when we render the text.

    server <- function(input, output, session){
      output$mymap <- renderLeaflet({
        leaflet() %>% 
          addTiles() %>% 
          setView(-98.5795, 39.8283, zoom=3)
      })
      observe({
        event <- input$mymap_click
        if (is.null(event)){
          return()
        }
        isolate({
          output$latlon <- renderText(paste0('Latitute=',event$lat,', Longitude=',event$lng))
        })
      })
    }

### Apply the changes

    shinyApp(ui, server, options=list(height=600))

Click somewhere, the Latitude and Longitude of that location should
appear each time you click. We’ll remove this text later, but we know we
have a working map.
