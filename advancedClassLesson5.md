Lesson 5
--------

We change the page type again to allow for more features. We also add
user controls to the observations plot and add a data table showing the
recent observations on another page.

### Setup

No change here. That was easy.

### UI

We’ve changed the page type again, this time we’re using a `navbarPage`
so we can create different tabs across the top to display different
information. We send information to each tab using the `tabPage`
function and put the code for each page inside that function. In the map
page’s code we include a function `fillPage` which forces the map to
always fill the available area of the browser window. At the bottom of
the absolute panel we add a date range slider to control the dates of
the plotted observations along with a “redraw” button. The `fluidRow`
and `column` functions format the controls’ locations.

In the second tab panel we simply add a slot to display a table.

    ui <- navbarPage(title='Weather Map',
      tabPanel('Maps',
        fillPage(
          tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;cursor: crosshair}"),
        leafletOutput('mymap', width='100%', height='100%')),
        absolutePanel(id = 'plots', class = 'panel panel-default', fixed = TRUE,
                      draggable = TRUE, top = 60, left = 'auto', right = 20, bottom = 'auto',
                      width = 280, height = "auto", style = 'opacity: 0.8',
                      
                      h3('Quick Plots'),
                      
                      plotOutput('forecastPlot', height = 200),
                      plotOutput('observationPlot', height = 200),
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
                          12, align='center',
                          actionButton('redrawPlot','Redraw')
                        )
                      )
        ),
      ),
      tabPanel('Observation Explorer',
               tags$style(type = "text/css", "html, body {overflow-y:scroll}"),
               tableOutput('dataOut')
      )
    )

### Server

In the server we first add a message box telling the user the API is
running and not to be impatient. Next we modify the server to allow for
redrawing the the observation plot without having to rerun the API. We
move the observation object outside of the `isolate` function which
changes it’s scope, we want to access that information whenever we want
to redraw the observations plot. We add a data table render and send the
information to the correct slot on the data tab. Finally, we tie the
“redraw” button to a redraw event which filters the observation data
based on the dates on the date range slider.

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
      obPlot <- reactiveValues()
      observe({
        event <- input$mymap_click
        if (is.null(event)){
          return()
        }
        showModal(modalDialog(
          title = 'Connecting',
          'Waiting for API response'
        ))
        obPlot$p <- getRecentObservations(event$lat,event$lng)
        isolate({
          forPlot <- getWeatherForcast(event$lat,event$lng)
          output$forecastPlot <- renderPlot({forPlot})
          output$observationPlot <- renderPlot({obPlot$p$plot})
          output$dataOut <- renderTable(obPlot$p$obs)
        })
        removeModal()
      })
      observeEvent(input$redrawPlot, {
        # redraw button clicked
          newPlot <- plotRecentObservations(obPlot$p$obs, obPlot$p$station,
                                            input$dateSlider[1], input$dateSlider[2])
          output$observationPlot <- renderPlot({newPlot})
        })
    }

### Apply the changes

    shinyApp(ui, server, options=list(height=600))

When the page loads you’ll see two tabs on top and the range slider with
button in the panel. The charts appear as they did before, but now you
can adjust the date range of the observation display. Also, the data tab
fills with the table of all available observations for that weather
station.
