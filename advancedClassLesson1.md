Shiny Map
=========

Here’s a short example of using advanced `shiny` features along with
`leaflet` and external data building using an API to create an app which
displays the weather information of a location we select. We’ll step
through the app build, adding content and modifying the layers as we add
complexity.

Lesson 1
--------

Let’s create a basic page with a map. We’ll use the `leaflet` package to
generate the map for us, they make great maps and its easy to use. Lets
get started.

### The parts of shiny file

There are three parts (or code blocks) of a Shiny file:

1.  Setup
2.  User Interface (UI)
3.  Server

#### Setup

Things you want all objects and processes to access.

-   Packages
-   External functions
-   Data pre-processing scripts
-   Global data

To start our app we need the packages `shiny` and `leaflet`. We’ll add
other objects as we go along.

    require(leaflet, quietly = TRUE)
    require(shiny, quietly = TRUE)
    require(tidyverse, quietly = TRUE)

#### UI

-   Data object, generates a very large list full of things
-   Creates the externals (or front end) of the webpage
-   Many options and settings
-   Syntax is not intuitive for first time users

We’ll begin with a simple page. First we call the `fluidPage` function.
This page-type gives us nice control over objects within the page and
the page will re-size itself and do some work in rearranging things for
us. To start with we only add an output slot for a leaflet plot. We’ll
link to that slot using the `server`.

    ui <- fluidPage(
      leafletOutput('mymap')
    )

Look at the `ui` object, our `fluidPage` function call created the shell
of a webpage!

    #unlist(ui)
    print(ui[[3]][[1]])

    ## <div class="container-fluid">
    ##   <div id="mymap" style="width:100%; height:400px; " class="leaflet html-widget html-widget-output"></div>
    ## </div>

#### Server

-   Function that does the work (or back end)
-   Generates output, sends to UI for display
-   Tricky syntax
    -   Everything is a sub function
    -   Watch out for object scoping issues

Server is a function that runs the back end of the webpage. It does the
work and passes data to the UI for display. Basically the server is the
brains of the operation, it controls what is displayed by the UI.
Whereas the UI is a large function call with a difficult syntax, the
server syntax is more akin to standard coding where you go top to
bottom, line by line just like a normal script. The trick is keeping
track of object scoping. If you create an object at one point of the
server it may not be available to other commands depending on how its
written.

For our app we’ll create a `leaflet` plot at full zoom (level = 1)
centered on Kansas. The `renderLeaflet` function takes the plot
generated by the enclosed `leaflet` syntax and renders it into an object
the UI will understand. The server sends that object to the appropriate
slot in the UI using the `output` list.

    server <- function(input, output, session){
      output$mymap <- renderLeaflet({
        leaflet() %>% 
          addTiles() %>% 
          setView(-98.5795, 39.8283, zoom=1)
      })
    }

#### Put it together

Now to put it together and create a webpage. The basic method is call
the `shinyApp` function and put it all together. This opens a web
browser and displays the page.

    shinyApp(ui, server)

Now we have a simple webpage. All it does is show a `leaflet` plot with
the settings we give it. But since javascript powers `leaflet` plots we
get a ton of functionality. Here all we can do is zoom in and out but
later we’ll use some of the other features.