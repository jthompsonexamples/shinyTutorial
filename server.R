
server <- function(input, output, session){
  # create a reactive object that fires when the plotAction button is clicked
  # store the results in this counter-intuitive way
  options(shiny.maxRequestSize=30*1024^2)
  df <- reactive({
    req(input$inputFile)
    read_csv(input$inputFile$datapath)
  })
  observe({
    #input$inputFile
    df()
    if (is.null(df()))
      return(NULL)
    updateSelectInput(
      session,
      'necSelect',
      'Which NECs?',
      choices = c('All', df() %>% distinct(asunit) %>% pull())
    )
  })
  
  p <- eventReactive(input$plotAction, {
    exercisePlot(df(), input$necSelect, input$dateRange[1],
                 input$dateRange[2], input$priorityCheck,
                 input$countCheck)
  },
  ignoreInit = TRUE) # does not run the code when the app initializes
  # put the plot and table on the page
  output$boxPlot <- renderPlot({p()$plot})
  output$dataTable <- renderTable({p()$tab})
  #output$dataTable <- renderTable({df()})
}
