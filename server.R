library(shiny)

# Define server logic for random distribution application
function(input, output) {
  
  # Reactive expression to generate the requested distribution.
  # This is called whenever the inputs change. The output
  # functions defined below then all use the value computed from
  # this expression
  data <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data. Also uses the inputs to build
  # the plot label. Note that the dependencies on both the inputs
  # and the data reactive expression are both tracked, and
  # all expressions are called in the sequence implied by the
  # dependency graph
  output$summaryView <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(data(), 
         main=paste(input$tabpanelId, '(', n, ')', sep=''))
  })

  output$barplot <- renderPrint({
      #summary(data())
      input$tabpanelId
  })

  output$genresView <- renderPrint({
      #summary(data())
      input$tabpanelId
  })
  
  output$genresView <- renderPrint({
      #summary(data())
      input$tabpanelId
  })

  output$sourcesView <- renderPrint({
      #summary(data())
      input$tabpanelId
  })

  output$typesView <- renderPrint({
      #summary(data())
      input$tabpanelId
  })
  
  # Generate an HTML table view of the data
  #output$table <- renderTable({
    #data.frame(x=data())
  #})
  
  output$testView <- renderPlot({
      plot(1, 1)
  })
}


