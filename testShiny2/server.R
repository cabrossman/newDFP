shinyServer(function(input, output) {
  output$obs <- renderUI({
    sliderInput("obs", "Number of observations:", 
                min = 10000, max = 90000, 
                value = 50000, step = 10000)
  })
  output$distPlot <- renderPlot({
    if (!is.null(input$obs)) {
      dist <- NULL
      for (i in 1:input$obs) {
        dist <- c(dist, rnorm(1))
      }
      hist(dist, breaks = 100)
    }
  })
})