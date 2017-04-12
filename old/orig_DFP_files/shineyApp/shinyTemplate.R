library(shiny)

#fluid page function to create user interface
ui <- fluidPage(
  #a way to get user inputs
  sliderInput(inputId = "num",
              label = "Choose a number",
              value = 5,
              min = 1,
              max = 100),
  #output to display
  plotOutput("hist")
  
)


server <- function(input,output){
  output$hist <- renderPlot(
    {hist(rnorm(input$num))}
    )
  
}

shinyApp(ui = ui, server = server)