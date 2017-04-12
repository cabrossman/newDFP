shinyUI(bootstrapPage(
  # Add custom CSS & Javascript;
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "busy.js")
    )
  ),
  div(class = "busy",  
      p("Calculation in progress.."), 
      img(src="ajax-loader.gif")
  ),
  div(class = "span4", uiOutput("obs")),
  div(class = "span8", plotOutput("distPlot"))
))