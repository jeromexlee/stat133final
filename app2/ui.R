library(shiny)
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("factor", "Choose a type or factor:", 
                  choices = list("Pop","Income","A","SF","MVSF","twoB","threeB","fourB"),
                  selected = "Pop"),
      sliderInput("year",
				  "Year:",
				  min = 1996,
				  max = 2014,
				  value = 1996,
				  step = 1, 
				  animate = animationOptions(interval = 250, loop =T))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
))