library(shiny)
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("factor", "Choose a factor:", 
                  choices = list("Pop","Income"),
                  selected = "Pop"),
      selectInput("type", "Choose a type:", 
                  choices = list("A","SF","MVSF","2B","3B","4B"),
                  selected = "A"),
      selectInput("county", "Choose a county:", 
                  choices = list("alameda","contra costa","san mateo","marin","napa","sacramento","santa clara","san francisco"),
                  selected = "alameda"),
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