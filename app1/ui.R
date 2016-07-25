library(shiny)
shinyUI(fluidPage(
  titlePanel("Life Expectacy and Income"),
  sidebarLayout(
	sidebarPanel(
	  sliderInput("year",
				  "Year:",
				  min = 1800,
				  max = 2015,
				  value = 1800,
				  step = 1, 
				  animate = animationOptions(interval = 250, loop =T))
	),
	mainPanel(
	  plotOutput("plot")
	)
  )
)
)