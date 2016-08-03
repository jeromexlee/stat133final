library(shiny)
library(ggplot2)
library(readr)
library(scales)
library(dplyr)
shinyServer(function(input, output) {
  output$plot <- renderPlot({
	df = read.csv("cleaned_bubble_data.csv") %>% 
		filter(year == input$year & Type == input$type & County == input$county)
	xaxis = "Date"
	yaxis = "Value"
	region = "City"
	pop = input$factor
	# colors = c("green", "red", "yellow", "purple", "#66787F", "blue")
	ggplot(df) + 
		geom_point(aes_string(x=xaxis, y=yaxis, fill=region),color="black", alpha=0.8, shape=21) + 
		# scale_x_log10(limits = c(500, 50000), breaks = c(500, 5000, 50000), labels = dollar_format(prefix="$")) + 
		# scale_y_continuous(limits = c(20,80), breaks = seq(25,75,25), labels = c("25 \nyears", "50 \nyears", "75 \nyears")) +
		# guides(size = FALSE, alpha = FALSE) + 
		labs(x = "GDP Per Capita (Inflation-Adjusted)", y = "Life Expectancy at Birth", title = input$year)
		# scale_size_area(max_size = 30)
		# scale_fill_manual(values = colors)
  })
})
