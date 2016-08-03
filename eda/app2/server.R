library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(tidyr)
library(ggmap)
library(mapdata)
library(xml2)

shinyServer(function(input, output) {
  output$plot <- renderPlot({
    df = read_csv("cleaned_map_data.csv") %>%
    	filter(year == input$year)
    ditch_the_axes <- theme(
      axis.text = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.title = element_blank()
    )
    states = map_data("state")
    ca_df = filter(states, region == "california")
    ggplot(ca_df, aes(x = long, y = lat, group = group)) + 
      coord_fixed(1.3) + 
      geom_polygon(color = "black", fill = "gray") +
      geom_polygon(data = df, aes_string(fill=input$factor),color = "white")+
      geom_polygon(color = 'black',fill=NA)+
      theme_bw() +
      ditch_the_axes +
      coord_fixed(xlim = c(-123, -121.0),  
                  ylim = c(37, 39), 
                  ratio = 1.3) +
      labs(title = input$year)
  })
})