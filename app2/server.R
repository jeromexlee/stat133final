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
    full_data = read.csv("cleaned_data.csv") %>%
    	filter(quest == input$question) %>%
      	right_join(map_data("state"),by="region")
    ggplot(full_data) + 
      geom_polygon(aes(x = long, y = lat, fill = answer, group = group), color = "black") +
      coord_fixed(1.3)+
      scale_fill_discrete(labels=function(x) str_wrap(x,width=20))+
      labs(title=input$question, x = "", y = "")
  })
})