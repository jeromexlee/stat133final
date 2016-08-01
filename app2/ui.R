library(shiny)
library(readr)

question=read_csv("cleaned_question.csv")
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("question", "Choose a question:", 
                  choices = question$quest )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
))