library(shiny)

shinyUI(fluidPage(

  # titlePanel("Gibbs Sampling Steps Coin Flip Example"),

  sidebarLayout(
    sidebarPanel(
       sliderInput('n_steps',
                   "Number of Gibbs Steps",
                   min = 0,
                   max = 50,
                   value = 0,
                   animate = TRUE),
       radioButtons('addition', "Information", choices = c("Steps", "True Contours"), selected = "Steps", inline = TRUE)
    ),

    mainPanel(
       plotOutput('stepPlot')
    )
  )
))