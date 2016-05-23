library(shiny)

shinyUI(fluidPage(

  # titlePanel("Gibbs Sampling Steps Normal Gamma"),

  sidebarLayout(
    sidebarPanel(
      sliderInput('n_steps',
                  "Number of Gibbs Steps",
                  min = 0,
                  max = 60,
                  value = 0,
                  animate = TRUE),
      radioButtons('addition', "Information", choices = c("Steps"), selected = "Steps", inline = TRUE)
    ),

    mainPanel(
      plotOutput('stepPlot')
    )
  )
))
