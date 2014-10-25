
# This is the user-interface definition of a Shiny web application.

library(shiny)

shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Bootstrapping"),
  
  # Sidebar with a slider input for number of bins
  sidebarPanel(
    sliderInput("sample_size",
                "Sample size:",
                min = 1,
                max = 50,
                value = 30),
    radioButtons("radio", label = div("Parameter of interest:"),
                 choices = list("Mean" = 1, "Median" = 2)),
    actionButton("rerun", "Rerun simulation"),
    br(),
    br(),
    img(src="bootstraps.png")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    htmlOutput("title"),
    plotOutput("dotPlot", height="450px"),
    htmlOutput("estimates"),
    h4("Instructions"),
    p("Bootstrapping is a resampling technique to estimate a statistical parameter.
      This app supports estimating either the Mean or Median. Using this technique,
      we create a random sample with replacement of size adjusted by the slider.
      Since this is a simulation technique, the results may vary each time. The
      simulation can be rerun by clicking the \"Rerun simulation\" button."),
    p("The dotplot shows a histogram of number of samples where the mean or median
      is equals to the value along the x-axis."),
    p("The term \"bootstrapping\" is taken from the phrase, \"To pull oneself up by
      one's bootstraps.\" A seemingly impossible feat. In statistics, bootstrapping
      can improve on the estimate of the population statistic by reusing the same
      sample over and over again.")
  )
))
