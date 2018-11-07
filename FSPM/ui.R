#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      actionButton(inputId = "runSim", label="Run model", icon("cogs"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      checkboxInput("plot3D", "3D plot ?", value=F),
      sliderInput("time_to_plot", "Time to plot", min=1, max=14, value=10),
      conditionalPanel(
        condition = "input.plot3D == true",
        plotlyOutput("archiPlot3D", height = "800px")
      ),
      conditionalPanel(
        condition = "input.plot3D == false",
        plotOutput("archiPlot", height = "800px")
      )
    )
  )
))
