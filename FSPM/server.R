#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output, clientData, session) {
    
  rs <- reactiveValues(data = NULL)
  
  output$archiPlot <- renderPlot({
    # data <- fread("www/results.csv")
    # data <- updateNames(data)
    # message("plotting data")
    if(is.null(rs$data)){return(NULL)}
    # message("plotting data 2")
    # Plot by article type
    # data <- rs$data
    rs$data %>%
        # filter(temps == (14 * 24) + 12) %>%
        filter(temps == (input$time_to_plot * 24) + 12) %>%
        ggplot() +
        geom_segment(aes(x = x, y = z, xend = x1, yend = z1, colour=name)) +
        coord_fixed()  +
        ylim(range(data$z)) +
        xlim(range(data$x))

  })
  
  output$archiPlot3D <- renderPlotly({
    # data <- fread("www/results.csv")
    # data <- updateNames(data)
    # message("plotting data")
    if(is.null(rs$data)){return(NULL)}
    # message("plotting data 2")
    # Plot by article type
    # data <- rs$data
    
    rs$data %>%
        filter(temps == (input$time_to_plot * 24) + 12) %>%
        plot_ly(x = ~x, y = ~y, z = ~z, color = ~name) %>%
        # add_markers(line = list(width = 1), marker= list(size=1)) %>%
        add_markers(marker= list(size=2)) %>%
        layout(scene = list(xaxis = list(range = range(data$x)), 
                            yaxis = list(range = range(data$y)), 
                            zaxis = list(range = range(data$z))))
    
    
  })
  
  
  
  observeEvent(input$runSim, {
    
    system("java -Xmx6000m -jar www/planet.jar www/current_maize.xml")
    
    message("Loading data")
    
    data <- fread("www/results.csv")
      
    # # Keep the aggregated data for latter use
    # temp <- ddply(rs, .(temps, type), summarize, 
    #               length = sum(article_length),
    #               surface = sum(article_surface),
    #               mass = sum(article_dry_mass),
    #               count = length(article_dry_mass),
    #               growth_eff = mean(growth_eff),
    #               maintenance = mean(maintenance_demand),
    #               growth = mean(growth_demand))
    # temp$sim <- gsub("planet-", "", gsub(".csv", "", f))
    # if(is.null(evol)) evol <- temp
    # else evol <- rbind(evol, temp)
      
      # Keep the fulla rchitecture data for the last time point
    # rs <- rs[rs$temps == max(unique(rs$temps)),] # Get only the last step of the simulation
    # data$sim <- gsub("planet-", "", gsub(".csv", "", f))
    # # if(is.null(data)) data <- rs
    # # else data <- rbind(data, rs)
    # 
    # data <- data[data$sim %in% sims,]
    message("Loading data done1")
    
    data$width <- round(data$article_diameter*100)
    data <- updateNames(data)
    
    rs$data <- data 
    print(str(rs$data))
    message("Loading data done")
    
  })
  
})
