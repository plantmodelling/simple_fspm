# Copyright (c) 2018 Forschungszentrum Jülich
# Copyright (c) 2014-2018 UCLouvain
# Copyright (c) 2014-2018 INRA-Avignon
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


library(shiny)
# Define server logic required to draw a histogram
shinyServer(
  
  function(input, output, clientData, session) {
    # uses 'helpfiles' directory by default
    observe_helpers(help_dir = "www/help/")
    
    
    rs <- reactiveValues(data = NULL)
    
    #### Update the time_to_plot slider
    observe({
      req(rs$data)

        updateSliderInput(session, "time_to_plot", value = (max(rs$data$temps[rs$data$sim == max(rs$data$sim)])),
                          min = (min(rs$data$temps[rs$data$sim == max(rs$data$sim)])), max = (max(rs$data$temps[rs$data$sim == max(rs$data$sim)])), step = 24)
    })

    
    observe({
      if(is.null(rs$data)){
        xml <- xmlParse("www/maize.xml")
        xml <- updateParameters(xml, input)  # Update the simulation parameters
        saveXML(xml, file='www/current_maize.xml') # Store the update XML 
        system("java -Xmx6000m -jar www/planet.jar www/current_maize.xml")
        data <- fread("www/results.csv")
        data <- updateNames(data)
        data$sim <- 0
        rs$data<- data
      }
    })
    
    
    observeEvent(input$resetSim, {
      # system("java -Xmx6000m -jar www/planet.jar www/current_maize.xml")
      # data <- fread("www/results-base.csv")
      # data <- updateNames(data)
      # data$sim <- 0
      rs$data<- rs$data[rs$data$sim == max(rs$data$sim),]
    })
    
    #### Plot evolution of plant and envi parameters

    output$evolEnviPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}
      
      temp <- rs$data[rs$data$z < -5,]
      temp$z <- round(temp$z)
      
      temp <- ddply(temp, .(sim, temps, z), summarise, 
                    water_pot = mean(water_pot_exo[organ == "root"])
      )
      
      temp %>%
        gather(key=var, value=value, -c(sim,temps, z)) %>% 
        ggplot(aes(z, value, colour=factor(temps), group=factor(temps))) +
        coord_flip() + 
        geom_line() + 
        facet_wrap(~sim, ncol = 2) +
        xlab("Depth [cm]") + 
        ylab("Soil water content [MPa]") 
    })
    
    
    # Plot Carbon related variables
    output$evolCarbonPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}
      
      if(input$plot_organ_c){
        temp <- ddply(rs$data, .(sim, temps, organ), summarise, 
                      photosynthesis = sum(photosynthesis[organ == "shoot"]),
                      growth_eff = mean(growth_eff),
                      demand = sum(growth_demand + maintenance_demand)
        )
        pl <- temp %>%
          gather(key=var, value=value, -c(sim,temps,organ)) %>% 
          filter(organ %in% input$plot_class_c) %>% 
          filter(var == input$plot_carbon) %>% 
          ggplot(aes(temps, value, colour=factor(sim), lty=organ))
      }else{
        temp <- ddply(rs$data, .(sim, temps, name), summarise, 
                      photosynthesis = sum(photosynthesis[organ == "shoot"]),
                      growth_eff = mean(growth_eff),
                      demand = sum(growth_demand + maintenance_demand)
        )
        pl <- temp %>%
          gather(key=var, value=value, -c(sim,temps,name)) %>% 
          filter(name %in% input$plot_name_c) %>% 
          filter(var == input$plot_carbon) %>% 
          ggplot(aes(temps, value, colour=factor(sim), lty=name))
      }
      
      pl + geom_line() + 
        # facet_wrap(~var, scales="free", ncol = 2) + 
        geom_vline(xintercept = (input$time_to_plot), lty=2) +
        xlab("Time [hours]") + 
        ylab(axis_names$text[axis_names$id == input$plot_carbon])+
        ggtitle(axis_names$text[axis_names$id == input$plot_carbon])+ 
        labs(caption=axis_names$descr[axis_names$id == input$plot_carbon])
      
      
    })
        
    # Plot architecture related variables
    output$evolWaterEnviPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}
      
       temp <- rs$data %>% 
         filter(z < 0 & z > -5) %>% 
         ddply( .(sim, temps, organ), summarise, 
                      value = mean(water_pot_exo))
        pl <- temp %>%
          ggplot(aes(temps, value, colour=factor(sim)))
      
      pl + geom_line() + 
        # facet_wrap(~var, scales="free", ncol = 2) + 
        geom_vline(xintercept = (input$time_to_plot), lty=2) +
        xlab("Time [hours]") + 
        ylab("Water potential in top soil layer [MPa]")+
        ggtitle("Water potential in top soil layer [MPa]")
    })
    
    # Plot water related variables
    output$evolWaterPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}
      
      temp <- ddply(rs$data, .(sim, temps, organ), summarise,
                      radial_water_flux = sum(radial_water_flux),
                      water_pot_exo = mean(water_pot_exo[organ == "root" & z < -5]),
                      stomata_openning = mean(Lr_percent[organ == "shoot"]),
                      water_pot_endo = mean(water_pot_endo),
                      aqp = mean(aqp),
                      cavitation = mean(cavitation))
      
      temp %>%
        gather(key=var, value=value, -c(sim,temps,organ)) %>% 
        filter(organ %in% input$plot_organ_water) %>% 
        filter(var == input$plot_water) %>% 
        ggplot(aes(temps, value, lty=organ, colour=factor(sim))) +
        geom_line() + 
        # facet_wrap(~var, scales="free", ncol = 2) + 
        geom_vline(xintercept = (input$time_to_plot), lty=2) +
        xlab("Time [hours]") + 
        ylab(axis_names$text[axis_names$id == input$plot_water])+
        ggtitle(axis_names$text[axis_names$id == input$plot_water]) + 
        labs(caption=axis_names$descr[axis_names$id == input$plot_water])
    })
    
    
    # Plot water related variables
    output$evolNitroPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}
      
      temp <- ddply(rs$data, .(sim, temps, organ), summarise,
                    n_uptake = sum(n_uptake[organ == "root" & z < -5]),
                    n_satis = mean(n_satis[organ == "root" & z < -5]))
      
      temp %>%
        gather(key=var, value=value, -c(sim,temps,organ)) %>% 
        filter(var == input$plot_nitrogen) %>% 
        ggplot(aes(temps, value, lty=organ, colour=factor(sim))) +
        geom_line() + 
        # facet_wrap(~var, scales="free", ncol = 2) + 
        geom_vline(xintercept = (input$time_to_plot), lty=2) +
        xlab("Time [hours]") + 
        ylab(axis_names$text[axis_names$id == input$plot_nitrogen])+
        ggtitle(axis_names$text[axis_names$id == input$plot_nitrogen]) + 
        labs(caption=axis_names$descr[axis_names$id == input$plot_nitrogen])
    })
    
    
    # Plot architecture related variables
    output$evolArchiPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}

      if(input$plot_organ){
        temp <- ddply(rs$data, .(sim, temps, organ), summarise, length = sum(article_length),
                      drymass = sum(article_dry_mass),
                      surface = sum(article_surface))
        pl <- temp %>%
          gather(key=var, value=value, -c(sim,temps,organ)) %>% 
          filter(organ %in% input$plot_class) %>% 
          filter(var == input$plot_archi) %>% 
          ggplot(aes(temps, value, colour=factor(sim), lty=organ))
          
      }else{
        temp <- ddply(rs$data, .(sim, temps, name), summarise, length = sum(article_length),
                      drymass = sum(article_dry_mass),
                      surface = sum(article_surface))
        pl <- temp %>%
          gather(key=var, value=value, -c(sim,temps,name)) %>% 
          filter(name %in% input$plot_name) %>% 
          filter(var == input$plot_archi) %>% 
          ggplot(aes(temps, value, colour=factor(sim), lty=name))
      }
      
      pl + geom_line() + 
        # facet_wrap(~var, scales="free", ncol = 2) + 
        geom_vline(xintercept = (input$time_to_plot), lty=2) +
        xlab("Time [hours]") + 
        ylab(axis_names$text[axis_names$id == input$plot_archi])+
        ggtitle(axis_names$text[axis_names$id == input$plot_archi])+ 
        labs(caption=axis_names$descr[axis_names$id == input$plot_archi])
    })
        
    
    #### Plot 2D root architecture 
    
    output$archiPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}
      
      
      pl <- rs$data %>%
        filter(temps == (input$time_to_plot),
               sim %in% c(max(sim),max(sim)-1)) %>%
        mutate(sim = factor(sim)) %>% 
        ggplot() +
        geom_segment(aes_string(x = "x", y = "z", xend = "x1", yend = "z1", colour=input$plot_color)) +
        coord_fixed() # +
        # ylim(range(rs$data$z)) +
        # xlim(range(rs$data$x))
      
      if(!input$plot_color %in% c("name", "sim", "organ")){
        pl <- pl + scale_color_viridis_c()
      } 
      pl
      # 
      # data2 %>%
      #   filter(temps == (10 * 24) + 12) %>%
      #   ggplot() +
      #   geom_segment(aes(x = x, y = z, xend = x1, yend = z1)) +
      #   coord_fixed()  +
      #   ylim(range(data$z)) +
      #   xlim(range(data$x))
    })
    
    
    #### Plot 3D root architecture 
    
    output$archiPlot3D <- renderPlotly({
      if(is.null(rs$data)){return(NULL)}      
      
      temp <- rs$data
      temp$var <- temp[[input$plot_color]]
      
      temp %>%
        filter(temps == (input$time_to_plot),
               sim %in% c(max(sim),max(sim)-1)) %>%
        plot_ly(x = ~x, y = ~y, z = ~z, color = ~var) %>%
        add_markers(marker= list(size=2)) %>%
        layout(scene = list(xaxis = list(range = range(temp$x)), 
                            yaxis = list(range = range(temp$y)), 
                            zaxis = list(range = range(temp$z))))
    })
    
    
    #### Run the simulation
    
    observeEvent(input$runSim, {
      
      # Read te base XMML file containing the parameter simulation
      xml <- xmlParse("www/maize.xml")
      
      # Update the simulation parameters
      xml <- updateParameters(xml, input)

      # Store the update XML file to be used in the simulation
      saveXML(xml, file='www/current_maize.xml')
      
      # withProgress(message = 'Creating the base plant', value = 0, {
      #   # Run the simulation
      #   system("java -Xmx6000m -jar www/planet.jar www/maize.xml")
      # })
      withProgress(message = 'Creating the plant', value = 0, {
        # Run the simulation
        system("java -Xmx6000m -jar www/planet.jar www/current_maize.xml")
      })
      
      # Load the output data and store it for the plots
      data1 <- fread("www/results.csv") %>% 
        mutate(sim = max(rs$data$sim)+1)

      dev <- (max(data1$x) - min(rs$data$x[rs$data$sim == max(rs$data$sim)])) + 10
      data1<- data1 %>%
        mutate(x = x + (30 * (max(rs$data$sim)+1))) %>%
        mutate(x1 = x1 + (30 * (max(rs$data$sim)+1)))
      # data1 <- data1[data1$temps <= max(data$temps),]
      
      # data2 <- fread("www/results-base.csv") %>% 
      #   mutate(sim= "base")
      # dev <- (max(data1$x)-min(data2$x)) + 10
      # data2<- data2 %>%
      #   mutate(x = x + dev) %>%
      #   mutate(x1 = x1 + dev)
      # data2 <- data2[data2$temps <= max(data1$temps),]
      
      
      # data <- rbind(data1, data2)
      data1 <- updateNames(data1) # Update the root type names
      print("Loading data done")
      rs$data <- rbind(rs$data, data1)
      
      print(nrow(data1) )
      if(nrow(data1) < (input$TotalTime*7)){
        sendSweetAlert(session, title = "Your plant died!", text = NULL, type = "error",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }else{
        sendSweetAlert(session, title = "Simulation done!", text = NULL, type = "success",
                       btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
      }
    })
    
    
    ##------ DOWNLOADS
    
    output$download_current <- downloadHandler(
      filename = function() {
        "current_sim.csv"
      },
      content = function(file) {
        if(is.null(rs$data)){return ()}
        write.csv(rs$data[rs$data$sim == max(rs$data$sim),], file = file)
      }
    )
    
    output$download_all <- downloadHandler(
      filename = function() {
        "all_sim.csv"
      },
      content = function(file) {
        if(is.null(rs$data)){return ()}
        write.csv(rs$data, file = file)
      }
    )
    
    
  })