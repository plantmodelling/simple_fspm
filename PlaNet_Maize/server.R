# Copyright © 2018, Université catholique de Louvain
# All rights reserved.
# 
# Copyright © 2018 Forschungszentrum Jülich GmbH
# All rights reserved.
# 
# Developers: Guillaume Lobet
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted under the GNU General Public License v3 and provided that the following conditions are met:
#   
#   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# 3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
# 
# Disclaimer
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# You should have received the GNU GENERAL PUBLIC LICENSE v3 with this file in license.txt but can also be found at http://www.gnu.org/licenses/gpl-3.0.en.html
# 
# NOTE: The GPL.v3 license requires that all derivative work is distributed under the same license. That means that if you use this source code in any other program, you can only distribute that program with the full source code included and licensed under a GPL license.


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
        # system("java -Xmx6000m -jar www/planet.jar www/current_maize.xml")
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
        xlab("Time [hours]")
    })
    
    output$evolCarbonPlot <- renderPlot({
      if(is.null(rs$data)){return(NULL)}
      
      # temp <- ddply(rs$data, .(sim, temps, organ), summarise, 
      #               photosynthesis = sum(photosynthesis[organ == "shoot"]),
      #               growth_eff = mean(growth_eff),
      #               demand = sum(growth_demand + maintenance_demand)
      #               )
      # 
      # temp %>%
      #   gather(key=var, value=value, -c(sim,temps,organ)) %>% 
      #   ggplot(aes(temps, value, lty=organ, colour=factor(sim))) +
      #   geom_line() + 
      #   facet_wrap(~var, scales="free", ncol = 2) + 
      #   geom_vline(xintercept = (input$time_to_plot), lty=2) +
      #   xlab("Time [hours]")
      
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
        ylab(axis_names$text[axis_names$id == input$plot_carbon])
      
      
    })
        
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
        ggplot(aes(temps, value, lty=organ, colour=factor(sim))) +
        geom_line() + 
        facet_wrap(~var, scales="free", ncol = 2) + 
        geom_vline(xintercept = (input$time_to_plot), lty=2) +
        xlab("Time [hours]")
    })
    
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
        ylab(axis_names$text[axis_names$id == input$plot_archi])
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
      sendSweetAlert(session, title = "Simulation done!", text = NULL, type = "success",
                     btn_labels = "Ok", html = FALSE, closeOnClickOutside = TRUE)
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