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

#
#'TODO
#' plot temperature and light
#'
#'




library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  
  #------- HEADER
  dashboardHeader(
    title = "PlaNet-Maize"#,
    # 
    # dropdownMenu(type = "notifications",
    #              notificationItem(
    #                text = "5 new users today",
    #                icon("users")
    #              ),
    #              notificationItem(
    #                text = "12 items delivered",
    #                icon("truck"),
    #                status = "success"
    #              ),
    #              notificationItem(
    #                text = "Server load at 86%",
    #                icon = icon("exclamation-triangle"),
    #                status = "warning"
    #              )
    # )
  ),
  

  
  #---- SIDEBAR
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      tags$style(".skin-blue .sidebar .shiny-download-link { color: #444; margin-left: 1em; margin-bottom: 1em;}"),
      tags$style("h4 { margin-left: 1em; margin-bottom: 1em;}"),
      
      menuItem("Model", tabName = "results", icon = icon("leaf")),
      
      menuItem("About", tabName = "about", icon = icon("question-circle")),
      tags$hr(),
      actionButton(inputId = "runSim", label="Run model", icon("rocket"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      actionButton(inputId = "resetSim", label="Reset simulations", icon("warning"), style="color: #fff; background-color: #f38f18; border-color: #bc6f14"),
      
      tags$hr(),
      h4("Download results"),
      downloadButton("download_current", "Current simulation"),tags$br(),
      downloadButton("download_all", "All simulations"),
      tags$hr(),
      actionButton(inputId='ab13', label="Report a bug", icon = icon("bug"), onclick ="window.open('#', '_blank')", style="color: #fff; background-color: #d83429; border-color: #d83429"))
      
  ),
  
  
  #----- BODY
  dashboardBody(
    tabItems(
      # Water tab content
      tabItem(tabName = "results",
              fluidRow(
                box(
                  status = "success",  solidHeader = TRUE, title = "View plant architecture [last 2 simulations]",
                  
                  dropdownButton(
                    label = "Setup",
                    icon = icon("sliders"),
                    status = "info",
                    circle = TRUE,
                    materialSwitch("plot3D", "3D plot ?", status = "primary", right = TRUE, value = FALSE),
                    sliderInput("time_to_plot", "Time to plot", min=1, max=14, value=10),
                    selectInput("plot_color", "Variable to color", choices = c("Root type" = "name",
                                                                               "Organ type" = "organ",
                                                                               "Age" = "article_age",
                                                                               "Simulation" = "sim",
                                                                               "Radial water flux" = "radial_water_flux",
                                                                               "Water potential" = "water_pot_endo",
                                                                               "Growth efficiency" = "growth_eff"))
                    
                  ),
                  
                  # checkboxInput("plot3D", "3D plot ?", value=F),
                  # sliderInput("time_to_plot", "Time to plot", min=1, max=14, value=10),
                  
                  conditionalPanel(
                    condition = "input.plot3D == true",
                    plotlyOutput("archiPlot3D", height = "800px")
                  ),
                  
                  conditionalPanel(
                    condition = "input.plot3D == false",
                    plotOutput("archiPlot", height = "800px")
                  )
                ),
                
                
                
                tabBox(
                  # Title can include an icon
                  title = tagList(shiny::icon("gear"), "Parameters"),
                  tabPanel("General",
                           helper(sliderInput("TotalTime", "Simulation time [h]", 100, 400, 300, step = 50), 
                             icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "TotalTime", size = "m")#,
                           
                  ),
                  tabPanel("Carbon",
                           
                           helper(materialSwitch(inputId = "ResolveCarbon", label = "Solve carbon fluxes", status = "success", right = TRUE, value = TRUE), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "ResolveCarbon", size = "m"),
                           
                           bsCollapse(multiple = FALSE, open = "col1", id = "collapse1",
                                bsCollapsePanel("Environment", 
                                    helper(sliderInput("maxPAR", "Photosynthetic active radiation [µmol/m2/sec]", 160, 600, 300, step = 20),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "maxPAR", size = "m"),
                                    helper(sliderInput("maxTemperature", "Temperature [°C]", 10, 30, 20, step = 1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "maxTemperature", size = "m"),
                                    id="col1", value="test1"),
                                bsCollapsePanel("Physiology", 
                                    helper(sliderInput("RootToShootPartition", "Root / Shoot carbon partitioning", 0.1, 0.9, 0.6, step = 0.1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "RootToShootPartition", size = "m"),
                                    helper(sliderInput("VMax", "V_Max", 1, 150, 50, step = 1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "V_Max", size = "m"),
                                    helper(sliderInput("JMax", "J_Max", 1, 300, 100, step = 1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "J_Max", size = "m"), 
                                    helper(sliderInput("Aerenchyma", "Proportion of aerenchyma", 0, 0.9, 0, step = 0.05),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "Aerenchyma", size = "m"), 
                                                id="col4", value="test4"),
                                      
                                  bsCollapsePanel("Root architecture", 
                                                  
                                    helper(sliderInput("NumberOfSeminals", "Number of seminal roots", 0, 6, 1, step = 1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "NumberOfSeminals", size = "m"),
                                    helper(sliderInput("PrimaryInterBranchDistance", "Distance between two laterals [cm]", 0.1, 2, 1.5, step = 0.1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "PrimaryInterBranchDistance", size = "m"),
                                    
                                    helper(sliderInput("SecondaryMaxLength", "Maximal length of lateral roots [cm]", 1, 15, 3, step = 1), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "SecondaryMaxLength", size = "m"),
                                    id="col2", value="test2"),
                                      
                                bsCollapsePanel("Shoot architecture", 
                                    helper(sliderInput("LeafGrowthRate", "Leaf growth rate [cm/°D]", 0.6, 1.2, 0.6, step = 0.1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "LeafGrowthRate", size = "m"),
                                    helper(sliderInput("LeafApparitionRate", "Leaf apparition rate [-]", 0.1, 2, 1, step = 0.1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "LeafApparitionRate", size = "m"),
                                    id="col3", value="test3")
                           )                            
                  ),
                  
                  
                  
                  tabPanel("Other",

                   helper(materialSwitch(inputId = "ResolveWater", label = "Solve water fluxes", status = "primary", right = TRUE, value = FALSE),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "ResolveWater", size = "m"),
                   
                   helper(sliderInput("RadialModifier", "Root radial conductivity [-]", 0.1, 10, 1, step = 0.1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "RadialModifier", size = "m"),

                   helper(sliderInput("AxialModifier", "Root axial conductivity  [-]", 0.1, 10, 1, step = 0.1),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "AxialModifier", size = "m"),

                   helper(materialSwitch(inputId = "HydraulicRegulation", label = "Hydraulic regulation of the plant",status = "success", right = TRUE, value = TRUE),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "AxialModifier", size = "m"),

                  # ),
                  # tabPanel("Environment",
                   tags$h3("Atmosphere"),



                   tags$h3("Water"),

                   helper(selectInput("StressType", "Stress type", choices = c("None" = 0, "Gradual drying" = 11)), icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "StressType", size = "m"),

                   helper(sliderInput("StartStress", "Stress start [h]", 0, 400, 0, step = 20),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "StartStress", size = "m"),

                   helper(sliderInput("ReWateringFrequence", "Rewatering start [h]", 0, 400, 400, step = 20),icon = "question-circle", colour = "#337ab7",type = "markdown", title = "", content = "ReWateringFrequence", size = "m")
                  )
                ),
                
                
                tabBox(
                  # Title can include an icon
                  title = tagList(shiny::icon("leaf"), "Results"),
                  tabPanel("Architecture",
                           dropdownButton(
                             label = "",
                             icon = icon("sliders"),
                             status = "info",
                             circle = FALSE,
                             materialSwitch("plot_organ", "Organ class?", status = "primary", right = TRUE, value = TRUE),
                             conditionalPanel(
                                condition = "input.plot_organ == false",
                                selectInput("plot_name", "Organ type to plot", multiple = T, choices = organ_types, selected = organ_types)
                               ),
                             conditionalPanel(
                                condition = "input.plot_organ == true",
                                selectInput("plot_class", "Organ class to plot", multiple = T,  choices = organ_class, selected = organ_class)

                                ),
                             selectInput("plot_archi", "Variable to plot", multiple = F,  
                                         choices = c("Dry mass [g]" = "drymass",
                                                     "Length [cm]" = "length",
                                                     "Surface [cm2]" = "surface"), selected = 1)
                             
                           ),
                           # tags$hr,
                           plotOutput("evolArchiPlot")
                  ),
                  
                  
                  
                  tabPanel("Carbon",
                           dropdownButton(
                             label = "",
                             icon = icon("sliders"),
                             status = "info",
                             circle = FALSE,
                             materialSwitch("plot_organ_c", "Organ class?", status = "primary", right = TRUE, value = TRUE),
                             conditionalPanel(
                               condition = "input.plot_organ_c == false",
                               selectInput("plot_name_c", "Organ type to plot", multiple = T, choices = organ_types, selected = organ_types)
                             ),
                             conditionalPanel(
                               condition = "input.plot_organ_c == true",
                               selectInput("plot_class_c", "Organ class to plot", multiple = T,  choices = organ_class, selected = organ_class)
                             ),
                             selectInput("plot_carbon", "Variable to plot", multiple = F,  
                                         choices = c("Photosynthesis [g CO₂]" = "photosynthesis",
                                                     "Carbon demand [g CO₂]" = "demand",
                                                     "Growth efficiency [-]" = "growth_eff"), selected = 1)
                             
                           ),
                           # tags$hr,
                           plotOutput("evolCarbonPlot")
                  ),
                  
                  
                  
                  
                  tabPanel("Water",
                           dropdownButton(
                             label = "",
                             icon = icon("sliders"),
                             status = "info",
                             circle = FALSE,
                            selectInput("plot_organ_water", "Organ to plot", multiple = T,  choices = organ_class, selected = organ_class)
                           ),
                           # tags$hr,
                           plotOutput("evolWaterPlot", height = "600px")
                  ),
                  

                  tabPanel("Environment",
                           # tags$hr,
                           plotOutput("evolEnviPlot", height = "600px")
                  )
                )
                
                # box(
                #   status = "info",solidHeader = TRUE, title = "Water status evolution",
                #   plotOutput("evolWaterPlot", height = "600px")
                # ),
                # box(
                #   status = "primary",solidHeader = TRUE, title = "Architecture evolution",
                #   plotOutput("evolArchiPlot", height = "600px")
                # ),
                # 
                # box(
                #   status = "warning",solidHeader = TRUE, title = "Carbon status evolution",
                #   plotOutput("evolCarbonPlot", height = "600px")
                # )
      
              )
      ),
      
      # View plant parameters
      tabItem(tabName = "params",
              fluidRow(
               
                # box(
                #   status = "success",solidHeader = TRUE, title = "Simulation parameters",
                #   sliderInput("TotalTime", "Simulation time [h]", 100, 400, 300, step = 50),
                #   materialSwitch(inputId = "ResolveWater", label = "Solve water fluxes", status = "primary", right = TRUE, value = TRUE),
                #   materialSwitch(inputId = "ResolveCarbon", label = "Solve carbon fluxes", status = "success", right = TRUE, value = TRUE)
                # ),
                # box(
                #   status = "success",solidHeader = TRUE, title = "Tune plant architecture",
                #   sliderInput("n_seminals", "Number of seminal roots", 0, 3, 1, step = 1),
                #   sliderInput("interbranch_distance", "Distance between two laterals", 1, 2, 1.5, step = 0.1),
                #   sliderInput("SecondaryMaxLength", "Maximal length of lateral roots", 1, 5, 3, step = 1),
                #   sliderInput("LeafGrowthRate", "Leaf growth rate [cm/°D]", 0.3, 1.2, 0.6, step = 0.1)
                # ),
                # box(
                #   status = "success",solidHeader = TRUE, title = "Tune plant physiology",
                #   sliderInput("c_partioning", "Root / Shoot carbon partitioning", 0.1, 0.9, 0.6, step = 0.1)
                # )
                # 
              )
      ),
      
      
      # About tab content
      tabItem(tabName = "about",
              box(
                title = "About the app", solidHeader = TRUE, width = 6, status = "primary",
                helpText("The focus of PlaNet-Maize is to investigate the effect of environmental and endogenous factors on the growth and water relations of the maize plant. This functional–structural plant model (FSPM) encompasses the entire soil-plant-atmosphere continuum with a sub-organ resolution. The model simulates the growth and development of an individual maize plant and the flux of water through the plant structure, from the rhizosphere to the leaf boundary layer."),

                helpText("This web interface only display basic capabilities of PlaNet-Maize, mainly for teaching purposes."),
                tags$hr(),
                helpText("The code of this web app is open source and available here:"),
                actionButton(inputId='ab1', label="Source code", icon = icon("th"), onclick ="window.open('#', '_blank')")
                ),
              
              
              box(
                title = "How to cite PlaNet-Maize",  solidHeader = TRUE, width = 6, status = "warning",
                tags$strong("A modeling approach to determine the importance of dynamic regulation of plant hydraulic conductivities on the water uptake dynamics in the soil-plant-atmosphere system (2014)"),
                helpText("Lobet G, Pagès P, Draye X"),
                actionButton(inputId='ab1', label="View paper", icon = icon("flask"), onclick ="window.open('http://dx.doi.org/10.1016/j.ecolmodel.2013.11.025', '_blank')")
              ),
              
              
              box(
                title = "MIT Licence",
                helpText("Planet-Maize-Shiny is released under a MIT licence."),
                helpText("
The MIT License (MIT)

Copyright (c) 2018 Forschungszentrum Jülich
Copyright (c) 2018 UClouvain

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the 'Software'), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.")
              ),
              box(
                title =  "Disclaimer",
                helpText("This software is provided by the copyright holders and contributors 'as is' and any express or implied warranties, including, but not limited to, the implied warranties of merchantability and fitness for a particular purpose are disclaimed. In no event shall the copyright holder or contributors be liable for any direct, indirect, incidental, special, exemplary, or consequential damages (including, but not limited to, procurement of substitute goods or services; loss of use, data, or profits; or business interruption) however caused and on any theory of liability, whether in contract, strict liability, or tort (including negligence or otherwise) arising in any way out of the use of this software, even if advised of the possibility of such damage.")
              )
        )
      
      
      
    )
  )
)
