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


# //////////////////////////////////////////
# Libraries
# //////////////////////////////////////////

library(tidyverse)
library(cowplot)
library(data.table)
library(plotly)
library(viridis)
library(shinyhelper)
library(XML)
library(plyr)
library(shinyWidgets)
library(shinydashboard)
library(shinyBS)

# //////////////////////////////////////////
# Utility functions
# //////////////////////////////////////////

organ_types <- c("primary root", "lateral root", "stem", "nodal root", "leaf", "seminal root")
organ_class <- c("root", "shoot")

axis_names <- data.frame(id = c("surface", "length", "drymass", 
                                "photosynthesis", "demand", "growth_eff",
                                "water_pot_endo","water_pot_endo","radial_water_flux","stomata_openning","aqp","cavitation",
                                "n_uptake", "n_satis"),
                         
                          text = c("Surface [cm2]", "Length [cm]", "Dry biomass [g]",
                                  "Photosynthesis [g CO2]", "Carbon demand [CO2]", "Growth efficiency [-]",
                                  "Mean endogeneous water potential [MPa]","Mean exogeneous water potential [MPa]","Radial water flux [g H₂O]",
                                  "Relative stomata openning [-]","Relative aquaporin activity [-]","Relative cavitation [-]",
                                  "Nitrogen uptake", "Nitrogen satisfaction coefficient"),
                         
                          descr = c("\n Evolution of the organ surface.\n
                                   Important for the acquisition processes",
                                   
                                   "\n Evolution of the organ length.\n
                                   Important for the exploration of the environment",
                                   
                                   "\n Evolution of the organ dry biomass.\n
                                   Important for the carbon demand",
                                   
                                   "\n Evolution of the total photosynthesis of the plant. \n
                                   Obviously 0 for the roots", 
                                   
                                   "\n Evolution of the total carbon demand for each organ.\n
                                   This is the sum of maintenance and growth demand. ", 
                                   
                                   "\n Evolution of the growth efficiency of each organs. \n
                                   Values <1 means the growth was not completelly satisfied by the C supply.\n 
                                   As a result the organ growth was reduced",
                                   
                                   "\n Evolution of the water potential [MPa] at the plant collar.\n
                                   Indication of the water sense by the plant.\n
                                   Below 1.5 MPa = stress",
                                   
                                   "\n Evolution of the mean exogeneous water potential arpound the plant.\n
                                   This is an an indication of the environment pressure on the plant.",
                                   
                                   "\n Evolution of the sum of water entering (positive) or existing the plant (negative).\n 
                                   The root and shoot values should cancel each other \n
                                   (no storage of water). ",
                                   
                                   "\n Evolution of the relative aperture of the stomata.\n 
                                   If =1, then totaly open. If =0, totaly closed.\n 
                                   This affects the water flow and photosynthesis in the plant",
                                   
                                   "\n Evolution of the relative activity of the aquaporins\n 
                                   If =1, then totaly open. If =0, totaly closed\n 
                                   This aff
                                   ects the radial water flow in the roots",
                                   
                                   "\n Evolution of the relative onset of cavitation.\n 
                                   If =1, then no cavitation. If =0, full cavitation\n 
                                   This affects the axial water flow in the roots",
                                   
                                   "\n Evolution of the nitrogen uptake by the roots",
                                   
                                   "\n Evolution of the nitrogen satisfaction in the plant"))

# Function to update the XML file
updateXML <- function(value, name, xml){
  node = xpathApply(xml, paste0("//", name))
  lapply(node, function(n) {xmlValue(n) <- value})
}


# Add the name of the different organs
updateNames <- function(rs){
  
  # Add the proper article names 
  rs$name[rs$type == 0] <- "seed"
  rs$name[rs$type == 10] <- "primary root"
  rs$name[rs$type == 20] <- "seminal root"
  rs$name[rs$type == 30] <- "nodal root"
  rs$name[rs$type == 11 | rs$type == 21 | rs$type == 31] <- "lateral root"
  rs$name[rs$type == 12 | rs$type == 22 | rs$type == 32] <- "tertiary root"
  rs$name[rs$type == 40] <- "stem"
  rs$name[rs$type == 41] <- "leaf"
  rs$name <- factor(rs$name)
  
  rs$organ <- "root"
  rs$organ[rs$type == 40 | rs$type == 41] <- "shoot"
  rs$organ <- factor(rs$organ)
  
  return(rs)
}

updateParameters <- function(xml, input){
  updateXML(input$NumberOfSeminals, deparse(substitute(NumberOfSeminals)), xml)
  updateXML(input$PrimaryInterBranchDistance, deparse(substitute(PrimaryInterBranchDistance)), xml)
  updateXML(input$RootToShootPartition, deparse(substitute(RootToShootPartition)), xml)
  updateXML(input$SecondaryMaxLength, deparse(substitute(SecondaryMaxLength)), xml)
  updateXML(input$TotalTime, deparse(substitute(TotalTime)), xml)
  updateXML(input$LeafGrowthRate, deparse(substitute(LeafGrowthRate)), xml)
  updateXML(input$LeafApparitionRate, deparse(substitute(LeafApparitionRate)), xml)
  updateXML(input$StressType, deparse(substitute(StressType)), xml)
  updateXML(input$StartStress, deparse(substitute(StartStress)), xml)
  updateXML(-input$WaterPotInit, deparse(substitute(WaterPotInit)), xml)
  updateXML(input$ReWateringFrequence, deparse(substitute(ReWateringFrequence)), xml)
  updateXML(input$WateringFrequence, deparse(substitute(WateringFrequence)), xml)
  updateXML(input$RadialModifier, deparse(substitute(RadialModifier)), xml)
  updateXML(input$AxialModifier, deparse(substitute(AxialModifier)), xml)
  updateXML(input$maxPAR, deparse(substitute(maxPAR)), xml)
  updateXML(input$maxPAR2, deparse(substitute(maxPAR2)), xml)
  updateXML(input$switchPAR, deparse(substitute(switchPAR)), xml)
  updateXML(input$maxTemperature, deparse(substitute(maxTemperature)), xml)
  updateXML(input$JMax, deparse(substitute(JMax)), xml)
  updateXML(input$VMax, deparse(substitute(VMax)), xml)
  updateXML(input$Aerenchyma, deparse(substitute(Aerenchyma)), xml)
  updateXML(input$Km, deparse(substitute(Km)), xml)
  updateXML(input$Knu, deparse(substitute(Knu)), xml)
  updateXML(input$IMax, deparse(substitute(IMax)), xml)
  updateXML(input$NitrogenInit / 25, deparse(substitute(NitrogenInit)), xml)
  updateXML(input$NitrogenInit2 / 25, deparse(substitute(NitrogenInit2)), xml)
  updateXML(input$switchNitrogen, deparse(substitute(switchNitrogen)), xml)
  updateXML("www/results.csv", deparse(substitute(outputFileName)), xml)
  
  
  if(input$ResolveCarbon){
    updateXML(1, deparse(substitute(ResolveCarbon)), xml)  
  }else{
    updateXML(0, deparse(substitute(ResolveCarbon)), xml)
  }
  
  if(input$ResolveNitrogen){
    updateXML(1, deparse(substitute(ResolveNitrogen)), xml)  
  }else{
    updateXML(0, deparse(substitute(ResolveNitrogen)), xml)
  }
  
  if(input$ResolveWater){
    updateXML(1, deparse(substitute(ResolveWater)), xml)
  }else{
    updateXML(0, deparse(substitute(ResolveWater)), xml)
  }
  # if(input$HydraulicRegulation){
  #   updateXML(1, deparse(substitute(StomataModifier)), xml)  
  #   updateXML(1, deparse(substitute(AQPModifier)), xml)  
  #   updateXML(1, deparse(substitute(CavitationModifier)), xml)  
  # }else{
  #   updateXML(0, deparse(substitute(StomataModifier)), xml)  
  #   updateXML(0, deparse(substitute(AQPModifier)), xml)  
  #   updateXML(0, deparse(substitute(CavitationModifier)), xml)  
  # }
  return(xml)
}


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
