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


# install.packages("BioCro", repos="http://R-Forge.R-project.org")
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
                                "photosynthesis", "demand", "growth_eff"),
                         text = c("Surface [cm2]", "Length [cm]", "Dry biomass [g]",
                                  "Photosynthesis [g CO2]", "Carbon demand [CO2]", "Growth efficiency [-]"))



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
  updateXML(input$ReWateringFrequence, deparse(substitute(ReWateringFrequence)), xml)
  updateXML(input$RadialModifier, deparse(substitute(RadialModifier)), xml)
  updateXML(input$AxialModifier, deparse(substitute(AxialModifier)), xml)
  updateXML(input$maxPAR, deparse(substitute(maxPAR)), xml)
  updateXML(input$maxTemperature, deparse(substitute(maxTemperature)), xml)
  updateXML(input$JMax, deparse(substitute(JMax)), xml)
  updateXML(input$VMax, deparse(substitute(VMax)), xml)
  updateXML(input$Aerenchyma, deparse(substitute(Aerenchyma)), xml)
  updateXML("www/results.csv", deparse(substitute(outputFileName)), xml)
  
  
  if(input$ResolveCarbon){
    updateXML(1, deparse(substitute(ResolveCarbon)), xml)  
  }else{
    updateXML(0, deparse(substitute(ResolveCarbon)), xml)
  }
  if(input$ResolveWater){
    updateXML(1, deparse(substitute(ResolveWater)), xml)
  }else{
    updateXML(0, deparse(substitute(ResolveWater)), xml)
  }
  if(input$HydraulicRegulation){
    updateXML(1, deparse(substitute(StomataModifier)), xml)  
    updateXML(1, deparse(substitute(AQPModifier)), xml)  
    updateXML(1, deparse(substitute(CavitationModifier)), xml)  
  }else{
    updateXML(0, deparse(substitute(StomataModifier)), xml)  
    updateXML(0, deparse(substitute(AQPModifier)), xml)  
    updateXML(0, deparse(substitute(CavitationModifier)), xml)  
  }
  return(xml)
}


