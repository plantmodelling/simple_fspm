


# //////////////////////////////////////////
# Libraries
# //////////////////////////////////////////

library(tidyverse)
library(cowplot)
library(data.table)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
# //////////////////////////////////////////
# Utility functions
# //////////////////////////////////////////

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
