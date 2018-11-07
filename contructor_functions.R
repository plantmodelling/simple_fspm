

#' Parameters constructor. The parameters object stored both the initial parameters and 
#' the current state of the simulation (such as the time)
#' @param root_growth_rate = growth rate of primary roots, in cm/day
#' @param root_branching_age = age before the branching of root (days)
#' @param root_diameter_ratio = ratio of diameter between root of successive type
#' @param root_tropism = indicator of root tropism strengh
#' @param root_max_lat_length = maximal length of lateral roots
#' @param root_diameter = primary root diameter (cm)
#' @param root_dry_mass_ratio = root tissue density [g / m3]
#' 
#' @param stem_growth_rate = growth rate of stem, in cm/day
#' @param stem_branching_age = age before the production of leaves (degree days)
#' @param stem_diameter = diameter of the stem (cm)
#' @param stem_dry_mass_ratio = stem tissue density [g / m3]
#'  
#' @param leaf_growth_rate = growth rate of leaf, in cm/day
#' @param leaf_orientation = left/right orientation of the leaf
#' @param leaf_max_length = Maximal length for a leaf (cm)
#' @param leaf_max_number = Maximal number of leaves before flowering
#' @param leaf_number = Current leaf number
#' @param leaf_tropism = indicator of leaf tropism strengh
#' @param leaf_width = leaf width [cm]
#' @param leaf_thickness = leaf thickness [cm]
#' @param leaf_dry_mass_ratio = leaf tissue density [g / m3]
#'  
#' @param flower_growth_rate = growth rate of flower, in cm/day
#' @param flower_max_number = Maximal number of flower on the inflorescence
#' @param flower_max_diameter = Maximal diameter of flower on the inflorescence
#' @param flower_number = Current flower number
#' @param flower_dry_mass_ratio = flower tissue density [g / m3]
#' 
#' @param tiller_number = current number of tillers
#' @param tiller_max_number = max number of tillers
#' @param tiller_timestamp = Time between which tillers are initiated
#' 
#' @param max_id = the maximal id of the different nodes
#' @param max_organ_id = the maximal id of the different organs
#' @param current_time = the current time in the simulation
#' @param temperature_mean = the mean daily temperature during the simulation [°C]
#' @param precipitation_mean = the mean daily recipitation during the simulation [mm]
#' 
#' @param potential_growth = potential growth for the next time step
#' @param maintenance_respiration_rate = Amount of carbon needed for the maintenance of the [gCO2/gDM]  (Drouet & Pagès 2003) 
#' @param temp_coefficient = Temperature coefficient used to compute the carbon maintenance demand  (Drouet & Pagès 2003)
#' @param carbon_growth_efficiency = Amount of carbon needed for the growth of the article [gCO2/gDM] (Drouet & Pagès 2003)
#' 
#' @return the parameters
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @export
#' @examples
#' params <- parameters()
#' params$root_growth_rate <- 0.8

simulationParameters = 
  function()
  {
    rts = list(root_growth_rate = 0.5,
               root_branching_age = 10,
               root_diameter_ratio = 0.6,
               root_tropism = 0.1,
               root_max_lat_length = 5,
               root_diameter = 0.2,
               root_dry_mass_ratio = 0.05, # Carbon. From Drouet and Pagès 2003
               
               stem_growth_rate = 1,
               stem_branching_age = 100,
               stem_diameter = 1,
               stem_dry_mass_ratio = 0.005, # Carbon. From Drouet and Pagès 2003
               
               leaf_growth_rate = 0.5,
               leaf_max_length = 4,
               leaf_max_number = 5,
               leaf_orientation = 1,
               leaf_number = 0,
               leaf_tropism = 0.6,
               leaf_width = 3,
               leaf_thickness = 0.1,
               leaf_dry_mass_ratio = 0.006, # Carbon. From Drouet and Pagès 2003
               
               flower_max_number = 10,
               flower_max_diameter = 2,
               flower_number = 0,
               flower_growth_rate = 0.5,
               flower_dry_mass_ratio = 0.05, # Carbon. From Drouet and Pagès 2003
               
               tiller_number = 0,
               tiller_max_number = 0,
               tiller_timestamp = 5,
               
               max_id = 0,
               max_organ_id = 0,
               current_time = 0,
               temperature_mean = 15,
               precipation_mean = 2.5,
               carbon_growth_efficiency = 0.73,
               maintenance_respiration_rate = 0.0032,
               temp_coefficient = 2
               )
    class(rts) = "parameters"
    rts
  }


#' Simulation environment
#' @param soil
#' @param atmosphere 
#' @return the environment
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @export
#' @examples

simulationEnvironment = 
  function()
  {
    rts = list(soil=soil(), 
               atmosphere=atmosphere())
    rts
  }


#' Atsmophere 
#' @param temperatures
#' @param precipitations
#' @param days
#' @param light 
#' @return a vector with the temperature over 200 days
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
atmosphere = 
  function()
  {
    rts = list(temperatures = sample(c(12:18), 200, replace=T), 
               precipitations = sample(c(0:6), 200, replace=T),
               days = c(1:200),
               light = rep(500, 200))
    rts
  }

#' Soil 
#' @params water content
#' @return the soil environent
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
soil = 
  function() # TODO
  {
    rts = list(water = NULL,
               days = c(1:200))
    rts
  }



#' Plant constructor
#' @param nodes = the nodes composing the plant. Can be null
#' @param params = the parameter set associated with the simulation
#' @param environment = the environment associated with the simulation
#' @param data = the computed data at each simulation time point (as a tibble)
#' @return the plant
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @export
#' @examples
#' r <- plant()
#' 
#' n <- node(1, 1)
#' r <- plant(n)
plant = 
  function(nodes = NULL, params = simulationParameters(), data=NULL)
  {
    rts = list(nodes = nodes, params = params, data=data, environment=simulationEnvironment())
    class(rts) = "plant"
    rts
  }


#' Node constructor
#' @param x = x coordinate of the node. Mandatory
#' @param y = y coordinate of the node. Mandatory
#' @param z = z coordinate of the root. Optional
#' @param diameter = diameter of the node. Optional
#' @param orientation = orientation of the node. Optional
#' @param bLength = lenght from the node position in the root from the base of the root. Optional
#' @param type = type of organ. Either root, stem or leaf. Optional
#' @param age = age of the node, in days. Optional
#' @param parent = parent node in the tree structure. Optional
#' @param id = unique identifier of the node. Optional
#' @param creation_time: Time in the simulation at which the node was created
#' @param TTage = Thermal time age of the object
#' @param growing = can the node still grow ?
#' @param branching = can the node still branch ?
#' @param order = topological order of the node
#' @param organ_id = identifier of the organ
#' @param direction = the direction of the organ (left/right)
#' @param volume = the volume of the organ (cm3)
#' @param surface = the surface of the organ (cm2)
#' @param length = the length of the organ (cm)
#' @return the node
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @keywords fspm
#' @export
#' @examples
#' n <- node(1, 1)
node =
  function(x, y, z=0, diameter=0.1, orientation=0, bLength=0, 
           type="root", age=0, parent=0, id=0, creation_time=0, TTage=0, 
           growing=T, branching = T, order = 1, organ_id = 1, direction = 1, volume=0, length=0, surface=0)
  {
    if (!is.numeric(x) || !is.numeric(y) ||
        !all(is.finite(x)) || !all(is.finite(y))
    )
      
      stop("invalid coordinates")
    if (length(x) > 1 || length(y) > 1)
      stop("too big dimension of coordinates")
    nds = list(x = x, y = y, z = z, diameter=diameter, orientation=orientation, 
               bLength=bLength, type=type, age=age, parent=parent, id=id, creation_time=creation_time, 
               TTage=TTage, growing=growing, branching=branching, 
               order = order, organ_id=organ_id, direction=direction, volume = volume, length = length, surface=surface)
    class(nds) = "node"
    nds
  }





#' Create a new plant, together with its parameter set
#' @return the plant, with the parameter set
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @export
#' @examples
#' pl <- initializePlant()
#' 
initializePlant = 
  function(){

    # Initialize the plant
    plant <- plant()
    
    # Add the first nodes to the plant (seed, root and stem)
    plant <- addNodeToPlant(plant, node(1,0, parent=0, id=1, type="seed", organ_id = 1))
    plant <- addNodeToPlant(plant, node(1,-1, parent=1, id=2, type="root", 
                                        organ_id = 2, bLength = 1, 
                                        diameter = plant$params$root_diameter))
    plant <- addNodeToPlant(plant, node(1,1, parent=1, id=3, type="stem", 
                                        organ_id = 3, bLength = 1,
                                        diameter = plant$params$stem_diameter))

    # update the parameter sets
    plant$params$max_id <- 3
    plant$params$max_organ_id <- 3
    
    return(plant)
  }





#' Convert the plant to a tibble
#' @param pl = the plant, that should have been initialized
#' @return the plant as a tibble
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @export
#' @examples
#' rs <- plantToTibble(pl)
plantToTibble = 
  function(pl){
    return(
      pl$nodes %>% 
        transpose() %>%
        as.tibble() %>% 
        unnest()
    )
  }


# li <-       pl$nodes %>%
#   transpose() %>%
#   as.tibble()
# for(i in c(1:length(li))){
#   # print((pl$nodes[[i]]$type))
#   print(length(li[[i]]))
# }
# 
# pl$nodes[[6]]

#' Grow and develop the plant of a givn time step
#' @param pl = the plant, that should have been initialized
#' @param time = the time step to grow the plant
#' @return the plant and the updated parameter set
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @export
#' @examples
#' sim <- initializePlant()
#' pl <- sim$plant
#' params <- sim$params
#' 
growPlant = 
  function(pl, max_time){

    # Loopover the new time steps
    # pl <- initializePlant()
    
    for(i in c(1:max_time)){
      message(paste0("Time in current sim = ",i))
      # Update the time of the simulation
      pl$params$current_time <- pl$params$current_time + 1
      
      #////////////////////////////////////////////////////////////////////////
      # Create tillers (one stem + one root)
      if(pl$params$current_time == pl$params$tiller_timestamp & 
         pl$params$tiller_number < pl$params$tiller_max_number){
        
        pl$params$tiller_timestamp <- pl$params$tiller_timestamp + pl$params$tiller_timestamp # Update the time stamp
        pl$params$tiller_number = pl$params$tiller_number + 1 # Update the number of tillers
        
        # Add a tiller
        tiller <- node(x = runif(1, -2, 2), 
                       y = 1, 
                       parent = 1, 
                       id = pl$params$max_id+1, 
                       organ_id = pl$params$max_organ_id+1,
                       diameter = pl$params$stem_diameter,
                       type = "stem")
        
        pl <- addNodeToPlant(pl, tiller)
        pl$params$max_organ_id <- pl$params$max_organ_id+1
        
        # Add a root to the tiller
        pl <- addNodeToPlant(pl, node(x = tiller$x, 
                                      y = -0.5, 
                                      parent = tiller$id, 
                                      id = pl$params$max_id+1, 
                                      organ_id = pl$params$max_organ_id+1,
                                      diameter = pl$params$root_diameter,
                                      type = "root"))
        pl$params$max_organ_id <- pl$params$max_organ_id+1
      }      
      
      
      #////////////////////////////////////////////////////////////////////////
      # Grow the different nodes in the plant
      for(j in c(1:length(pl$nodes))){
        if(pl$nodes[[j]]$growing){
          growth <- T
          if(pl$nodes[[j]]$type == "stem" & pl$nodes[[j]]$TTage <= pl$params$stem_branching_age) growth = F
          if(growth){
            pl <- growNode(pl, pl$nodes[[j]], time=1)
            pl$nodes[[j]]$growing <- F
          }
        }
      }
      
      
      #////////////////////////////////////////////////////////////////////////
      # Branch the different nodes in the plant
      for(j in c(1:length(pl$nodes))){
        
        if(pl$nodes[[j]]$type == "root"){
          if(pl$nodes[[j]]$branching & pl$nodes[[j]]$age > pl$params$root_branching_age){
            pl <- branchNode(pl, pl$nodes[[j]])
            pl$nodes[[j]]$branching <- F
            pl$params$max_id <- pl$params$max_id + 1
            pl$params$max_organ_id <- pl$params$max_organ_id + 1
          }
          
        }else if(pl$nodes[[j]]$type == "stem"){
          if(pl$nodes[[j]]$branching & pl$nodes[[j]]$TTage > pl$params$stem_branching_age){
            pl <- branchNode(pl, pl$nodes[[j]])
            pl$nodes[[j]]$branching <- F
            pl$params$max_id <- pl$params$max_id + 1
            pl$params$max_organ_id <- pl$params$max_organ_id + 1
            pl$params$leaf_number <- pl$params$leaf_number + 1
            pl$params$leaf_orientation <- - pl$params$leaf_orientation
          }
        }
      }
      
      #////////////////////////////////////////////////////////////////////////
      # Update the global simulation parameters and data
      pl$params$flower_number <- 0
      pot_growth <- list(root=0, leaf=0, stem=0, flower=0)
      maintenance <- list(root=0, leaf=0, stem=0, flower=0)
      for(j in c(1:length(pl$nodes))){
        
        # The current node
        # no <- pl$nodes[[j]]
        
        # Update ages and degree days
        pl$nodes[[j]]$age <- pl$nodes[[j]]$age + 1
        pl$nodes[[j]]$TTage <- pl$nodes[[j]]$TTage + pl$environment$atmosphere$temperatures[i]
        
        # Compute the potential growth for the next time step, as well as the maintenance demand for each organ
        # if(pl$nodes[[j]]$growing){
          time = 1
          growth <- pl$nodes[[j]]$growing
          if(pl$nodes[[j]]$type == "stem" & 
             pl$nodes[[j]]$TTage <= pl$params$stem_branching_age) growth = F
          
          #//////////////////////////////////////////////////////////////////////
          # ROOT
          if(pl$nodes[[j]]$type == "root" & 
             (pl$nodes[[j]]$order == 1 | pl$nodes[[j]]$bLength < pl$params$root_max_lat_length)){
            if(growth){
              pot_growth$root <- pot_growth$root + time * pl$nodes[[j]]$volume * 
                                                        pl$params$root_dry_mass_ratio * 
                                                        pl$params$carbon_growth_efficiency
            }
            maintenance$root <- maintenance$root + time * pl$nodes[[j]]$volume * 
                                                        pl$params$root_dry_mass_ratio * 
                                                        pl$params$maintenance_respiration_rate * 
                                                        (pl$params$temp_coefficient^pl$environment$atmosphere$temperatures[2])
            
            #//////////////////////////////////////////////////////////////////////
            # STEM
          }else if(pl$nodes[[j]]$type == "stem" & 
                   pl$nodes[[j]]$TTage > pl$params$stem_branching_age){
            
            if(growth){
              pot_growth$stem <- pot_growth$stem + time * pl$nodes[[j]]$volume * 
                                                        pl$params$stem_dry_mass_ratio * 
                                                        pl$params$carbon_growth_efficiency 
            }
              maintenance$stem <- maintenance$stem + time * pl$nodes[[j]]$volume * 
                                                        pl$params$stem_dry_mass_ratio * 
                                                        pl$params$maintenance_respiration_rate*
                                                        (pl$params$temp_coefficient^pl$environment$atmosphere$temperatures[2])
            
            #////////////////////////////////////////////////////////////////////// 
            # LEAF
          }else if(pl$nodes[[j]]$type == "leaf" & 
                   pl$nodes[[j]]$bLength < pl$params$leaf_max_length){
            if(growth){
              pot_growth$leaf <- pot_growth$leaf + time * pl$nodes[[j]]$volume * 
                                                        pl$params$leaf_dry_mass_ratio * 
                                                        pl$params$carbon_growth_efficiency
            }
            maintenance$leaf <- maintenance$leaf + time * pl$nodes[[j]]$volume * 
                                                        pl$params$leaf_dry_mass_ratio * 
                                                        pl$params$maintenance_respiration_rate * 
                                                        (pl$params$temp_coefficient^pl$environment$atmosphere$temperatures[2])                                              
            
            #//////////////////////////////////////////////////////////////////////  
            # FLOWER
          }else if(pl$nodes[[j]]$type == "flower" & 
                   pl$params$flower_number < pl$params$flower_max_number){
            if(growth){
              pot_growth$flower <- pot_growth$flower + time * 0.5 * 
                                                      pl$params$flower_dry_mass_ratio * 
                                                      pl$params$carbon_growth_efficiency
            }
            maintenance$flower <- maintenance$flower + time * pl$nodes[[j]]$volume * 
                                                      pl$params$flower_dry_mass_ratio * 
                                                      pl$params$maintenance_respiration_rate * 
                                                      (pl$params$temp_coefficient^pl$environment$atmosphere$temperatures[2])
            
          }else{
            gr <- 0
          # }
        }
        
        
        if(pl$nodes[[j]]$type == "flower"){
          pl$params$flower_number  <- pl$params$flower_number +1
          if(pl$nodes[[j]]$diameter < pl$params$flower_max_diameter){
            pl$nodes[[j]]$diameter <- pl$nodes[[j]]$diameter + pl$params$flower_growth_rate
          }
        }
      }
      pl$params$potential_growth <- pot_growth
      
      temp <- list(x=1, y=2)
      
      #////////////////////////////////////////////////////////////////////////
      # Get the total length per organs type
      rs  <- plantToTibble(pl) %>% 
        dplyr::group_by(type) %>% 
        dplyr::summarize(length = sum(length), volume=sum(volume), surface=sum(surface)) %>% 
        gather(key=var, value=value, -type) %>% 
        mutate(time = pl$params$current_time)
      
      rs <- rbind(rs, data.frame(type = "root", var="growth_demand", 
                                 value = pot_growth$root, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "leaf", var="growth_demand", 
                                 value = pot_growth$leaf, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "stem", var="growth_demand", 
                                 value = pot_growth$stem, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "flower", var="growth_demand", 
                                 value = pot_growth$flower, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "total", var="growth_demand", 
                                 value = do.call(sum, pot_growth), time = pl$params$current_time))
      
      rs <- rbind(rs, data.frame(type = "root", var="maintenance_demand", 
                                 value = maintenance$root, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "leaf", var="maintenance_demand", 
                                 value = maintenance$leaf, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "stem", var="maintenance_demand", 
                                 value = maintenance$stem, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "flower", var="maintenance_demand", 
                                 value = maintenance$flower, time = pl$params$current_time))
      rs <- rbind(rs, data.frame(type = "total", var="maintenance_demand", 
                                 value = do.call(sum, maintenance), time = pl$params$current_time))
      
      rs <- rbind(rs, data.frame(type = "total", var="total_demand", 
                                 value = do.call(sum, maintenance) + do.call(sum, pot_growth), time = pl$params$current_time))
    
      
      
      carbon_supply <- 0
      if(length(rs$value[rs$type == "leaf" & rs$var == "surface"]) > 0){
        carbon_supply <- farquhar(surface = rs$value[rs$type == "leaf" & rs$var == "surface"]) / 10  # Divide by 10 to be of the same order than the demand. Need to fix this and find where it is coming from
      }
      print(carbon_supply )
      rs <- rbind(rs, data.frame(type = "leaf", var="photosynthesis", value = carbon_supply, time = pl$params$current_time))
      
      
      pl$data <- rbind(pl$data, rs)
      
      
    }
    return(pl)
  }






#' Add a node to an existing plant
#' @param ro = the current plant
#' @param no = the current node
#' @return the plant, with the added node
#' @author Guillaume Lobet - g.lobet@fz-juelich.de
#' @export
#' @examples
#' n <- node(1, 1)
#' p <- plant()
#' r <- addNodeToPlant(r, n)
addNodeToPlant = 
  function(pl, no)
  {
    
    if(class(no) != "node" || class(pl) != "plant" )
      stop("need objects of class node and plant")
    pl$nodes[[length(pl$nodes) + 1]] <- no
    pl$params$max_id <- pl$params$max_id + 1
    
    pl
  }




#' Grow the node
#' @param pl : the plant to grow
#' @param no : the node to grow
#' @param time : the time step of the growth period (typically one day)
#' @return null
#' @export
#' @examples
growNode = 
  function(pl, no, time = 1){
    
    #//////////////////////////////////////////////////////////////////////
    # ROOT
    if(no$type == "root" & (no$order == 1 | no$bLength < pl$params$root_max_lat_length)){
      gr <- time * (pl$params$root_growth_rate / no$order)

        if(no$order == 1){
          nx <-  no$x * runif(1, 0.9, 1.1)
          ny <- no$y - gr
        }else{
          nx <-  no$x  + (gr * no$direction)
          ny <- no$y - (pl$params$root_tropism * runif(1, 0.8, 1.2))
        }
       
        new_node <- node(x = nx,
                         y = ny,
                         id = pl$params$max_id + 1,
                         parent = no$id,
                         organ_id = no$organ_id,
                         order = no$order,
                         direction = no$direction,
                         branching = no$branching,
                         diameter = no$diameter,
                         bLength = no$bLength + gr,
                         length = gr,
                         type = "root",
                         creation_time = pl$params$current_time)
        
        new_node$volume <-  new_node$length * (new_node$diameter/2)^2 * pi
        new_node$surface <-  new_node$length * (new_node$diameter/2) * pi * 2
        
        pl <- addNodeToPlant(pl, new_node)
        no <- new_node
      
    #//////////////////////////////////////////////////////////////////////
    # STEM
    }else if(no$type == "stem" & no$TTage > pl$params$stem_branching_age){
      
      # Vegetative stage
      if(pl$params$leaf_number < pl$params$leaf_max_number-1){
        gr <- time * pl$params$stem_growth_rate * no$order
        new_node <- node(x = no$x * runif(1, 0.8, 1.2),
                         y = no$y + gr,
                         id = pl$params$max_id + 1,
                         parent = no$id,
                         organ_id = no$organ_id,
                         bLength = no$bLength + gr,
                         length = gr,
                         diameter = no$diameter,
                         type = "stem",
                         creation_time = pl$params$current_time)
        
        new_node$volume <-  new_node$length * (new_node$diameter/2)^2 * pi
        new_node$surface <-  new_node$length * (new_node$diameter/2) * pi * 2
        
      # Reproductive stage -> The stem mareistem become reproductive and a flower
      }else{
        gr <- time * pl$params$stem_growth_rate * no$order
        new_node <- node(x = no$x * runif(1, 0.8, 1.2),
                         y = no$y + gr,
                         id = pl$params$max_id + 1,
                         parent = no$id,
                         organ_id = no$organ_id,
                         bLength = no$bLength + gr,
                         length = gr,
                         diameter = no$diameter,
                         type = "flower",
                         creation_time = pl$params$current_time)
        
        new_node$volume <-  new_node$length * (new_node$diameter/2)^2 * pi
        new_node$surface <-  new_node$length * (new_node$diameter/2) * pi * 2
      }
      pl <- addNodeToPlant(pl, new_node)
     
    #////////////////////////////////////////////////////////////////////// 
    # LEAF
    }else if(no$type == "leaf" & no$bLength < pl$params$leaf_max_length){
      gr <- time * (pl$params$leaf_growth_rate)

      nx <-  no$x  + (gr * no$direction)
      ny <- no$y + (pl$params$leaf_tropism * runif(1, 0.5, 1.5))
      
      new_node <- node(x = nx,
                       y = ny,
                       id = pl$params$max_id + 1,
                       parent = no$id,
                       organ_id = no$organ_id,
                       order = no$order,
                       bLength = no$bLength + gr,
                       length = gr,
                       direction = no$direction,
                       branching = no$branching,
                       diameter = no$diameter,
                       type = "leaf",
                       creation_time = pl$params$current_time)
      
      new_node$volume <-  new_node$length * new_node$diameter * pl$params$leaf_thickness
      new_node$surface <-  new_node$length * new_node$diameter
      
      pl <- addNodeToPlant(pl, new_node)
    
    #//////////////////////////////////////////////////////////////////////  
    # FLOWER
    }else if(no$type == "flower" & pl$params$flower_number < pl$params$flower_max_number){
      gr <- 0.5
      nx <-  no$x * runif(1, 0.8, 1.2)
      ny <- no$y + gr
      new_node <- node(x = nx,
                       y = ny,
                       id = pl$params$max_id + 1,
                       parent = no$id,
                       organ_id = no$organ_id,
                       order = no$order,
                       bLength = no$bLength + gr,
                       length = gr,
                       direction = no$direction,
                       branching = no$branching,
                       diameter = no$diameter,
                       type = "flower",
                       creation_time = pl$params$current_time)
      
      new_node$volume <-  (new_node$diameter/2)^3 * pi * 4/3
      new_node$surface <-  (new_node$diameter/2)^2 * pi * 4
      
      pl <- addNodeToPlant(pl, new_node)
      
    }else{
      new_node <- NULL
    }
    
    return(pl)
  }




#' Euclidian distance btween two points
#' @param xy = coordinates of both nodes
#' @return the distance
#' @export
#' @examples
eudist <- 
  function(x1, y1, x2, y2){
    return(sqrt((x2-x1)^2 + (y2-y1)^2))
  }

#' Branch the node
#' @param pl : the plant to develop
#' @param no : the node to branch
#' @param params: the parameter set
#' @return plant
#' @export
#' @examples
branchNode = 
  function(pl, no){
    
    #/////////////////////////////////////////////////////////////////
    # ROOT > CREATE A LATERAL ROOT
    if(no$type == "root"){
      direction <- sample(c(-1, 1), 1)
      new_node <- node(x = no$x + (direction/2),
                       y = no$y - (pl$params$root_tropism * runif(1, 0.5, 1.5)),
                       id = pl$params$max_id + 1,
                       direction = direction,
                       organ_id = pl$params$max_organ_id + 1,
                       parent = no$id,
                       diameter = no$diameter * pl$params$root_diameter_ratio,
                       branching = F,
                       type = "root",
                       order = no$order +1,
                       creation_time = pl$params$current_time)
      new_node$length <- eudist(new_node$x, new_node$y, no$x, no$y)
      new_node$bLength <- new_node$length
      new_node$volume <-  new_node$length * (new_node$diameter/2)^2 * pi
      
      
    #/////////////////////////////////////////////////////////////////
    # STEM  > CREATE A LEAF
   } else if(no$type == "stem"){
     new_node <- node(x = no$x + (pl$params$leaf_orientation/2),
                      y = no$y + (0.4 * runif(1, 0.5, 1.5)),
                      id = pl$params$max_id + 1,
                      direction = pl$params$leaf_orientation,
                      organ_id = pl$params$max_organ_id + 1,
                      parent = no$id,
                      diameter = pl$params$leaf_width,
                      branching = F,
                      type = "leaf",
                      order = no$order +1,
                      creation_time = pl$params$current_time)
     
     new_node$length <- eudist(new_node$x, new_node$y, no$x, no$y)
     new_node$bLength <- new_node$length
     new_node$volume <-  new_node$length * new_node$diameter * pl$params$leaf_thickness
   }else{
      new_node <- NULL
    }
    
    if(!is.null(new_node)) pl <- addNodeToPlant(pl, new_node)
    return(pl)
  }





#' Plot the plant
#' @param x object of class plant
#' @param ... plot options
#' @keywords rsml
#' @import ggplot2
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import cowplot
#' @return null
#' @export
#' @examples
#' # Plot 2D plant
#' data(lupin)
#' plot(lupin)
plot.plant = function(x, colour = "type", seg=T, point=T){
  
  rs <- plantToTibble(x) %>% 
    mutate(x1 = x) %>% 
    mutate(y1 = y)
  
  rs$var <- rs[[colour]]
  if(colour == "organ_id" | colour == "order") rs$var = factor(rs$var)

  for(i in c(2:nrow(rs))){
    rs$x1[i] <- rs$x[rs$id == rs$parent[i]]
    rs$y1[i] <- rs$y[rs$id == rs$parent[i]]
  }


  pl1 <- ggplot(rs, aes(x, y, colour=var)) + 
    coord_fixed()+
    guides(colour=guide_legend(title=colour)) + 
    geom_hline(yintercept = 0, colour="brown")
  
  pl1 <- pl1 + geom_segment(aes(xend=x1, yend=y1), size=0.5) 
  # pl1 <- pl1 + geom_point() 
  pl1 <- pl1+ geom_point(data = rs[rs$type == "flower",], aes(size=diameter), alpha = 0.5)
  
  pl1
}



#' Farquar model for photosynthesis
#' From http://biocycle.atmos.colostate.edu/shiny/photosynthesis/#farquhar.R
#' @param V.max = maximum rubisco-limited rate in micromoles per (m^2 sec)
#' @param J.max = maximum light-limited rate in micromoles per (m^2 sec)
#' @param APAR = absorbed photosynthetically-active radiation in micromoles per (m^2 sec)
#' @param c.i = intercellular CO2 partial pressure in Pascals (roughly ppm/10)
#' @param surface = leaf surface (cm2)
#' @param time = time (days)
#'
#'
#'
farquhar <- function(V.max=50, J.max=100, APAR=500, c.i=30, surface=1, time=1) {
  
  # Model inputs:
  # V.max = maximum rubisco-limited rate in micromoles per (m^2 sec)
  # J.max = maximum light-limited rate in micromoles per (m^2 sec)
  # APAR = absorbed photosynthetically-active radiation in micromoles per (m^2 sec)
  # c.i = intercellular CO2 partial pressure in Pascals (roughly ppm/10)
  
  time.sec = time * 3600 * 24
  surface.m = surface / 10000
  
  # Some local parameters we need
  p.sfc <- 101325  # surface air pressure (Pascals)
  gamma <- 3. # CO2 compensation point (Pascals)
  O.i <- 0.209 * p.sfc  # oxygen partial pressure in chloroplast
  K.c <- 30 # Michaelis-Menten constant for carboxylation (Pascals)
  K.o <- 30000 # Michaelis-Menten constant for oxidation (Pascals)
  
  # Solution of quadratic (Bonan 17.8)
  a <- 0.7
  b <- -(J.max + 0.385 * APAR)
  c <- 0.385 * J.max * APAR
  J.1 <- (-b + sqrt(b^2 - 4 * a * c) ) / (2 * a)
  J.2 <- (-b - sqrt(b^2 - 4 * a * c) ) / (2 * a)
  J <- min(J.1, J.2)
  
  # Rubisco-limited rate of photosynthesis
  w.c <- V.max * (c.i - gamma) / (c.i + K.c * (1 + O.i/K.o))  # Bonan 17.6
  
  # Light-limited rate of photosynthesis
  w.j <- J * (c.i - gamma) / (4 * (c.i + 2 * gamma))            # Bonan 17.7
  
  # Sink-limited rate of photosynthesis
  w.s <- V.max / 2
  
  # Dark respiration 
  R.d <- 0.015 * V.max
  
  # Net assimilation in µmol CO₂ m-2 s-1
  A.n <- min(w.c, w.j, w.s)-R.d
  
  # Net carbon fixed
  mol.mass.co2 <- 44  # molecular mass of CO₂
  tot_carbon <- (A.n / 1000) * time.sec * surface.m  * mol.mass.co2
  
  return(tot_carbon)
}

