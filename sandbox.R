
library(tidyverse)
library(cowplot)

source("contructor_functions.R", echo = F)

pl <- initializePlant()
myPlant <- initializePlant()
myPlant$params$leaf_max_number <- 10

myPlant <- growPlant(myPlant, 30)
plot(myPlant)
myPlant <- growPlant(myPlant, 10)
plot(myPlant)
myPlant <- growPlant(myPlant, 10)
plot(myPlant)


rs <- plantToTibble(myPlant)
rs %>% filter(type== "stem")

ggplot(myPlant$data, aes(time, value, colour=type)) +
  geom_line() + 
  facet_wrap(~var, scales="free")

rs <- as.tibble(myPlant$environment$atmosphere) %>% 
  gather(key=param, value=value, -days)

ggplot(rs, aes(days, value, colour=param)) + 
  geom_line()


myPlant$params$potential_growth
