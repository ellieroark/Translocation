################################
## Species-area relationship plots for the Northern Mariana Islands
## 
## author: Ellie Roark
## created: 22 Sept 2021
## last modified: 22 Sept 2021
## 
## inputs: *terrestrial_bird_species_by_island.csv -- file with bird species
##            listed by island they occupy, historically and presently
##
##         
## outputs: *
##            
## TODO: * 
################################


library(Hmisc)
library(tidyverse)
library(lubridate)

#windows wd
setwd("X:/EllieRoark/R/Translocation")

#linux wd
#setwd("~/Dropbox/Ellie Roark/CNMI_DFW/EllieRoark/R/Translocation")

birdisl <- read.csv("./terrestrial_bird_species_by_island.csv", 
                    stringsAsFactors = FALSE)
birdisl <- birdisl[,1:4]

isldat <- read.csv("./island_area.csv", stringsAsFactors = FALSE)
isldat <- rename(isldat, island = ï..island)

## add up number of bird species on each island
birdisl_sum <- count(birdisl, island)

birdisl_sum <- birdisl %>% 
  select(species, island, study) %>%
  unique() %>%
  group_by(study, island) %>% 
  summarise(n())

birdisl_sum <- rename(birdisl_sum, num_sp = `n()`)

sp_isl <- left_join(birdisl_sum, isldat)

## create colour-blind friendly palette for use in plots
cbPalette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
               "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

## plot number of species by total island land area
ggplot(sp_isl[which(sp_isl$study != "assumed post-translocation"), ], 
       aes(x = land_area_km2, y = num_sp)) + 
  stat_smooth(method="lm", formula=y~x, se=F) + 
  geom_point(size = 4, aes(color= island)) + 
  scale_x_log10(limits = c(1, 130)) +
  scale_y_log10(limits = c(1, 25)) +
  scale_colour_manual(values = cbPalette) +
  theme_bw() + 
  xlab("Land Area (sq. km)") +
  ylab("Number of Species") +
  ggtitle("Total Land Area vs. Terrestrial Bird Species") + 
  geom_point(data = sp_isl[which(sp_isl$study == "assumed post-translocation"), ], 
             size = 4, color = "black")



## plot number of species by suitable habitat
ggplot(sp_isl, aes(x = suitable_habitat_km2, y = num_sp, color = island, shape = study)) + 
  geom_point(size = 4) +
  scale_x_log10(limits = c(1, 130)) +
  scale_y_log10(limits = c(1, 25)) + 
  ggtitle("suitable habitat vs. terrestrial bird sp") 
  






