################################
## Saipan Reed-Warbler (SRWA or NIRW) morphometric data exploration
## 
## author: Ellie Roark
## created: 17 August 2022
## last modified: 17 August 2022
## 
## inputs: *Saipan morphology.csv-- data from Robert Craig with bird morphometrics
##          from 1988-1992. 
##
##         
## outputs: *saipan_NIRW_morphology_RCraigdata.csv-- morphology for only NIRW
##          *sexmorph_plot1-- plot of wing and tail lengths by sex for NIRW
##            
## TODO: * 
################################


library(Hmisc)
library(tidyverse)

## read in R.Craig morphology data

morph <- read_csv("./Saipan morphology.csv")

## subset to only NIRW records
morph_rw <- morph[which(morph$Species == "NIRW"), ]

## plot wing and tail lengths by sex
sexmorph_plot1 <- ggplot(data = morph_rw, aes(tail, wing)) + 
  geom_point(size = 4, aes(shape = sex)) +
  scale_shape_manual(values = c(1,3)) +
  theme_bw() +
  ggtitle("Morphometrics for Saipan Reed-warbler\n1988-1992, data from R. Craig")


## write out NIRW morphology data from R Craig to .csv to share with PBC
write.csv(morph_rw, file = "./saipan_NIRW_morphology_RCraigdata.csv")
