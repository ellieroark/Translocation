################################
## Survey data simulation-- Guguan 2022
## 
## author: Ellie Roark
## created: 14 March 2022
## last modified: 14 March 2022
## 
## inputs: *Guguan2106.csv-- Data from guguan 2016 surveys
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

# read in data
gu16 <- read.csv("./Guguan2016.csv", stringsAsFactors = FALSE)
