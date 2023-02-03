################################
## Guguan exploratory data analysis 2022
## 
## author: Ellie Roark
## created: 11 Oct 2022
## last modified: 11 Oct 2022
## 
## inputs: *Guguan_forest_bird_surveys_2022_ER.csv
##
##
## outputs: *
##            
## TODO: * 
################################

library(tidyverse)
library(gt)
library(tictoc)
library(readxl)
library(patchwork)

# Read first sheet from excel- 2022 data
gug_dat22all <- readxl::read_xlsx("Data/Guguan Forest Bird Surveys 2022_QCd_ER.xlsx", sheet = 1)

# Read first sheet from excel- 2016 data
gug_dat1 <- readxl::read_xlsx("./Data/Guguan forest bird surveys 2016.xlsx", sheet = 1)

# Clean 2016 data
gug_dat1 <- gug_dat1 %>% 
  rename(
    Transect = `...1`,          # Name first 2 cols
    Point = `...2`
  )  %>% 
  .[-nrow(.),] %>%              # Remove last row with totals
  fill(Transect) %>%            # Fill in blanks for transect
  mutate(
    across(MIST:BRWE,
           ~replace_na(.x, 0)), # Change remaining NAs (counts) to 0
    Transect = as.numeric(gsub("\\D*", "", Transect))
  ) %>%           
  pivot_longer(                 # Pivot longer to make tidy
    MIST:BRWE,
    names_to = "Species_Code",
    values_to = "Count"
  )

# Read second sheet from excel- 2016 data
gug_dat2 <- readxl::read_xlsx("Data/Guguan forest bird surveys 2016.xlsx", sheet = 2) %>%
  rename(Type = `Forest OR Grassland`)

# Join habitat type to aggregated counts from sheet 1
gug_dat1 <- gug_dat1 %>%
  left_join(
    gug_dat2 %>% 
      select(Transect, Point, Type) %>%
      unique(),
    by = c("Transect", "Point")
  )


# Clean 2022 data
gug_dat22 <- gug_dat22all %>% 
  rename(Type = `Forest OR Grassland`) %>% 
  rename(Species_Code = Species)

## remove Fruit Bat observations
gug_dat22 <- gug_dat22[which(gug_dat22$Species_Code != "MAFB"), ]

## create a count column that groups identical rows
gug_dat22 <- gug_dat22 %>%
  group_by(across(.cols=everything())) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  distinct()

# gug_dat22 now contains only separate records for each species recorded at each
# distance at each location.


## Mean detections per station by type- 2022------------------------------------
# first aggregate by total detections per species at each station
sp_st <- gug_dat22 %>%
  group_by(Species_Code, TransStat) %>%
  summarise(Count = sum(Count)) %>%
  ungroup()

## add in nondetection data (0 counts) for species not present at a point
sp_st <- pivot_wider(sp_st, id_cols = TransStat, names_from = Species_Code, 
                     values_from = Count)

sp_st[is.na(sp_st)] <- 0

sp_st <- pivot_longer(sp_st, cols = c(2:9), names_to = "Species_Code")

sp_st <- rename(sp_st, Count = value)

gug_dat22 <- gug_dat22[, c("Transect", "TransStat", "Point", "Type")]
gug_dat22 <- unique(gug_dat22)

sp_st <- left_join(sp_st, gug_dat22, by = "TransStat")


# Points per transect by type-- 2022
types <- sp_st %>% 
  select(Transect, Point, Type) %>%
  unique() %>%
  group_by(Transect, Type) %>%
  summarize(n = n()) %>%
  pivot_wider(id_cols = Transect,
              names_from = Type,
              names_prefix = "n",
              values_from = n,
              values_fill = 0)

sp_st$Points <- c("test")
sp_st$Points[which(sp_st$Type == "F")] <- sum(types$nF)
sp_st$Points[which(sp_st$Type == "G")] <- sum(types$nG)
sp_st$Points <- as.numeric(sp_st$Points)


md_22 <- sp_st %>%
  group_by(Species_Code, Type) %>%
  summarise(Detections = sum(Count), Points = n(), Mean_Det = Detections/Points, 
            Med_Det = median(Count), IQRange = IQR(Count))

## end mean detections by station point 2022------------------------------------

##### Mean detections per station by type- 2016--------------------------------

# Points per transect by type-- 2016
types16 <- gug_dat1 %>% 
  select(Transect, Point, Type) %>%
  unique() %>%
  group_by(Transect, Type) %>%
  summarize(n = n()) %>%
  pivot_wider(id_cols = Transect,
              names_from = Type,
              names_prefix = "n",
              values_from = n,
              values_fill = 0)

gug_dat1$Points <- c("test")
gug_dat1$Points[which(gug_dat1$Type == "F")] <- sum(types16$nF)
gug_dat1$Points[which(gug_dat1$Type == "G")] <- sum(types16$nG)
gug_dat1$Points <- as.numeric(gug_dat1$Points)


md_16 <- gug_dat1 %>%
  group_by(Species_Code, Type) %>%
  summarise(Detections = sum(Count), Points = n(), Mean_Det = Detections/Points, 
            Med_Det = median(Count), IQRange = IQR(Count))

## end mean detections per station point 2016----------------------------------


# Merge 2016 data and 2022 data
##prep for join by changing column names
md_22 <- md_22 %>% 
  rename(Detections2022 = Detections, 
         Mean_Det2022 = Mean_Det, 
         nPoints2022 = Points, 
         Med_Det2022 = Med_Det,
         IQRange2022 = IQRange)
md_16 <- md_16 %>%
  rename(Detections2016 = Detections, 
         Mean_Det2016 = Mean_Det, 
         nPoints2016 = Points, 
         Med_Det2016 = Med_Det,
         IQRange2016 = IQRange)
mdb <- full_join(md_16, md_22, by = c("Species_Code", "Type"))

write.csv(mdb, file = "./Outputs/mean_detections_per_st_var.csv")

# Type of each station in each transect- 2022
sp_st %>% 
  select(Transect, Point, Type) %>%
  unique()

#bar plot-- mean sp per point
md1 <- ggplot(mdb, aes(Species_Code, Mean_Det2016, fill = Type)) + 
  geom_col() 
#bar plot-- mean sp per point
md2 <- ggplot(mdb, aes(Species_Code, Mean_Det2022, fill = Type)) + 
  geom_col()

md3 <- md1 + md2 + 
  plot_layout(guides = 'collect') + 
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme()

#t <- ggplot(mdb, aes(Species_Code, Mean_Det2016, fill = ))


# ### get standard deviation for detections for each species- 2022 data-----------
# # SLOPPY CODE!!!! Fix this eventually
# ## COKI Grassland
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "COKI" & sp_st$Type == "G"), c("Count")])))
# z <- sum(types$nG)-length(k)
# k <- append(k, integer(z))
# v <- sd(k)
# mdb[which(mdb$Species_Code == "COKI" & mdb$Type == "G"), c("SD")] <- v
# 
# ## MAFD Forest
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "MAFD" & sp_st$Type == "F"), c("Count")])))
# z <- sum(types$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MAFD" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MAFD Grassland
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "MAFD" & sp_st$Type == "G"), c("Count")])))
# z <- sum(types$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MAFD" & mdb$Type == "G"), c("SD")] <- v
# 
# ## MIHO Forest
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "MIHO" & sp_st$Type == "F"), c("Count")])))
# z <- sum(types$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIHO" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MIHO Grassland
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "MIHO" & sp_st$Type == "G"), c("Count")])))
# z <- sum(types$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIHO" & mdb$Type == "G"), c("SD")] <- v
# 
# ## MIME Forest
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "MIME" & sp_st$Type == "F"), c("Count")])))
# z <- sum(types$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIME" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MIST Forest
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "MIST" & sp_st$Type == "F"), c("Count")])))
# z <- sum(types$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIST" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MIST Grassland
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "MIST" & sp_st$Type == "G"), c("Count")])))
# z <- sum(types$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIST" & mdb$Type == "G"), c("SD")] <- v
# 
# ## RUFA Forest
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "RUFA" & sp_st$Type == "F"), c("Count")])))
# z <- sum(types$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "RUFA" & mdb$Type == "F"), c("SD")] <- v
# 
# ## RUFA Grassland
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "RUFA" & sp_st$Type == "G"), c("Count")])))
# z <- sum(types$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "RUFA" & mdb$Type == "G"), c("SD")] <- v
# 
# ## TIMO Forest
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "TIMO" & sp_st$Type == "F"), c("Count")])))
# z <- sum(types$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "TIMO" & mdb$Type == "F"), c("SD")] <- v
# 
# ## WTGD Forest
# k <- as.numeric(unlist((sp_st[which(sp_st$Species_Code == "WTGD" & sp_st$Type == "F"), c("Count")])))
# z <- sum(types$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "WTGD" & mdb$Type == "F"), c("SD")] <- v
# ## end 2022 standard deviations-------------------------------------------------
# 
# 
# ### get standard deviation for detections for each species- 2016 data-----------
# # SLOPPY CODE!!!! Fix this eventually
# ## COKI Grassland
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "COKI" & gug_dat1$Type == "G"), c("Count")])))
# z <- sum(types16$nG)-length(k)
# k <- append(k, integer(z))
# v <- sd(k)
# mdb[which(mdb$Species_Code == "COKI" & mdb$Type == "G"), c("SD")] <- v
# 
# ## MAFD Forest
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "MAFD" & gug_dat1$Type == "F"), c("Count")])))
# z <- sum(types16$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MAFD" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MAFD Grassland
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "MAFD" & gug_dat1$Type == "G"), c("Count")])))
# z <- sum(types16$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MAFD" & mdb$Type == "G"), c("SD")] <- v
# 
# ## MIHO Forest
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "MIHO" & gug_dat1$Type == "F"), c("Count")])))
# z <- sum(types16$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIHO" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MIHO Grassland
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "MIHO" & gug_dat1$Type == "G"), c("Count")])))
# z <- sum(types16$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIHO" & mdb$Type == "G"), c("SD")] <- v
# 
# ## MIME Forest
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "MIME" & gug_dat1$Type == "F"), c("Count")])))
# z <- sum(types16$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIME" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MIST Forest
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "MIST" & gug_dat1$Type == "F"), c("Count")])))
# z <- sum(types16$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIST" & mdb$Type == "F"), c("SD")] <- v
# 
# ## MIST Grassland
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "MIST" & gug_dat1$Type == "G"), c("Count")])))
# z <- sum(types16$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "MIST" & mdb$Type == "G"), c("SD")] <- v
# 
# ## RUFA Forest
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "RUFA" & gug_dat1$Type == "F"), c("Count")])))
# z <- sum(types16$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "RUFA" & mdb$Type == "F"), c("SD")] <- v
# 
# ## RUFA Grassland
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "RUFA" & gug_dat1$Type == "G"), c("Count")])))
# z <- sum(types16$nG)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "RUFA" & mdb$Type == "G"), c("SD")] <- v
# 
# ## TIMO Forest
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "TIMO" & gug_dat1$Type == "F"), c("Count")])))
# z <- sum(types16$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "TIMO" & mdb$Type == "F"), c("SD")] <- v
# 
# ## WTGD Forest
# k <- as.numeric(unlist((gug_dat1[which(gug_dat1$Species_Code == "WTGD" & gug_dat1$Type == "F"), c("Count")])))
# z <- sum(types16$nF)-length(k)
# k <- append(k, integer(z))
# sd(k)
# v <- sd(k)
# mdb[which(mdb$Species_Code == "WTGD" & mdb$Type == "F"), c("SD")] <- v
# 
# 
# mdb[is.na(mdb)] <- 0
