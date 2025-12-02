#### Some fiddling around to determine good phenology chapter ####

library(tidyverse)
library(sf)
library(rnaturalearth)
library(USAboundaries)
library(USAboundariesData)
library(AOI)
library(paletteer)
library(usmap)
library(USA.state.boundaries)
library(terra)
library(stringr)
library(ggspatial)
library(purrr)

#####
### Set Working Directory
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

### Read in all data. Does not include species where more than 99% of all observations were 0
SBC_all_data <- read.csv(file = "SBC_all_data.csv") %>% 
  #read.csv("/Volumes/mpward/SBC_analyses/SBC_all_data.csv") %>% 
  select(-c(X)) %>% 
  rename(county = coun)

### Determine which species have been observed the most in the past 50 years of the count
head <- SBC_all_data %>% 
  group_by(Common_Name) %>% 
  summarise(Count = sum(Count,na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(Count)) %>% 
  head(50)

