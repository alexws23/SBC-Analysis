library(tidyverse)
library(visdat)

### Set working directory
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

### Read in data
SBC_GDD <- read.csv(file = "SBC_GDD.csv") %>% 
  select(-c(X)) %>%  #Drop extraneous column
  filter(countyyear != "Champaign 2024",  #Remove Champaign 2024 since it is missing data
         county != "Douglas") #remove douglas county since it has no data

average_gdd <- SBC_GDD %>% 
  group_by(county) %>% 
  summarise(mean = mean(GGD_1_25))

annual_gdd <- SBC_GDD %>% 
  group_by(county) %>% 
  unique(GGD_1_25)
  