library(tidyverse)
library(visdat)

setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

SBC_GDD <- read.csv(file = "SBC_GDD.csv") %>% 
  select(-c(X))







SBC %>% 
  filter(Common_Name == "Sora") %>% 
  group_by(year) %>%
  drop_na() %>% 
  summarise(
    Count = mean(Count)
    #avg_tmean_100 = mean(avg_tmean_100)
  ) %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = Count)) +
  geom_smooth(aes(y = Count), method = "loess") +
  theme_minimal()

Model <-  glm(Count ~ avg_tmean_25+ GGD_1_25 + GGD_2_25 + total_precip_25, family = "gaussian", data = merge %>% filter(Common_Name == "Dark-eyed Junco"))
summary(Model)
