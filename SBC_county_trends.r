##### Species Abundance Per County #####

library(tidyverse)
library(sf)
library(rnaturalearth)
library(USAboundaries)
library(USAboundariesData)
library(AOI)
library(paletteer)

### Set Working Directory
setwd("~/Ward Lab/SBC-Analysis")

### Read in all data. Does not include species where more than 99% of all observations were 0
SBC_all_data <- read.csv("/Volumes/mpward/SBC_analyses/SBC_all_data.csv") %>% 
  select(-c(X)) %>% 
  rename(county = coun)

unique(SBC_all_data$county)

counties <- c("Cook", "Champaign", "Peoria", "Madison","Moultrie","Effingham","Whiteside","Washington","Vermilion","Massac","Hancock","DuPage","Boone")

species <- c("White-throated Sparrow", "Yellow-rumped Warbler", "White-crowned Sparrow")

SBC_all_data %>% 
  filter(Common_Name %in% species,
         county %in% counties) %>%
  group_by(county, year, class) %>% 
  summarise(GGD_1_50 = mean(GGD_1_50),
            Count = mean(Count)) %>% 
  ggplot(aes(x = GGD_1_50, y = Count, color = class)) +
  geom_point() +
  geom_smooth(method = "gam") +
  facet_wrap(~county)

### Map
eqdc <- '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

states <- USAboundaries::us_states(resolution = "low") %>% 
  filter(state_abbr %in% c("IL")) %>% 
  st_transform(eqdc)

IL_counties <- USAboundaries::us_counties(states = "Illinois") %>% 
  select(-c(state_name)) %>% 
  rename(county = name) %>% 
  mutate(county = ifelse(county == "LaSalle", "La Salle", county))

WTSP_1980 <-  SBC_all_data %>% 
  filter(year < 1990,
         Common_Name == "Yellow-rumped Warbler") %>% 
  drop_na(Count) %>% 
  group_by(county) %>% 
  summarise(Count = mean(Count))
  
WTSP_1 <- left_join(IL_counties, WTSP_1980,by = join_by(county))

ggplot() +
  geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = WTSP_1, aes(fill = Count), show.legend = T) +
  scale_fill_viridis_c(option = "A")+
  theme_void()

#########
WTSP_1990 <-  SBC_all_data %>% 
  filter(year > 1990,
         year <2010,
         Common_Name == "White-throated Sparrow") %>% 
  drop_na(Count) %>% 
  group_by(county) %>% 
  summarise(Count = mean(Count))

WTSP_3 <- left_join(IL_counties, WTSP_1990,by = join_by(county))

ggplot() +
  geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = WTSP_3, aes(fill = Count), show.legend = T) +
  scale_fill_viridis_c(option = "A")+
  theme_void()

######
WTSP_2010 <-  SBC_all_data %>% 
  filter(year > 2010,
         Common_Name == "Yellow-rumped Warbler") %>% 
  drop_na(Count) %>% 
  group_by(county) %>% 
  summarise(Count = mean(Count))

WTSP_2 <- left_join(IL_counties, WTSP_2010,by = join_by(county))

ggplot() +
  geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = WTSP_2, aes(fill = Count), show.legend = T) +
  scale_fill_viridis_c(option = "A")+
  theme_void()

WTSP <- left_join(WTSP_2010,WTSP_1980, join_by(county))

WTSP <- WTSP %>% 
  mutate(diff = Count.x - Count.y) %>% 
  mutate(category = ifelse(diff >0, "Increase", "Decrease"))

WTSP_diff <- left_join(IL_counties, WTSP,by = join_by(county))

ggplot() +
  geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = WTSP_diff, aes(fill = category), show.legend = T) +
  scale_fill_viridis_d(option = "F")+
  theme_void()
