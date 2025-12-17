##### Species Abundance Per County #####

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

### Set Working Directory
setwd("~/Ward Lab/SBC-Analysis")

### Read in all data. Does not include species where more than 99% of all observations were 0
SBC_all_data <- read.csv(file = "Data/SBC_all_data.csv") %>% 
  #read.csv("/Volumes/mpward/SBC_analyses/SBC_all_data.csv") %>% 
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

#states <- USAboundaries::us_states(resolution = "low") %>% 
  #filter(state_abbr %in% c("IL")) %>% 
  #st_transform(eqdc)

#IL_counties <- USAboundaries::us_counties(states = "Illinois") %>% 
  #select(-c(state_name)) %>% 
  #rename(county = name) %>% 
  #mutate(county = ifelse(county == "LaSalle", "La Salle", county))

IL_counties <- usmap::us_map(regions = "counties") %>% 
  filter(abbr == "IL") %>% 
  mutate(county = ifelse(county == "LaSalle County", "La Salle", county)) %>% 
  st_transform(eqdc)

IL_counties$county <- gsub(" County","", IL_counties$county)

IL <- usmap::us_map(regions = "states") %>% 
  filter(abbr == "IL") %>% 
  st_transform(eqdc)

GDD_1980 <-  SBC_all_data %>% 
  filter(year < 1990,
         Common_Name == "Yellow-rumped Warbler") %>% 
  drop_na(GGD_1_50) %>% 
  group_by(county) %>% 
  summarise(GGD_1_50 = mean(GGD_1_50))
  
GDD_1 <- left_join(IL_counties, GDD_1980,by = join_by(county))

ggplot() +
  #geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = GDD_1, aes(fill = GGD_1_50), show.legend = T) +
  scale_fill_viridis_c(option = "A")+
  theme_void() +
  labs(title = "1973-1989")

#########
GDD_1990 <-  SBC_all_data %>% 
  filter(year >= 1990,
         year <2010,
         Common_Name == "White-throated Sparrow") %>% 
  drop_na(GGD_1_50) %>% 
  group_by(county) %>% 
  summarise(GGD_1_50 = mean(GGD_1_50))

GDD_3 <- left_join(IL_counties, GDD_1990,by = join_by(county))

ggplot() +
  #geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = WTSP_3, aes(fill = GGD_1_50), show.legend = T) +
  scale_fill_viridis_c(option = "A")+
  theme_void() +
  labs(title = "1991-2009")

######
GDD_2010 <-  SBC_all_data %>% 
  filter(year >= 2010,
         Common_Name == "Yellow-rumped Warbler") %>% 
  drop_na(GGD_1_50) %>% 
  group_by(county) %>% 
  summarise(GGD_1_50 = mean(GGD_1_50))

GDD_2 <- left_join(IL_counties, GDD_2010,by = join_by(county))

ggplot() +
  #geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = WTSP_2, aes(fill = GGD_1_50), show.legend = T) +
  scale_fill_viridis_c(option = "A")+
  theme_void() +
  labs(title = "2010-2025")

WTSP <- left_join(WTSP_2010,WTSP_1980, join_by(county))

WTSP <- WTSP %>% 
  mutate(diff = GGD_1_50.x - GGD_1_50.y) %>% 
  mutate(category = ifelse(diff >0, "Increase", "Decrease"))

WTSP_diff <- left_join(IL_counties, WTSP,by = join_by(county))

ggplot() +
  #geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = WTSP_diff, aes(fill = category), show.legend = T) +
  scale_fill_viridis_d(option = "F")+
  theme_void()

#### Some NDVI stuff
### Map
eqdc <- '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

#states <- USAboundaries::us_states(resolution = "low") %>% 
#filter(state_abbr %in% c("IL")) %>% 
#st_transform(eqdc)

#IL_counties <- USAboundaries::us_counties(states = "Illinois") %>% 
#select(-c(state_name)) %>% 
#rename(county = name) %>% 
#mutate(county = ifelse(county == "LaSalle", "La Salle", county))

IL_counties <- usmap::us_map(regions = "counties") %>% 
  filter(abbr == "IL") %>% 
  mutate(county = ifelse(county == "LaSalle County", "La Salle", county)) %>% 
  st_transform(eqdc)

IL_counties$county <- gsub(" County","", IL_counties$county)

IL <- usmap::us_map("states") %>%
  filter(abbr == "IL") %>%
  as_sf %>%
  st_transform(crs_target)

IL <- ipumsr::read_ipums_sf(shape_file = "C:/Users/awsmilor/Desktop/IL_BNDY_State.zip", file_select = "IL_BNDY_State_Ln.shp") |> 
  st_make_valid() |> # Fix minor border inconsistencies
  st_union()

files <- list.files("C:/Users/awsmilor/Desktop/Earthdata/MOD13A2_061-20251120_152524", full.names = TRUE)

tile_codes <- unique(str_extract(files, "h[0-9]{2}v[0-9]{2}"))

tiles <- map(
  tile_codes,
  function(code) files[str_detect(files, code)]
)

names(rast(files[1]))

rast(files[1], lyrs = 1)

# Maximum layer value will be the first layer in the very last file being loaded
max_lyrs <- 12 * length(tiles[[1]])

# Create sequence in increments of 12 up to the maximum layer index needed
ndvi_layers <- seq(1, max_lyrs, by = 12)

ndvi_layers
#> [1]  1 13

# Load NDVI layers for each set of files corresponding to a particular tile
ndvi_tiles <- map(
  tiles, 
  function(x) rast(x, lyrs = ndvi_layers)
)

map(ndvi_tiles, ext)

# Obtain CRS of NDVI raster data
ndvi_crs <- crs(ndvi_tiles[[1]])

# Transform borders to same CRS as NDVI
IL <- st_transform(IL, crs = ndvi_crs)

ndvi_tiles <- map(
  ndvi_tiles, 
  function(x) crop(x, IL)
)

# Mosaic two of our tiles together
IL_ndvi <- reduce(ndvi_tiles, mosaic)

IL_ndvi <- IL_ndvi / 100000000


ndvi_pal <- list(
  pal = c(
    "#fdfbdc",
    "#f1f4b7",
    "#d3ef9f",
    "#a5da8d",
    "#6cc275",
    "#51a55b",
    "#397e43",
    "#2d673a",
    "#1d472e" 
  ),
  values = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 1)
)

ggplot() +
  layer_spatial(IL_ndvi[[1]]) +
  scale_fill_gradientn(
    colors = ndvi_pal$pal,
    values = ndvi_pal$values,
    limits = c(0, 1),
    na.value = "transparent"
  ) +
  layer_spatial(IL) +
  labs(
    title = "NDVI: Illinois",
    subtitle = "January -16, 2025",
    fill = "NDVI",
    caption = "Source: NASA MOD13Q1"
  )

IL_ndvi_mask <- mask(IL_ndvi, mask = vect(IL))

ggplot() +
  layer_spatial(IL_ndvi[[1]]) +
  layer_spatial(IL, fill = NA) +
  scale_fill_gradientn(
    colors = ndvi_pal$pal,
    values = ndvi_pal$values,
    limits = c(0, 1),
    na.value = "transparent"
  ) +
  labs(
    subtitle = "January 1-16, 2014",
    fill = "NDVI"
  ) + 
  theme_minimal()
  