library(tidyverse)
library(terra)
library(MODISTools)
library(sf)
library(exactextractr)
library(patchwork)

#####
### Set Working Directory
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

### Read in all data. Does not include species where more than 99% of all observations were 0
SBC_all_data <- read.csv(file = "SBC_all_data.csv") %>% 
  #read.csv("/Volumes/mpward/SBC_analyses/SBC_all_data.csv") %>% 
  select(-c(X)) %>% 
  rename(county = coun) %>% 
  mutate(zone = class) %>% 
  mutate(zone = ifelse(county %in% c("Madison", "St. Clair", "Randolph", "Perry", "Jackson", "Monroe", "Franklin", "Williamson", "Union", "Johnson", "Saline", "Pope", "Alexander", "Pulaski", "Massac", "Hardin", "Gallatin", "White"), "7a", zone),
         zone = ifelse(county %in% c("Hamilton", "Jefferson", "Washington", "Wayne", "Marion", "Clinton", "Wabash", "Edwards", "Richland", "Lawrence", "Crawford", "Jasper", "Clay", "Effingham", "Fayette", "Bond", "Montgomery", "Macoupin", "Jersey", "Calhoun", "Shelby", "Cumberland", "Clark"), "6b", zone),
         zone = ifelse(county %in% c("Iroquois", "Kankakee", "Grundy", "LaSalle", "Ogle", "Livingston", "Woodford", "Marshall", "Putnam", "Peoria", "Knox", "Stark", "Bureau","Henry", "Whiteside", "Mercer", "Henderson", "Warren", "Rock Island", "Lee", "La Salle", "DeKalb", "Kendall", "Kankakee", "DuPage", "Lake", "McHenry", "Winnebago", "Jo Daviess", "Carroll", "Stephenson", "Boone", "Kane"), "5", zone),
         #Probably need to check to see if Livingston is 5b or 6a
         zone = ifelse(county %in% c("Coles", "Edgar", "Douglas", "Moultrie", "Macon", "Christian", "Sangamon", "Morgan", "Greene", "Pike", "Scott", "Adams", "Brown", "Cass", "Menard", "Logan", "De Witt", "Piatt", "Champaign", "Vermilion", "Mason", "Schuyler", "Hancock", "McDonough", "Tazewell", "McLean", "Ford", "Fulton", "Will", "Cook"), "6a", zone)
  )

## Load in area of interest. In this case, a polygon shapefile of all counties in Illinois
IL_counties <- vect("./IL_BNDY_County/IL_BNDY_County_Py.shp")
IL_counties <- project(IL_counties, "+proj=longlat +datum=WGS84")

#### Specify the coordinates to download. Coordinates are determined to ensure full coverage of the state.
lat <- c(40.57213,40.57213, 40.57213, 42.170664, 42.170664, 42.170664, 38.9736, 38.9736, 38.9736, 37.37507,37.37507,37.37507)
lon <- c(-87.209763,-89.309763, -91.409763, -89.309763, -87.109763, -91.509763, -87.209763, -89.309763, -91.409763, -87.209763, -89.309763, -91.409763)
site_name <- c("IL1", "IL2", "IL3", "IL4", "IL5", "IL6", "IL7", "IL8", "IL9", "IL10", "IL11", "IL12")
coords <- data.frame(site_name, lat, lon)

products <- mt_products()

# bands of the vegetation indices product
bands <- mt_bands(product = "VNP13A1")
head(bands)

dates <- mt_dates(product = "VNP13A1", lat = 40.369364, lon = -89.309763)
head(dates)

#### Download NDVI data for One point ----
start_time <- Sys.time()
ndvi <- mt_subset(product = "VNP13A1",
                      lat = 40.369364,
                      lon = -89.309763,
                      band = c("500_m_16_days_NDVI",
                               "500_m_16_days_pixel_reliability"),
                      start = "2021-05-03",
                      end = "2021-05-10",
                      km_lr = 100,
                      km_ab = 100,
                      site_name = "IL",
                      internal = TRUE,
                      progress = TRUE)

end_time <- Sys.time()
end_time - start_time

#### Batch download NDVI data for May from 2012 to 2024. This process takes multiple hours. Test with smaller datasets before proceeding
years <- c(2012:2024)

start_time <- Sys.time()
  
for (i in years) {
  tmp <- mt_batch_subset(df = coords,
                             product = "VNP13A1",
                             band = c("500_m_16_days_NDVI"),
                             start = paste0(i,"-05-03"),
                             end = paste0(i,"-05-10"),
                             km_lr = 100,
                             km_ab = 100,
                             internal = TRUE)
  df_name <- paste0("IL_ndvi_", i)
  assign(df_name, tmp)
}

end_time <- Sys.time()
end_time - start_time

#### Export rasters for all 12 years so you don't need to do the batch download again.
ndvi_split <- IL_ndvi_2024 %>% #change year wherever you see it
  filter(band == "500_m_16_days_NDVI") %>% split(IL_ndvi_2024$site)

ndvi_rast_list <- lapply(ndvi_split, function(x) {mt_to_terra(x, reproject = TRUE)})

ndvi_rast_1 <- ndvi_rast_list$IL1
ndvi_rast_2 <- ndvi_rast_list$IL2
ndvi_rast_3 <- ndvi_rast_list$IL3
ndvi_rast_4 <- ndvi_rast_list$IL4
ndvi_rast_5 <- ndvi_rast_list$IL5
ndvi_rast_6 <- ndvi_rast_list$IL6
ndvi_rast_7 <- ndvi_rast_list$IL7
ndvi_rast_8 <- ndvi_rast_list$IL8
ndvi_rast_9 <- ndvi_rast_list$IL9
ndvi_rast_10 <- ndvi_rast_list$IL10
ndvi_rast_11 <- ndvi_rast_list$IL11
ndvi_rast_12 <- ndvi_rast_list$IL12

merged_rasters <- merge(ndvi_rast_1,ndvi_rast_2,ndvi_rast_3,ndvi_rast_4,ndvi_rast_5,ndvi_rast_6,ndvi_rast_7,ndvi_rast_8,ndvi_rast_9,ndvi_rast_10,ndvi_rast_11,ndvi_rast_12)

plot(merged_rasters)
plot(IL_counties, add = T)

writeRaster(merged_rasters, filename = "IL_NDVI_2024.tif")

#### Get county means for each raster
ndvi_2024 <- rast("IL_NDVI_2024.tif")
IL_counties <- st_read("./IL_BNDY_County/IL_BNDY_County_Py.shp")

if (st_crs(IL_counties) != crs(ndvi_2024)) {
  IL_counties <- st_transform(IL_counties, crs(ndvi_2024))
}

county_NDVI_median_2024 <- exact_extract(
  ndvi_2024, 
  IL_counties, 
  fun = "median",
  append_cols = "COUNTY_NAM"
)

ndvi_median_2024 <-  IL_counties %>% 
  left_join(county_NDVI_median_2024)

county_names <- read.csv("county_names.csv")
ndvi_median_2024 <-  ndvi_median_2024 %>% 
  left_join(county_names) %>% 
  select(-c(COUNTY_NAM)) %>% 
  mutate(year = 2024)

SBC_2024 <- SBC_all_data %>% 
  filter(year == 2024) %>% 
  select(c(year, county, GGD_1_25,GGD_1_50,GGD_1_100, Count,Common_Name, class, zone))

all_2024 <- ndvi_median_2024 %>% 
  left_join(SBC_2024)

all_2024 %>%
  filter(Common_Name == "Yellow-rumped Warbler") %>% 
  ggplot(aes(x = median, y = Count)) + 
  geom_point()+
  geom_smooth(method = "glm")+
  theme_minimal() +
  facet_wrap(~zone)

m <- glm(data = all_2024 %>% filter(Common_Name == "Yellow-rumped Warbler", zone == "6a"), 
         formula = Count ~ median)
summary(m)

p1 <- all_2024 %>% 
  ggplot()+
  geom_sf(aes(fill = median))+
  scale_fill_viridis_c(option = "D")+
  theme_void()

p2 <- all_2024 %>% 
  ggplot()+
  geom_sf(aes(fill = GGD_1_25))+
  scale_fill_viridis_c(option = "D")+
  theme_void()

p1 + p2

unique(ndvi_median_2024$COUNTY_NAM)
