library(tidyverse)
library(visdat)

"%ni%" = Negate("%in%")

############ To-do:
############ 1. Pivot Bird data to have a row for counties instead of separate columns for each county
############ 2. Make a shiny app that allows you to see the population trend overtime for any species.
############ 3. Maybe consider adding growing degrees data
############ 4. Run GLMS to see if the growing degree data has any effect (would suggest earlier migration of things like DEJU)

setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

# Data Preparation
##### Read in temperature for all counties in illinois.
temp <- read.csv("./GDD_Final/AllCounties_FINAL.csv") %>% 
  mutate(countyyear = paste0(county, " ",year)) %>% 
  select(-c(year, county))

##### Read in the SBC bird data
df <- read.csv("./SBC_Data_PH_2024.csv") %>% 
  rename(DeKalb = De.Kalb,
         'De Witt' = De.Witt,
         DuPage = Du.Page,
         'St. Clair' = St..Clair,
         'Rock Island' = Rock.Island,
         'Jo Daviess' = Jo.Daviess,
         'La Salle' = La.Salle,
         Vermilion = Vermilion)

# Pivot and clean the SBC data
SBC <- df %>%
  pivot_longer(
    cols = Adams:Woodford,              # all county columns
    names_to = "County",                # new column name for counties
    values_to = "Count"                 # new column name for counts
  ) %>%
  select(Common_Name, County, Count, Year, TAXON_ORDER, SCI_NAME) %>%   # reorder columns
  arrange(Common_Name, County) %>%          # tidy ordering'
  rename(year = Year,
         county = County) %>% 
  mutate(countyyear = paste0(county, " ",year))

df_2025 <- read.csv("./SBC_Data_2025.csv") %>% 
  rename(DeKalb = De.Kalb,
         'De Witt' = De.Witt,
         DuPage = Du.Page,
         'St. Clair' = St..Clair,
         'Rock Island' = Rock.Island,
         'Jo Daviess' = Jo.Daviess,
         'La Salle' = La.Salle,
         Vermilion = Vermilion)

SBC_2025 <- df_2025 %>%
  pivot_longer(
    cols = Adams:Woodford,              # all county columns
    names_to = "County",                # new column name for counties
    values_to = "Count"                 # new column name for counts
  ) %>%
  select(Common_Name, County, Count, Year) %>%   # reorder columns
  arrange(Common_Name, County) %>%          # tidy ordering'
  rename(year = Year,
         county = County) %>% 
  mutate(countyyear = paste0(county, " ",year))

SBC <- bind_rows(SBC, SBC_2025)

##### Merge Datasets
### Merge the  temperature and growing degree days data with the SBC data
merge <- left_join(SBC, temp, by = join_by(countyyear)) %>% 
    filter(year > 1972)  #Filter to exclude years without growing degree data

### Write a CSV with this information
merge %>% 
  write.csv(file = "SBC_GDD.csv")

vis_dat(slice_sample(merge, n = 10000))

unique(SBC$county)

unique(temp$county)

# Pivot and clean the SBC data
tree_il <- tree_us %>%
  filter(subnational1 == "Illinois",
         threshold == 0) %>% 
  select(-c(extent_2000_ha,extent_2010_ha, gain_2000.2012_ha)) %>% 
  rename(state = subnational1,
         county = subnational2) %>% 
  pivot_longer(
    cols = '2001':'2024',              # all county columns
    names_to = "year",                # new column name for counties
    values_to = "tc_loss_ha"                 # new column name for counts
  ) %>%
  mutate(county = case_when(
    county == "De Witt" ~ "DeWitt",
    county == "De Kalb" ~ "DeKalb",
    county == "La Salle" ~ "LaSalle",
    county == "Rock Island" ~ "RockIsland",
    county == "Saint Clair" ~ "StClair",
    county == "Jo Daviess" ~ "JoDaviess",
    county == "Vermillion" ~ "Vermillion",
    TRUE ~ county  # keep others unchanged
  )) %>%
  arrange(county) %>%    # tidy ordering'
  group_by(county) %>% 
  mutate(cum_loss = cumsum(tc_loss_ha)) %>% 
  ungroup() %>% 
  mutate(countyyear = paste0(county, " ",year)) %>% 
  select(-c(county, year))

##### Read in county specific data on tree cover loss in the US for each year since 2001 from Global Forest Watch
tree_us <- read.csv("./tree_cover_loss.csv")

colnames(tree_us) <- gsub("tc_loss_ha_", "", colnames(tree_us))

x <- 1:15


### Add in tree cover loss data (optional)
merge_all <- left_join(merge, tree_il, by = join_by(countyyear))