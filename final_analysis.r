library(tidyverse)
library(visdat)
library(patchwork)
library(broom)
library(ggfortify)
library(ggtext)
library(viridis)
library(mgcv)

### Set working directory
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

### Read in data
SBC_GDD <- read.csv(file = "SBC_GDD.csv") %>% 
  select(-c(X)) %>%     #Drop extraneous column
  mutate(class = latitude) %>% 
  mutate(class = ifelse(class > 41, 3, class),
         class = ifelse(class > 39, 2, class),
         class = ifelse(class >37, 1, class))

ph <- read.csv(file = "PH_over_time_2024.csv") %>% 
  select(c(Counties, Species, Birds, Observers, Parties, Party_Hours, year)) %>% 
  rename(county = Counties) %>% 
  mutate(county = ifelse(county == "De.Kalb", "DeKalb", county), #rename counties to match the SBC_GDD naming conventions
         county = ifelse(county == "De.Witt", "De Witt", county),
         county = ifelse(county == "La.Salle", "La Salle", county),
         county = ifelse(county == "Rock.Island", "Rock Island", county),
         county = ifelse(county == "St..Clair", "St. Clair", county),
         county = ifelse(county == "Jo.Daviess", "Jo Daviess", county),
         county = ifelse(county == "Vermilion", "Vermillion", county),
         county = ifelse(county == "Du.Page", "DuPage", county)
  ) %>%
  mutate(countyyear = paste0(county, " ",year)) %>% #Create a county & year variable
  select(-c(county, year))

SBC_GDD <- SBC_GDD %>% 
  left_join(ph, by = join_by(countyyear)) #Merge the Party hour data into the SBC_GDD data

filtered_data <- SBC_GDD %>%
  group_by(Common_Name) %>%
  mutate(prop_zero = mean(Count == 0, na.rm = TRUE)) %>%   # proportion of zeros per species
  filter(prop_zero < 0.99) %>%                             # keep species with <99% zeros
  select(-prop_zero) %>% 
  drop_na(Party_Hours) #Drop rows where the party hour variable is NA, indicating no coverage (May need to revisit this since some years where Party Hour = NA had data & observers)

species <- unique(filtered_data$Common_Name) #If you want to run the loop for all species

#### Create a list of species of interest for data visualization and exploration purposes
species <- c("Yellow-rumped Warbler", "Swainson's Thrush", "Veery", "White-throated Sparrow",
             "Dark-eyed Junco", "Tennessee Warbler", "Magnolia Warbler", "Bay-breasted Warbler",
             "Blackpoll Warbler", "Ruby-crowned Kinglet","Eastern Towhee", "Brown Thrasher", "Chimney Swift", "White-breasted Nuthatch",
             "American Robin", "Wilson's Snipe", "Palm Warbler", "Black-capped Chickadee", "Carolina Chickadee", "Lincoln's Sparrow", "Mourning Warbler",
             "Northern Cardinal", "Prothonotary Warbler", "Nashville Warbler", "Yellow-throated Warbler", "House Wren", "Yellow-billed Cuckoo", "Gray-cheeked Thrush",
             "Eastern Wood-Pewee", "Northern Waterthrush", "Bobolink", "Dickcissel",
             "Least Sandpiper", "Semipalmated Plover", "Semipalmated Sandpiper", "Dunlin", "Lesser Yellowlegs", "Rose-breasted Grosbeak",
             "Northern Shoveler", "Blue-winged Teal", "Green-winged Teal", "Eastern Kingbird", "Common Nighthawk", "Purple Finch",
             "Red-breasted Nuthatch", "Eastern Bluebird", "Hermit Thrush", "Willow Flycatcher", "Least Flycatcher", "Song Sparrow", "Swamp Sparrow", "Vesper Sparrow",
             "Hooded Warbler", "Kentucky Warbler", "Worm-eating Warbler", "Northern Parula", "Orange-crowned Warbler", "Golden-winged Warbler", "Blue-winged Warbler",
             "Ovenbird", "Ruby-throated Hummingbird", "Chestnut-sided Warbler", "Cerulean Warbler", "Chipping Sparrow", "Common Yellowthroat", "American Redstart",
             "American Goldfinch", "Field Sparrow", "Cape May Warbler", "Canada Warbler", "Carolina Wren")

######################
##### For loop for final plots

for (i in species) {
  
  message(paste0("Started ", i))
  
  sub <- filtered_data %>% 
    filter(Common_Name == i) %>% 
    group_by(year, class) %>% 
    summarise(mean = mean(Count),
              GGD_1_25_mean = mean(GGD_1_25)) %>% 
    ungroup()
  
  
  plot <- sub %>% 
    ggplot(aes(x = GGD_1_25_mean, y = mean, color = class)) +
    geom_point() +
    geom_smooth(method = "gam", color = "indianred") +
    theme_minimal() +
    labs(title = paste(i, ": Mean Count by mean GDD in past 25 days"))+
    facet_wrap(~class)
  
  message("Plot 1 created")
  
  plot2 <- sub %>% 
    ggplot(aes(x = year, y = GGD_1_25_mean, color = class)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "gam", color = "indianred", se = F) +
    theme_minimal()+
    facet_wrap(~class)
  
  message("Plot 2 created")
  
  plot3 <- sub %>% 
    ggplot(aes(x = year, y = mean, color = GGD_1_25_mean)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "gam", color = "indianred", se = F) +
    theme_minimal() +
    scale_color_viridis_c(option = "plasma") +
    facet_wrap(~class)
  
  message("Plot 3 created")
  
  ## ---- GAM STATS ----
  gam_stats <- sub %>%
    group_by(class) %>%
    group_modify(~{
      m <- gam(mean ~ s(GGD_1_25_mean), data = .x)
      s <- summary(m)
      
      tibble(
        p_value = signif(s$s.table[1,4], 3),
        r2_adj = signif(s$r.sq, 3)
      )
    }) %>%
    pivot_longer(cols = -class, names_to = "metric", values_to = "value")
  
  stats_plot <- gam_stats %>%
    ggplot(aes(x = metric, y = 0)) +
    geom_tile(color = "white", fill = "white") +
    geom_text(aes(label = value), size = 4, color = "gray20")+
    theme_void() + 
    theme(axis.text.x = element_text(colour = "gray20")) +
    labs(title = paste("GAM Summary:", i),
         x = "", y = "") +
    facet_wrap(~class)
  
  final_plot <- plot / plot2 / plot3 / stats_plot
  
  ggsave(paste0("GAM_",i,".png"), final_plot, width = 10, height = 12, dpi = 300, path = "C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Imgs/GAM_Plots", create.dir = TRUE)
  message(i, " Done")
  
}

###Interesting Species to look at:
# Baltimore Oriole
# Blue Jay
# Blue-winged Teal
# Brown Creeper
# Chestnut-sided Warbler
# Common Yellowthroat
# Dark-eyed Junco
# Dickcissel
# Downy Woodpecker
# Eastern Kingbird
# Eastern Wood-Pewee
# Golden-crowned Kinglet
# Grasshopper Sparrow
# Gray Catbird
# Great Crested Flycatcher
# House Wren
# Indigo Bunting
# Lesser Yellowlegs
# Northern Shoveler
# Olive-sided Flycatcher
# Orchard Oriole
# Palm Warbler
# Pied-billed Grebe
# Prothonotary Warbler
# Purple Finch
# Red-breasted Nuthatch
# Red-eyed Vireo
# Ruby-crowned Kinglet
# Ruby-throated Hummingbird
# Rusty Blackbird
# Scarlet Tanager
# Swamp Sparrow
# Tennessee Warbler
# Warbling Vireo
# White-throated Sparrow
# Wood Thrush
# Yellow-billed Cuckoo
# Yellow-breasted Chat
# Yellow-rumped Warbler