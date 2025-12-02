library(tidyverse)
library(visdat)
library(patchwork)
library(broom)
library(ggfortify)
library(ggtext)
library(viridis)
library(mgcv)
library(zoo)

### Set working directory
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

### Read in data
#Read in SBC data
SBC_GDD <- read.csv(file = "SBC_GDD.csv") %>% 
  select(-c(X))     #Drop extraneous column

#Read in Party Hour Data
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

ph_2025 <- read.csv(file = "County_Stats_2025.csv") %>% 
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
  mutate(Parties = as.character(Parties),
         Party_Hours = as.numeric(Party_Hours)) %>% 
  mutate(countyyear = paste0(county, " ",year)) %>% #Create a county & year variable
  select(-c(county, year))

ph <- bind_rows(ph, ph_2025)

#Read in precipitation data
precip <- read.csv(file = "precip_final/AllCounties_FINAL.csv") %>% 
  mutate(countyyear = paste0(county, " ",year)) %>% #Create a county & year variable
  select(-c(county, year, latitude,longitude,julian_SBC))

temp_2025 <- read.csv(file = "2025_FINAL/AllCounties_FINAL_2025.csv") %>% 
  mutate(countyyear = paste0(county, " ",year)) %>% #Create a county & year variable
  select(-c(county, year))

SBC_GDD <- SBC_GDD %>% 
  left_join(ph, by = join_by(countyyear)) #Merge the Party hour data into the SBC_GDD data

SBC <- SBC_GDD %>% 
  left_join(precip, by = join_by(countyyear)) #Merge the precipitation data into the SBC_GDD data

SBC_final <- SBC %>% 
  left_join(temp_2025,by = join_by(countyyear))

SBC_final <- SBC_final %>%
  mutate(across(ends_with(".y"), ~ coalesce(., get(str_replace(cur_column(), ".y$", ".x"))))) %>%
  select(-ends_with(".x")) %>%
  rename_with(~ str_remove(., ".y$"))

SBC_final <- SBC_final %>% 
  arrange(countyyear) %>% 
  mutate(class = latitude) %>% 
  mutate(class = ifelse(class > 40.6625933334, 3, class),
         class = ifelse(class > 38.8165766667, 2, class),
         class = ifelse(class >36.970560, 1, class))

test <- SBC_final %>% 
  filter(Common_Name == "Veery")

write.csv(SBC_final, "SBC_all_data.csv")

filtered_data <- SBC_final %>%
  group_by(Common_Name) %>%
  mutate(prop_zero = mean(Count == 0, na.rm = TRUE)) %>%   # proportion of zeros per species
  filter(prop_zero < 0.99) %>%                             # keep species with <99% zeros
  select(-prop_zero) %>% 
  drop_na(Party_Hours) %>%  #Drop rows where the party hour variable is NA, indicating no coverage (May need to revisit this since some years where Party Hour = NA had data & observers)
  ungroup()
  
species <- unique(filtered_data$Common_Name) #If you want to run the loop for all species

species <- unique(head$Common_Name)

#### Create a list of species of interest for data visualization and exploration purposes
#species <- c("Yellow-rumped Warbler", "Swainson's Thrush", "Veery", "White-throated Sparrow",
#             "Dark-eyed Junco", "Tennessee Warbler", "Magnolia Warbler", "Bay-breasted Warbler",
#             "Blackpoll Warbler", "Ruby-crowned Kinglet","Eastern Towhee", "Brown Thrasher", "Chimney Swift", "White-breasted Nuthatch",
#             "American Robin", "Wilson's Snipe", "Palm Warbler", "Black-capped Chickadee", "Carolina Chickadee", "Lincoln's Sparrow", "Mourning Warbler",
#             "Northern Cardinal", "Prothonotary Warbler", "Nashville Warbler", "Yellow-throated Warbler", "House Wren", "Yellow-billed Cuckoo", "Gray-cheeked Thrush",
#             "Eastern Wood-Pewee", "Northern Waterthrush", "Bobolink", "Dickcissel",
#             "Least Sandpiper", "Semipalmated Plover", "Semipalmated Sandpiper", "Dunlin", "Lesser Yellowlegs", "Rose-breasted Grosbeak",
#             "Northern Shoveler", "Blue-winged Teal", "Green-winged Teal", "Eastern Kingbird", "Common Nighthawk", "Purple Finch",
#             "Red-breasted Nuthatch", "Eastern Bluebird", "Hermit Thrush", "Willow Flycatcher", "Least Flycatcher", "Song Sparrow", "Swamp Sparrow", "Vesper Sparrow",
#             "Hooded Warbler", "Kentucky Warbler", "Worm-eating Warbler", "Northern Parula", "Orange-crowned Warbler", "Golden-winged Warbler", "Blue-winged Warbler",
#             "Ovenbird", "Ruby-throated Hummingbird", "Chestnut-sided Warbler", "Cerulean Warbler", "Chipping Sparrow", "Common Yellowthroat", "American Redstart",
#             "American Goldfinch", "Field Sparrow", "Cape May Warbler", "Canada Warbler", "Carolina Wren")

######################
##### For loop for final plots

for (i in species) {
  
  message(paste0("Started ", i))
  
  sub <- filtered_data %>% 
    filter(Common_Name == i) %>% 
    group_by(year, class) %>% 
    summarise(mean = mean(Count),
              GGD_1_50_mean = mean(GGD_1_50)) %>% 
    ungroup()
  
  
  plot <- sub %>% 
    ggplot(aes(x = GGD_1_50_mean, y = mean, color = class)) +
    geom_point() +
    geom_smooth(method = "gam", color = "indianred") +
    theme_minimal() +
    labs(title = paste(i, ": Mean Count by mean GDD in past 25 days"))+
    facet_wrap(~class)
  
  message("Plot 1 created")
  
  plot2 <- sub %>% 
    ggplot(aes(x = year, y = GGD_1_50_mean, color = class)) +
    geom_point() +
    geom_line() +
    geom_smooth(method = "gam", color = "indianred", se = F) +
    theme_minimal()+
    facet_wrap(~class)
  
  message("Plot 2 created")
  
  plot3 <- sub %>% 
    ggplot(aes(x = year, y = mean, color = GGD_1_50_mean)) +
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
      m <- gam(mean ~ s(GGD_1_50_mean), data = .x)
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
  
  ggsave(paste0("GAM_",i,".png"), final_plot, width = 10, height = 12, dpi = 300, path = "C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Imgs/GAM_Plots_top50", create.dir = TRUE)
  message(i, " Done")
  
}

## Testing

test <- filtered_data %>% 
  filter(Common_Name == "Green-winged Teal") %>% 
  group_by(year, class) %>% 
  summarise(GGD_1_50 = mean(GGD_1_50),
            precip = mean(total_precip_25),
            mean = mean(Count),
            .groups = "drop")

# Create 5-year bins
test <- test %>%
  group_by(class) %>% 
  mutate(bin = cut(year, 
                   breaks = seq(min(year), max(year) + 4, by = 5),
                   right = FALSE,
                   labels = paste(seq(min(year), max(year), by = 5),
                                  seq(min(year)+4, max(year)+4, by = 5),
                                  sep = "-"))) %>% 
  ungroup() %>% 
  mutate(bin = paste0(bin, "_",class))



# Compute 5-year block average
block_avg <- test %>%
  group_by(bin) %>%
  summarise(mean_GGD = mean(GGD_1_50),
            mean_precip = mean(precip),
            .groups = "drop")

# Merge block averages back for plotting bars
test <- left_join(test, block_avg, by = "bin")

test <- test %>% 
  group_by(year) %>% 
  mutate(vs_mean_GDD = ifelse(GGD_1_50 - mean_GGD > 0, "Above", "Below"),
         vs_mean_precip = ifelse(precip - mean_precip > 0, "Above", "Below")) %>% 
  ungroup()

plot_GDD <- ggplot(test, aes(x = year)) +
  geom_col(aes(y = mean_GGD, fill = "5-Year Mean"), width = 1, alpha = 0.5) +
  geom_point(aes(x = year, y = GGD_2_50), data = filtered_data %>% filter(Common_Name == "White-throated Sparrow"), color = "gray80", alpha = .5)+
  geom_point(aes(y = GGD_1_50, color = vs_mean_GDD)) +
  geom_line(aes(y = GGD_1_50)) +
  geom_smooth(aes(y = GGD_1_50), method = "glm", se = FALSE, color = "black") +
  scale_fill_manual(values = "indianred3") +
  labs(
    x = "Year",
    y = "GGD (1–50)",
    title = "GGD_1_50 Over Time",
    fill = ""
  ) +
  theme_minimal() +
  facet_wrap(~class)

plot_GDD

plot_precip <- ggplot(test, aes(x = year)) +
  geom_col(aes(y = mean_precip, fill = "5-Year Mean"), width = 1, alpha = 0.5) +
  geom_point(aes(x = year, y = total_precip_25), data = filtered_data %>% filter(Common_Name == "White-throated Sparrow"), color = "gray80", alpha = .5)+
  geom_point(aes(y = precip, color = vs_mean_precip)) +
  geom_line(aes(y = precip)) +
  geom_smooth(aes(y = precip), method = "glm", se = FALSE, color = "black") +
  scale_fill_manual(values = "indianred3") +
  labs(
    x = "Year",
    y = "Total Precipitation",
    title = "Precipitation Over Time",
    fill = ""
  ) +
  theme_minimal() +
  facet_wrap(~class)

plot_precip

plot <- test %>% 
  ggplot(aes(x = GGD_1_50, y = mean, color = class)) +
  geom_point() +
  geom_smooth(method = "gam", color = "indianred") +
  theme_minimal() +
  labs(title = paste("BWTE: Mean Count by mean GDD in past 50 days"))+
  facet_wrap(~class)

plot_2 <- test %>% 
  ggplot(aes(x = precip, y = mean, color = class)) +
  geom_point() +
  geom_smooth(method = "gam", color = "indianred") +
  theme_minimal() +
  labs(title = paste("BWTE: Mean Count by total precip in past 50 days"))+
  facet_wrap(~class)

## ---- GAM STATS ----
gam_stats_1 <- test %>%
  group_by(class) %>%
  group_modify(~{
    m <- gam(mean ~ s(GGD_1_50), data = .x)
    s <- summary(m)
    
    tibble(
      p_value = signif(s$s.table[1,4], 3),
      r2_adj = signif(s$r.sq, 3)
    )
  }) %>%
  pivot_longer(cols = -class, names_to = "metric", values_to = "value")

stats_plot_1 <- gam_stats_1 %>%
  ggplot(aes(x = metric, y = 0)) +
  geom_tile(color = "white", fill = "white") +
  geom_text(aes(label = value), size = 4, color = "gray20")+
  theme_void() + 
  theme(axis.text.x = element_text(colour = "gray20")) +
  labs(title = paste("GAM_GDD Summary: BWTE"),
       x = "", y = "") +
  facet_wrap(~class)

gam_stats_2 <- test %>%
  group_by(class) %>%
  group_modify(~{
    m <- gam(mean ~ s(precip), data = .x)
    s <- summary(m)
    
    tibble(
      p_value = signif(s$s.table[1,4], 3),
      r2_adj = signif(s$r.sq, 3)
    )
  }) %>%
  pivot_longer(cols = -class, names_to = "metric", values_to = "value")

stats_plot_2 <- gam_stats_2 %>%
  ggplot(aes(x = metric, y = 0)) +
  geom_tile(color = "white", fill = "white") +
  geom_text(aes(label = value), size = 4, color = "gray20")+
  theme_void() + 
  theme(axis.text.x = element_text(colour = "gray20")) +
  labs(title = paste("GAM_Precip Summary: BWTE"),
       x = "", y = "") +
  facet_wrap(~class)

gam_stats_3 <- test %>%
  group_by(class) %>%
  group_modify(~{
    m <- gam(mean ~ s(GGD_1_50) + s(precip), data = .x)
    s <- summary(m)
    
    tibble(
      p_value = signif(s$s.table[1,4], 3),
      r2_adj = signif(s$r.sq, 3)
    )
  }) %>%
  pivot_longer(cols = -class, names_to = "metric", values_to = "value")

stats_plot_3 <- gam_stats_3 %>%
  ggplot(aes(x = metric, y = 0)) +
  geom_tile(color = "white", fill = "white") +
  geom_text(aes(label = value), size = 4, color = "gray20")+
  theme_void() + 
  theme(axis.text.x = element_text(colour = "gray20")) +
  labs(title = paste("GAM_Both Summary: BWTE"),
       x = "", y = "") +
  facet_wrap(~class)


plot_GDD / plot / stats_plot_1 / plot_precip / plot_2 / stats_plot_2 / stats_plot_3


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