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
library(fmdates)

#####
### Set Working Directory
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

### Read in all data. Does not include species where more than 99% of all observations were 0
SBC_all_data <- read.csv(file = "SBC_all_data.csv") %>% 
  #read.csv("/Volumes/mpward/SBC_analyses/SBC_all_data.csv") %>% 
  select(-c(X)) %>% 
  rename(county = coun)

SBC_zones <- SBC_all_data %>% 
  mutate(zone = class) %>% 
  mutate(zone = ifelse(county %in% c("Madison", "St. Clair", "Randolph", "Perry", "Jackson", "Monroe", "Franklin", "Williamson", "Union", "Johnson", "Saline", "Pope", "Alexander", "Pulaski", "Massac", "Hardin", "Gallatin", "White"), "7a", zone),
         zone = ifelse(county %in% c("Hamilton", "Jefferson", "Washington", "Wayne", "Marion", "Clinton", "Wabash", "Edwards", "Richland", "Lawrence", "Crawford", "Jasper", "Clay", "Effingham", "Fayette", "Bond", "Montgomery", "Macoupin", "Jersey", "Calhoun", "Shelby", "Cumberland", "Clark"), "6b", zone),
         zone = ifelse(county %in% c("Iroquois", "Kankakee", "Grundy", "LaSalle", "Ogle", "Livingston", "Woodford", "Marshall", "Putnam", "Peoria", "Knox", "Stark", "Bureau","Henry", "Whiteside", "Mercer", "Henderson", "Warren", "Rock Island", "Lee", "La Salle", "DeKalb", "Kendall", "Kankakee", "DuPage", "Lake", "McHenry", "Winnebago", "Jo Daviess", "Carroll", "Stephenson", "Boone", "Kane"), "5", zone),
         #Probably need to check to see if Livingston is 5b or 6a
         zone = ifelse(county %in% c("Coles", "Edgar", "Douglas", "Moultrie", "Macon", "Christian", "Sangamon", "Morgan", "Greene", "Pike", "Scott", "Adams", "Brown", "Cass", "Menard", "Logan", "De Witt", "Piatt", "Champaign", "Vermilion", "Mason", "Schuyler", "Hancock", "McDonough", "Tazewell", "McLean", "Ford", "Fulton", "Will", "Cook"), "6a", zone)
         )

vernal <- read.csv("vernal_equinox.csv") %>% 
  rename(year = Year) %>% 
  mutate(Equinox = as.Date(Equinox, format = "%Y %B %d")) %>% 
  mutate(Equinox_j = yday(Equinox))

SBC_vernal <- SBC_zones %>% 
  left_join(vernal, by = join_by(year)) %>% 
  mutate(day_after_equi = julian_SBC - Equinox_j)

days_after_equi <- SBC_vernal %>% 
  select(year, day_after_equi)

days_after_equi <- unique(days_after_equi)

SBC_vernal %>% 
  filter(Common_Name == "Yellow-rumped Warbler",
         zone == "5") %>% 
  ggplot(aes(x = day_after_equi, y = Count)) +
  geom_point() +
  geom_smooth(
    method = "glm",
    color = "indianred",
    se = FALSE
  ) +
  theme_minimal()



subset_data <- SBC_vernal %>% 
  filter(Common_Name == "Yellow-rumped Warbler",
         zone == "5")

Model <- glm(GGD_1_50 ~ day_after_equi, family = gaussian, data = subset_data)
summary(Model)

### Determine which species have been observed the most in the past 50 years of the count
head <- SBC_all_data %>% 
  group_by(Common_Name) %>% 
  summarise(Count = sum(Count,na.rm = T)) %>% 
  ungroup() %>% 
  arrange(desc(Count)) %>% 
  head(50)

filtered_data <- SBC_vernal %>%
  group_by(Common_Name) %>%
  mutate(prop_zero = mean(Count == 0, na.rm = TRUE)) %>%   # proportion of zeros per species
  filter(prop_zero < 0.50) %>%                             # keep species with <99% zeros
  select(-c(prop_zero,class)) %>% 
  rename(class = zone) %>% 
  drop_na(Party_Hours) %>%  #Drop rows where the party hour variable is NA, indicating no coverage (May need to revisit this since some years where Party Hour = NA had data & observers)
  ungroup()

species <- unique(filtered_data$Common_Name)

view(species)

species <- c("Common Yellowthroat") #, "Blue Jay", "Yellow-rumped Warbler", "Northern Cardinal")

for (i in species) {
  
  message(paste0("Started ", i))
  
  sub <- filtered_data %>% 
    filter(Common_Name == i,
           day_after_equi %in% c(47,48,49,50)) %>% 
    group_by(year, class) %>% 
    summarise(mean = mean(Count),
              GGD_1_50_mean = mean(GGD_1_50)) %>% 
    ungroup()
  
  sub <- sub %>% 
    left_join(days_after_equi)
  
  ## ---- GET GAM SIGNIFICANCE ----
  glm_sig <- sub %>%
    group_by(class) %>%
    group_modify(~{
      m <- glm(mean ~ GGD_1_50_mean, data = .x)
      
      # Extract GLM summary table
      s <- summary(m)$coefficients
      
      # p-value for the predictor (2nd row, 4th column)
      p_value <- s[2, 4]
      
      tibble(
        class = unique(.x$class),
        p_value = p_value,
        signif_flag = ifelse(p_value < 0.05, "significant", "nonsignificant")
      )
    }) %>%
    ungroup()
  
  #gam_sig <- sub %>%
    #group_by(class) %>%
   # group_modify(~{
      #m <- glm(mean ~ day_after_equi, data = .x)
      #s <- summary(m)
      
      #tibble(
       # class = unique(.x$class),
        #p_value = s$s.table[1,4],
        #signif_flag = ifelse(p_value < 0.05, "significant", "nonsignificant")
      #)
    #}) %>%
    #ungroup()
  
  # Join significance back to sub
  sub_sig <- sub %>% left_join(glm_sig, by = "class")
  
  plot <- sub_sig %>% 
    ggplot(aes(x = GGD_1_50_mean, y = mean, color = class)) +
    geom_point() +
    geom_smooth(
      method = "glm",
      aes(linetype = signif_flag),   # <--- key line
      color = "indianred"
    ) +
    scale_linetype_manual(values = c("significant" = "solid",
                                     "nonsignificant" = "dashed")) +
    theme_minimal() +
    labs(title = paste(i, ": Mean Count by mean GDD in past 50 days")) +
    facet_wrap(~class, scales = "free_y")
  
  message("Plot 1 created")
  
  ## ---- GET GAM SIGNIFICANCE (Second model) ----
  gam_sig <- sub %>%
    group_by(class) %>%
    group_modify(~{
      m <- gam(GGD_1_50_mean ~ s(year), data = .x)
      s <- summary(m)
      
      tibble(
        class = unique(.x$class),
        p_value = s$s.table[1,4],
        signif_flag = ifelse(p_value < 0.05, "significant", "nonsignificant")
      )
    }) %>%
    ungroup()
  
  # Join significance back to sub
  sub_sig <- sub %>% left_join(gam_sig, by = "class")
  
  plot2 <- sub_sig %>% 
    ggplot(aes(x = year, y = GGD_1_50_mean, color = class)) +
    geom_point() +
    geom_line() +
    geom_smooth(
      method = "gam",
      aes(linetype = signif_flag),
      color = "indianred",
      se = FALSE
    ) +
    scale_linetype_manual(values = c("significant" = "solid",
                                     "nonsignificant" = "dashed")) +
    theme_minimal() +
    facet_wrap(~class, scales = "free_y")
  
  
  message("Plot 2 created")
  
  ## ---- GET GAM SIGNIFICANCE (Third model) ----
  gam_sig <- sub %>%
    group_by(class) %>%
    group_modify(~{
      m <- gam(mean ~ s(year), data = .x)
      s <- summary(m)
      
      tibble(
        class = unique(.x$class),
        p_value = s$s.table[1,4],
        signif_flag = ifelse(p_value < 0.05, "significant", "nonsignificant")
      )
    }) %>%
    ungroup()
  
  # Join significance back to sub
  sub_sig <- sub %>% left_join(gam_sig, by = "class")
  
  plot3 <- sub_sig %>% 
    ggplot(aes(x = year, y = mean, color = GGD_1_50_mean)) +
    geom_point() +
    geom_line() +
    geom_smooth(
      method = "gam",
      aes(linetype = signif_flag),
      color = "indianred",
      se = FALSE
    ) +
    scale_linetype_manual(values = c("significant" = "solid",
                                     "nonsignificant" = "dashed")) +
    theme_minimal() +
    scale_color_viridis_c(option = "plasma") +
    facet_wrap(~class, scales = "free_y")
  
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
  
  ggsave(paste0("GAM_",i,".png"), final_plot, width = 10, height = 12, dpi = 300, path = "C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Imgs/GAM_Plots_commonsp", create.dir = TRUE)
  message(i, " Done")
  
}

plot

##### Visualizing Zones
counties <- SBC_zones %>% 
  select(county, zone)

counties <- unique(counties)

eqdc <- '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

IL_counties <- usmap::us_map(regions = "counties") %>% 
  filter(abbr == "IL") %>% 
  mutate(county = ifelse(county == "LaSalle County", "La Salle", county)) %>% 
  st_transform(eqdc)

IL_counties$county <- gsub(" County","", IL_counties$county)

class <- left_join(IL_counties, counties,by = join_by(county))

ggplot() +
  #geom_sf(data = states, color = "gray55", lty = 1, size = .01) +
  geom_sf(data = class, aes(fill = zone), show.legend = T) +
  scale_fill_viridis_d(option = "D")+
  theme_void()
