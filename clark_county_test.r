library(tidyverse)
library(visdat)
library(patchwork)
library(broom)
library(ggfortify)
library(ggtext)
library(ggpubr)
library(nortest)
library(lme4)

############ To-do:
############ 1. Pivot Bird data to have a row for counties instead of separate columns for each county
############ 2. Make a shiny app that allows you to see the population trend overtime for any species.
############ 3. Maybe consider adding growing degrees data
############ 4. Run GLMS to see if the growing degree data has any effect (would suggest earlier migration of things like DEJU)


setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

# Data Preparation
##### Read in soil temperature for all counties in illinois.
##### !!!!!!!! Missing Douglas County and 2024 soil temp data for Champaign
temp <- read.csv("./AllCounties_FINAL.csv") %>% 
  mutate(countyyear = paste0(county, " ",year)) %>% 
  select(-c(year, county))

##### Read in the SBC bird data
df <- read.csv("./SBC_Data_PH_2024.csv") %>% 
  rename(DeKalb = De.Kalb,
         DeWitt = De.Witt,
         Dupage = Du.Page,
         'StClair' = St..Clair,
         'RockIsland' = Rock.Island,
         'JoDaviess' = Jo.Daviess,
         LaSalle = La.Salle,
         Vermillion = Vermilion)

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

##### Merge Datasets
### Merge the soil temperature and growing degree days data with the SBC data
Cook <- SBC %>% 
  filter(county == "Cook")

merge <- left_join(Cook, temp, by = join_by(countyyear)) %>% 
  filter(year >1972) #Filter to exclude years without growing degree data

### Write a CSV with this information
merge %>% 
  write.csv(file = "Cook_GDD.csv")

merge %>% 
  filter(Common_Name == "Chimney Swift") %>% 
  ggplot(aes(x = GGD_1_25, y = Count))+
  geom_point() +
  geom_smooth(method = "glm", se = TRUE, color = "blue") +
  theme_minimal()

average_gdd <- merge %>% 
  summarise(mean = mean(GGD_1_25))

annual_gdd <- merge %>% 
  mutate(GGD_1_25_resid = GGD_1_25 - 112)

annual_gdd %>% 
  filter(Common_Name == "Yellow-rumped Warbler") %>% 
  ggplot(aes(x = GGD_1_25_resid, y = Count))+
  geom_point() +
  geom_smooth(method = "glm", se = TRUE, color = "blue") +
  theme_minimal()

Model <- glm(GGD_1_25 ~ year, family = gaussian, data = annual_gdd %>% filter(Common_Name == "Ruby-crowned Kinglet"))
summary(Model)

autoplot(Model, which = 2)

gghistogram(Model$residuals)

species <- c("Yellow-rumped Warbler", "Swainson's Thrush", "Veery", "White-throated Sparrow",
             "Dark-eyed Junco", "Tennessee Warbler", "Magnolia Warbler", "Bay-breasted Warbler",
             "Blackpoll Warbler", "Ruby-crowned Kinglet")

species <- c("Eastern Towhee", "Brown Thrasher")

Variable <- c("GGD_1_25", "GGD_1_50", "GGD_1_100", "GGD_2_25", "GGD_2_50", "GGD_2_100")

for (i in species) {
  # Filter and subset data
  subset_data <- SBC_GDD %>% filter(Common_Name == i)
  message(i, " Begun")
  for (j in Variable) {
    message("Running model for ", j)
    
    # Proper formula construction
    formula <- as.formula(paste("Count ~", j))
  # Create Model
  Model <- glm(formula, family = gaussian, data = subset_data)
  model_summary <- summary(Model)
  message("Model Generated")
  
  var_data <- subset_data[[j]]
  st <- shapiro.test(var_data)
  hist_resid <- gghistogram(Model$residuals)
  hist_var <- gghistogram(var_data)
  hist_resid <- hist_resid + 
    labs(title = "Histogram of Residual Distribution")
  
  # Create GG plot scatter plot with a glm visualized
  p1 <- ggplot(subset_data, aes_string(x = j, y = "Count")) +
    geom_point() +
    geom_smooth(method = "glm", se = TRUE, color = "blue") +
    theme_minimal()+ 
    labs(title = paste0("GLM: Count ~", j," for ", i),
         subtitle = paste("p-value:", signif(coef(summary(Model))[2,4], 3)))
  message("Scatterplot Generated")
  
  #Create diagnostic plots
  p2 <- autoplot(Model, which = 1)
  p2 <- p2 +
    theme_minimal()
  p3 <- autoplot(Model, which = 2)
  p3 <- p3 +
    theme_minimal()
  message("Diagnostic Plots Generated")
  
  #Create label of important values from the  GLM model and create a ggplot object with that
  coefs <- broom::tidy(Model)
  stats_text <- paste(
    "Intercept:", round(coefs$estimate[1], 3), "\n",
    "Slope:", round(coefs$estimate[2], 3),"\n",
    "p-value:", signif(coefs$p.value[2], 3),"\n",
    "AIC:", round(model_summary$aic, 2), "\n",
    "SW Test statistic:", round(st$statistic, 3), "\n",
    "SW p-value:", signif(st$p.value, 3)
  )
  
  p_text <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = stats_text, size = 5, hjust = 0.5) +
    theme_void() + 
    theme(plot.background = element_rect(fill = "white"))
  
  final_plot <- p2 + p3 / hist_resid / p1 + p_text
  
  ggsave(paste0("GLM_",j,"_",i,".png"), final_plot, width = 10, height = 12, dpi = 300, path = "C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Imgs/")
  message(i," + ",j, " Done")
  }
}
