library(tidyverse)
library(visdat)
library(patchwork)
library(broom)
library(ggfortify)
library(ggtext)
library(ggpubr)
library(nortest)
library(lme4)
library(quantreg)
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

unique(SBC_GDD$county)

ph <- read.csv(file = "PH_over_time_2024.csv") %>% 
  select(c(Counties, Species, Birds, Observers, Parties, Party_Hours, year)) %>% 
  rename(county = Counties) %>% 
  mutate(county = ifelse(county == "De.Kalb", "DeKalb", county),
         county = ifelse(county == "De.Witt", "De Witt", county),
         county = ifelse(county == "La.Salle", "La Salle", county),
         county = ifelse(county == "Rock.Island", "Rock Island", county),
         county = ifelse(county == "St..Clair", "St. Clair", county),
         county = ifelse(county == "Jo.Daviess", "Jo Daviess", county),
         county = ifelse(county == "Vermilion", "Vermillion", county),
         county = ifelse(county == "Du.Page", "DuPage", county)
  ) %>%
  mutate(countyyear = paste0(county, " ",year)) %>% 
  select(-c(county, year))
  
unique(ph$county)

SBC_GDD <- SBC_GDD %>% 
  left_join(ph, by = join_by(countyyear))

vis_dat(slice_sample(SBC_GDD, n = 10000))

vis <- SBC_GDD %>% 
  filter(is.na(Party_Hours))

view(vis)

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

Variable <- c("latitude")

#, "GGD_1_50", "GGD_1_100", "GGD_2_25", "GGD_2_50", "GGD_2_100")

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
  #st <- shapiro.test(var_data)
  hist_resid <- gghistogram(Model$residuals)
  hist_var <- gghistogram(var_data)
  hist_resid <- hist_resid + 
    labs(title = "Histogram of Residual Distribution")
  
  # Create GG plot scatter plot with a glm visualized
  p1 <- ggplot(subset_data, aes_string(x = j, y = "Count")) +
    geom_point(aes(color = avg_tmax_25)) +
    geom_smooth(method = "glm", se = TRUE, color = "blue") +
    theme_minimal()+ 
    labs(title = paste0("GLM: Count ~", j," for ", i),
         subtitle = paste("p-value:", signif(coef(summary(Model))[2,4], 3)))+ 
    scale_color_viridis_c(option = "magma")
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
    "AIC:", round(model_summary$aic, 2), "\n"
    #"SW Test statistic:", round(st$statistic, 3), "\n",
    #"SW p-value:", signif(st$p.value, 3)
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

Cook <- SBC_GDD %>% 
  filter(county == "Cook")

q <- SBC_GDD %>% 
  drop_na(Party_Hours) %>% 
  summarise(median = median(Party_Hours))

ql <- SBC_GDD %>% 
  drop_na(Party_Hours) %>%
  filter(Party_Hours < 31.5) %>% 
  summarise(ql = median(Party_Hours))

qu <- SBC_GDD %>% 
  drop_na(Party_Hours) %>%
  filter(Party_Hours > 31.5) %>% 
  summarise(ql = median(Party_Hours))


mid <- SBC_GDD %>% 
  filter(Common_Name == "American Robin") %>% 
  left_join(ql, by = join_by(class)) %>% 
  left_join(qu, by = join_by(class)) %>%
  group_by(class) %>% 
  drop_na() %>%
  filter(Count > ql,
         Count < qu)



sub <- filtered_data %>% 
  slice_sample(prop = .1)

sub %>% 
  group_by(class) %>% 
  summarise(mean = mean(GGD_1_25_mean))

######################
##### For loop for final plots

for (i in species) {
  
  message(paste0("Started ", i))
  
  sub <- test %>% 
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

final_plot

plot <- test %>% 
  filter(Common_Name == "Carolina Chickadee") %>% 
  group_by(year, class) %>% 
  summarise(mean = mean(Count),
            GGD_1_25 = mean(GGD_1_25)) %>%
  ggplot(aes(x = GGD_1_25, y = mean, color = class)) +
  geom_point() +
  geom_smooth(method = "gam", color = "indianred") +
  theme_minimal() + 
  facet_wrap(~class)

plot

plot2 <- test %>% 
  filter(Common_Name == "Tennessee Warbler") %>% 
  mutate(
    avg_temp = (avg_tmin_100 + avg_tmax_100) / 2
  ) %>% 
  group_by(year, class) %>% 
  summarise(mean = mean(Count),
            GGD_1_25 = mean(GGD_1_25)) %>%
  ggplot(aes(x = year, y = GGD_1_25, color = class)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", color = "indianred", se = F) +
  geom_smooth(method = "lm", color = "goldenrod1", se = F) + 
  theme_minimal()+
  facet_wrap(~class)

plot2

plot3 <- test %>% 
  filter(Common_Name == "Tennessee Warbler") %>% 
  group_by(year, class) %>% 
  summarise(mean = mean(Count),
            avg_tmin_100 = mean(avg_tmin_100)) %>% 
  ggplot(aes(x = year, y = mean, color = avg_tmin_100)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "gam", color = "indianred", se = F) +
  geom_smooth(method = "lm", color = "goldenrod1") + 
  theme_minimal() +
  facet_wrap(~class)

plot3

model_prep <- test %>% 
  filter(Common_Name == "Tennessee Warbler",
         class == 2) %>% 
  group_by(year) %>% 
  summarise(mean = mean(Count),
            GGD_1_25 = mean(GGD_1_25)) %>% 
  ungroup

gam_model <- gam(mean ~ GGD_1_25, data = model_prep)

summary(gam_model)

plot / plot2 + plot3

test <- filtered_data %>% 
  drop_na(Party_Hours)# %>% 
  #filter(Party_Hours > 11.5)

test %>% 
  filter(Count ==0) %>% 
  nrow()

1252/118900

6408/865400

70756/118900

358463/865400

filtered_data <- SBC_GDD %>%
  group_by(Common_Name) %>%
  mutate(prop_zero = mean(Count == 0, na.rm = TRUE)) %>%   # proportion of zeros per species
  filter(prop_zero < 0.95) %>%                             # keep species with <95% zeros
  select(-prop_zero)                                       # optional: remove helper column

unique(filtered_data$Common_Name)
n_distinct(filtered_data$Common_Name)

ggsave("GGD_1_25.png", plot, width = 10, height = 12, dpi = 300, path = "C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Imgs/")

model2 <- glm(log(Count) ~ GGD_1_25, family = gaussian, data = sub)
summary(model2)

autoplot(model2, which = 2)

model3 <- glm(GGD_1_25 ~ year + julian_SBC, family = gaussian, data = sub)
summary(model3)

unique(SBC_GDD$GGD_1_25)

SBC_GDD %>% 
  filter(GGD_1_25 == 0)
