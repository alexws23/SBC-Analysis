library(tidyverse)
library(visdat)
library(patchwork)
library(broom)
library(ggfortify)
library(ggtext)
library(ggpubr)
library(nortest)
library(lme4)

### Set working directory
setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

### Read in data
SBC_GDD <- read.csv(file = "SBC_GDD.csv") %>% 
  select(-c(X)) %>%  #Drop extraneous column
  filter(countyyear != "Champaign 2024",  #Remove Champaign 2024 since it is missing data
         county != "Douglas") #remove douglas county since it has no data

species <- c("Yellow-rumped Warbler", "Swainson's Thrush", "Veery", "White-throated Sparrow",
             "Dark-eyed Junco", "Tennessee Warbler", "Magnolia Warbler", "Bay-breasted Warbler",
             "Blackpoll Warbler", "Ruby-crowned Kinglet")

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

sub <- SBC_GDD %>% 
  filter(Common_Name == "Dark-eyed Junco")

plot <- SBC_GDD %>% 
  filter(Common_Name == "Dark-eyed Junco") %>% 
  ggplot(aes(x = year, y = GGD_1_25)) +
  geom_point() +
  geom_smooth(method = "glm") +
  theme_minimal()

ggsave("GGD_1_25.png", plot, width = 10, height = 12, dpi = 300, path = "C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Imgs/")

model2 <- glm(GGD_1_25 ~ year, family = gaussian, data = sub)
summary(model2)

model3 <- glm(GGD_1_25 ~ year + julian_SBC, family = gaussian, data = sub)
summary(model3)

unique(SBC_GDD$GGD_1_25)

SBC_GDD %>% 
  filter(GGD_1_25 == 0)
