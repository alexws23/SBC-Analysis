library(tidyverse)
library(visdat)
library(shiny)
library(bslib)

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

##### Read in county specific data on tree cover loss in the US for each year since 2001 from Global Forest Watch
tree_us <- read.csv("./tree_cover_loss.csv")

colnames(tree_us) <- gsub("tc_loss_ha_", "", colnames(tree_us))

x <- 1:15

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




##### Merge Datasets
merge <- left_join(SBC, temp, by = join_by(countyyear)) %>% 
    filter(year >1980)

merge %>% 
  write.csv(file = "SBC_GDD.csv")

merge %>% 
  select(c(Common_Name,SCI_NAME)) %>% 
  distinct(Common_Name) %>% 
  write.csv(file = "SBC_species.csv")

merge_all <- left_join(merge, tree_il, by = join_by(countyyear))

vis_dat(slice_sample(merge, n = 2000))

vis <- merge %>% 
  filter(is.na(avg_tmean_100))

unique(vis$county)
  
SBC %>% 
  filter(Common_Name == "Sora") %>% 
  group_by(year) %>%
  drop_na() %>% 
  summarise(
    Count = mean(Count)
    #avg_tmean_100 = mean(avg_tmean_100)
  ) %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = Count)) +
  geom_smooth(aes(y = Count), method = "loess") +
  theme_minimal()

Model <-  glm(Count ~ avg_tmean_25+ GGD_1_25 + GGD_2_25 + total_precip_25, family = "gaussian", data = merge %>% filter(Common_Name == "Dark-eyed Junco"))
summary(Model)

  
  ##########################
# Define UI for miles per gallon app ----
ui <- page_sidebar(
  
  # App title ----
  title = "Spring Bird Count Trent",
  
  # Sidebar panel for inputs ----
  sidebar = sidebar(
    
    # Input: Selector for variable to plot against mpg ----
    selectInput(
      "variable",
      "Variable:",
      c(
        "Average Max Temp" = "",
        "Average Min Temp" = "avg_tmin_25",
        "Precipitation" = "total_precip_25"
      )
    ),
    
    # Input: Checkbox for whether outliers should be included ----
    checkboxInput("outliers", "Show outliers", TRUE)
  ),
  
  # Output: Formatted text for caption ----
  h3(textOutput("caption")),
  
  # Output: Plot of the requested variable against mpg ----
  plotOutput("crowPlot")
)

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$mpgPlot functions
  formulaText <- reactive({
    paste("mpg ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against mpg ----
  # and only exclude outliers if requested
  output$crowPlot <- renderPlot({
    boxplot(
      as.formula(formulaText()),
      data = mpgData,
      outline = input$outliers,
      col = "#75AADB",
      pch = 19
    )
  })
}

# Create Shiny app ----
shinyApp(ui, server)