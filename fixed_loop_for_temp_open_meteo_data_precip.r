library(tidyverse)
library(degday)
library(visdat)

setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

# Read the SBC dates and filter years
SBC_dates <- read.csv("date of SBC.csv", header = TRUE, sep = ",") %>%
  filter(Year > 1972)

coords <- read.csv("./County_Data.csv") %>% 
  select(c(County, Latitude, Longitude)) %>% 
  mutate(Longitude = Longitude * -1) %>%
  rename(
    county = County,
    latitude = Latitude,
    longitude = Longitude
  )

# Get all county data files in the working directory that match the pattern "_2.csv"
data_files <- list.files(pattern = "_2\\.csv$", path = "./precip_outputs/")

# Create an empty list to store final results for all counties
final_all_counties <- list()

# Loop through each county file
for (file in data_files) {
  message("Processing file: ", file)
  
  # Extract the county name (e.g., "Adams" from "Adams_2.csv")
  county_name <- sub("_2\\.csv$", "", file)
  
  # Define output file name
  file_name <- paste0("precip_final/",county_name, "_FINAL_precip.csv")
  
  # Read in the data
  data <- read.csv(paste0("./precip_outputs/",file), header = TRUE, sep = ",")
  
  # Rename columns for consistency
  colnames(data) <- c("date", "precipitation", "rain", "snowfall",
                        "latitude", "longitude", "location_id", "year")
  
  # Convert date column and add year/julian
  data <- data %>%
    mutate(
      date = as.Date(date),
      julian = as.numeric(format(date, "%j")),
      year = year(date)
    )
  
  # Initialize results list for this county
  all_results <- list()
  
  # Loop over SBC years
  for (i in 1:nrow(SBC_dates)) {
    yr <- SBC_dates$Year[i]
    SBC_date <- SBC_dates$Date_of_SBC[i]
    SBC <- as.POSIXct(SBC_date, format = "%m/%d/%Y")
    julian_SBC <- as.numeric(format(SBC, "%j"))
    
    data_sub <- data %>% filter(year == yr)
    
    for (window_days in c(100, 50, 25)) {
      d_window <- julian_SBC - window_days
      
      data_window <- data_sub %>%
        filter(julian >= d_window & julian <= julian_SBC)
      
      # Compute metrics
      total_precip <- sum(data_window$precipitation, na.rm = TRUE)
      total_rain <- sum(data_window$rain, na.rm = TRUE)
      total_snow <- mean(data_window$snowfall, na.rm = TRUE)
      
      # Store results
      results <- data.frame(
        county = county_name,
        year = yr,
        julian_SBC = julian_SBC,
        window_days = window_days,
        total_precip = total_precip,
        total_rain = total_rain,
        total_snow = total_snow
      )
      
      all_results[[length(all_results) + 1]] <- results
    }
  }
  
  # Combine and pivot wider
  data_2 <- bind_rows(all_results) %>%
    pivot_wider(
      names_from = window_days,
      values_from = c(total_precip, total_rain, total_snow),
      names_glue = "{.value}_{window_days}"
    ) %>%
    select(
      county, year, julian_SBC,
      total_precip_100, total_precip_50, total_precip_25,
      total_rain_100, total_rain_50, total_rain_25,
      total_snow_100, total_snow_50, total_snow_25
    )
  
  
  data_3 <- data_2 %>% 
    left_join(coords, by = join_by(county))
  
  # Save the county-level output
  write.csv(data_3, file = file_name, row.names = FALSE)
  
  # Add to the combined list
  final_all_counties[[length(final_all_counties) + 1]] <- data_3
}

# Combine all counties into a single dataset
final_combined <- bind_rows(final_all_counties)

# Optionally, save one combined file
write.csv(final_combined, "precip_final/AllCounties_FINAL.csv", row.names = FALSE)

message("✅ All counties processed and saved successfully!")
