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

# Define thresholds for development (EPA standard)
thresh_low <- 50
thresh_up <- 100

# Get all county data files in the working directory that match the pattern "_2.csv"
data_files <- list.files(pattern = "_2\\.csv$", path = "./gdd_data/")

# Create an empty list to store final results for all counties
final_all_counties <- list()

# Loop through each county file
for (file in data_files) {
  message("Processing file: ", file)
  
  # Extract the county name (e.g., "Adams" from "Adams_2.csv")
  county_name <- sub("_2\\.csv$", "", file)
  
  # Define output file name
  file_name <- paste0(county_name, "_FINAL.csv")
  
  # Read in the data
  data <- read.csv(paste0("./gdd_data/",file), header = TRUE, sep = ",")
  
  data <- data %>% 
    mutate(temperature_2m_max = (temperature_2m_max * (9/5))+32,
           temperature_2m_min = (temperature_2m_min * (9/5))+32)
  
  # Calculate GDD metrics
  data_2 <- data %>%
    mutate(
      sng_tri = dd_sng_tri(daily_min = temperature_2m_min, daily_max = temperature_2m_max,
                           thresh_low = thresh_low, thresh_up = thresh_up),
      sng_sine = dd_sng_sine(daily_min = temperature_2m_min, daily_max = temperature_2m_max,
                             thresh_low = thresh_low, thresh_up = thresh_up)
    )
  
  # Add next day's tmin
  data_3 <- data_2 %>%
    mutate(tmin_next = lead(temperature_2m_min, n = 1))
  
  # Calculate double-day metrics
  data_4 <- data_3 %>%
    mutate(
      dbl_tri = dd_dbl_tri(daily_min = temperature_2m_min, daily_max = temperature_2m_max, nextday_min = tmin_next,
                           thresh_low = thresh_low, thresh_up = thresh_up),
      dbl_sine = dd_dbl_sine(daily_min = temperature_2m_min, daily_max = temperature_2m_max, nextday_min = tmin_next,
                             thresh_low = thresh_low, thresh_up = thresh_up)
    )
  
  # Rename columns for consistency
  colnames(data_4) <- c("date", "tmax", "tmin", "latitude", "longitude", "location_id", "year",
                        "sng_tri", "sng_sine", "tmin_next", "dbl_tri", "dbl_sine")
  
  # Convert date column and add year/julian
  data_4 <- data_4 %>%
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
    
    data_sub <- data_4 %>% filter(year == yr)
    
    for (window_days in c(100, 50, 25)) {
      d_window <- julian_SBC - window_days
      
      data_window <- data_sub %>%
        filter(julian >= d_window & julian <= julian_SBC)
      
      # Compute metrics
      GGD_1 <- sum(data_window$dbl_tri, na.rm = TRUE)
      GGD_2 <- sum(data_window$dbl_sine, na.rm = TRUE)
      avg_tmin <- mean(data_window$tmin, na.rm = TRUE)
      avg_tmax <- mean(data_window$tmax, na.rm = TRUE)
      
      # Store results
      results <- data.frame(
        county = county_name,
        year = yr,
        julian_SBC = julian_SBC,
        window_days = window_days,
        GGD_1 = GGD_1,
        GGD_2 = GGD_2,
        avg_tmin = avg_tmin,
        avg_tmax = avg_tmax
      )
      
      all_results[[length(all_results) + 1]] <- results
    }
  }
  
  # Combine and pivot wider
  data_5 <- bind_rows(all_results) %>%
    pivot_wider(
      names_from = window_days,
      values_from = c(GGD_1, GGD_2, avg_tmin, avg_tmax),
      names_glue = "{.value}_{window_days}"
    ) %>%
    select(
      county, year, julian_SBC,
      GGD_1_100, GGD_1_50, GGD_1_25,
      GGD_2_100, GGD_2_50, GGD_2_25,
      avg_tmin_100, avg_tmin_50, avg_tmin_25,
      avg_tmax_100, avg_tmax_50, avg_tmax_25
    )
  
  ## Add latitude data
  tmp <- data %>% 
    select(c(location_id, latitude,longitude)) %>% 
    rename(county = location_id)
  
  data_5 <- data_5 %>% 
    left_join(coords, by = join_by(county))
  
  # Save the county-level output
  write.csv(data_5, file = file_name, row.names = FALSE)
  
  # Add to the combined list
  final_all_counties[[length(final_all_counties) + 1]] <- data_5
}

# Combine all counties into a single dataset
final_combined <- bind_rows(final_all_counties)

# Optionally, save one combined file
write.csv(final_combined, "AllCounties_FINAL.csv", row.names = FALSE)

message("✅ All counties processed and saved successfully!")
