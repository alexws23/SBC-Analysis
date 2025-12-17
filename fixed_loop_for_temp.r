library(tidyverse)
library(degday)

setwd("C:/Users/awsmilor/Git/Ward Lab/SBC-Analysis/Data")

# Read the SBC dates and filter years
SBC_dates <- read.csv("date of SBC.csv", header = TRUE, sep = ",") %>%
  filter(Year > 1980)

# Define thresholds for development (EPA standard)
thresh_low <- 50
thresh_up <- 100

# Get all county data files in the working directory that match the pattern "_2.csv"
data_files <- list.files(pattern = "_2\\.csv$", path = "./Raw Data")

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
  data <- read.csv(paste0("./Raw Data/",file), header = TRUE, sep = ",")
  
  # Calculate GDD metrics
  data_2 <- data %>%
    mutate(
      sng_tri = dd_sng_tri(daily_min = tmin..degrees.F., daily_max = tmax..degrees.F.,
                           thresh_low = thresh_low, thresh_up = thresh_up),
      sng_sine = dd_sng_sine(daily_min = tmin..degrees.F., daily_max = tmax..degrees.F.,
                             thresh_low = thresh_low, thresh_up = thresh_up)
    )
  
  # Add next day's tmin
  data_3 <- data_2 %>%
    mutate(tmin_next = lead(tmin..degrees.F., n = 1))
  
  # Calculate double-day metrics
  data_4 <- data_3 %>%
    mutate(
      dbl_tri = dd_dbl_tri(daily_min = tmin..degrees.F., daily_max = tmax..degrees.F., nextday_min = tmin_next,
                           thresh_low = thresh_low, thresh_up = thresh_up),
      dbl_sine = dd_dbl_sine(daily_min = tmin..degrees.F., daily_max = tmax..degrees.F., nextday_min = tmin_next,
                             thresh_low = thresh_low, thresh_up = thresh_up)
    )
  
  # Rename columns for consistency
  colnames(data_4) <- c("date", "precip", "tmin", "tmean", "tmax",
                        "sng_tri", "sng_sine", "tmin_next", "dbl_tri", "dbl_sine")
  
  # Convert date column and add year/julian
  data_4 <- data_4 %>%
    mutate(
      date = as.Date(date, format = "%m/%d/%Y"),
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
      avg_tmean <- mean(data_window$tmean, na.rm = TRUE)
      avg_tmin <- mean(data_window$tmin, na.rm = TRUE)
      avg_tmax <- mean(data_window$tmax, na.rm = TRUE)
      total_precip <- sum(data_window$precip, na.rm = TRUE)
      
      # Store results
      results <- data.frame(
        county = county_name,
        year = yr,
        julian_SBC = julian_SBC,
        window_days = window_days,
        GGD_1 = GGD_1,
        GGD_2 = GGD_2,
        avg_tmean = avg_tmean,
        avg_tmin = avg_tmin,
        avg_tmax = avg_tmax,
        total_precip = total_precip
      )
      
      all_results[[length(all_results) + 1]] <- results
    }
  }
  
  # Combine and pivot wider
  data_5 <- bind_rows(all_results) %>%
    pivot_wider(
      names_from = window_days,
      values_from = c(GGD_1, GGD_2, avg_tmean, avg_tmin, avg_tmax, total_precip),
      names_glue = "{.value}_{window_days}"
    ) %>%
    select(
      county, year, julian_SBC,
      GGD_1_100, GGD_1_50, GGD_1_25,
      GGD_2_100, GGD_2_50, GGD_2_25,
      avg_tmean_100, avg_tmean_50, avg_tmean_25,
      avg_tmin_100, avg_tmin_50, avg_tmin_25,
      avg_tmax_100, avg_tmax_50, avg_tmax_25,
      total_precip_100, total_precip_50, total_precip_25
    )
  
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
