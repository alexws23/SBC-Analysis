library(httr)
library(jsonlite)
library(dplyr)

setwd("~/Desktop/CampusCluster/shared/SBC_analyses/For_Linux_Server")
getwd

# --- Read and prep site locations ---
SiteLocations <- read.csv("./County_Data.csv") %>% 
  select(c(County, Latitude, Longitude)) %>% 
  mutate(Longitude = Longitude * -1) %>%
  rename(
    location_id = County,
    latitude = Latitude,
    longitude = Longitude
  ) %>% 
  filter(location_id %in% c("Christian", "Cook", "Champaign", "Shelby", "Livingston", "DuPage", "Menard", "Madison","Fulton","Mercer","Cumberland","Jackson", "Edwards", "Champaign", "Lake", "St. Clair","Stark","Stephenson","Tazewell","Union", "Vermilion","Wabash","Warren","Washington","Wayne","White","Whiteside","Will","Williamson","Winnebago","Woodford"
))

# --- Function to get daily weather data ---
get_weather_data <- function(lat, lon, start_date, end_date, variables = c("precipitation_sum", "rain_sum", "snowfall_sum")) {
  base_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  query <- list(
    latitude = lat,
    longitude = lon,
    start_date = start_date,
    end_date = end_date,
    daily = paste(variables, collapse = ","),
    timezone = "auto"
  )
  
  response <- GET(base_url, query = query)
  
  if (response$status_code == 200) {
    content_text <- content(response, "text")
    if (nchar(content_text) == 0) {
      warning(paste("Empty response for", lat, lon))
      return(NULL)
    }
    content <- fromJSON(content_text, flatten = TRUE)
    if (!is.null(content$daily)) {
      df <- as.data.frame(content$daily)
      df$latitude <- lat
      df$longitude <- lon
      return(df)
    } else {
      warning(paste("No daily data for", lat, lon))
      return(NULL)
    }
  } else {
    warning(paste("Failed for", lat, lon, "- Status code:", response$status_code))
    return(NULL)
  }
}

# --- Parameters ---
years <- 1973:2024
daily_vars <- c("precipitation_sum", "rain_sum", "snowfall_sum")
output_dir <- "R:/SBC_analyses/For_Linux_Server/precip_outputs"
if (!dir.exists(output_dir)) dir.create(output_dir)

# --- Rate Limiting Setup ---
max_per_hour <- 2000
max_per_day <- 4000
batch_size <- 300
pause_between_batches <- 60  # seconds
hourly_counter <- 0
daily_counter <- 0
hour_start <- Sys.time()
day_start <- Sys.Date()

# --- Main Loop ---
for (i in 1:nrow(SiteLocations)) {
  row <- SiteLocations[i, ]
  all_years_df <- data.frame()  # empty df to store all years for this site
  
  for (year in years) {
    
    # Check daily limit
    if (daily_counter >= max_per_day) {
      message("Reached daily limit of ", max_per_day, " requests. Stopping.")
      break
    }
    
    # Reset hourly counter if an hour has passed
    if (difftime(Sys.time(), hour_start, units = "hours") >= 1) {
      hourly_counter <- 0
      hour_start <- Sys.time()
      message("Hourly counter reset.")
    }
    
    # If hourly limit reached, wait until next hour
    if (hourly_counter >= max_per_hour) {
      wait_time <- 3600 - as.numeric(difftime(Sys.time(), hour_start, units = "secs"))
      message("Reached hourly limit. Waiting ", round(wait_time / 60, 1), " minutes...")
      Sys.sleep(wait_time)
      hourly_counter <- 0
      hour_start <- Sys.time()
    }
    
    # --- Set dates for that year ---
    start_date <- paste0(year, "-01-01")
    end_date <- paste0(year, "-05-10")
    
    # --- Perform API request ---
    df <- get_weather_data(
      lat = row$latitude,
      lon = row$longitude,
      start_date = start_date,
      end_date = end_date,
      variables = daily_vars
    )
    
    if (!is.null(df)) {
      df$location_id <- row$location_id
      df$year <- year
      all_years_df <- bind_rows(all_years_df, df)
      message("Added data for ", row$location_id, " (", year, ")")
    } else {
      warning("No data for ", row$location_id, " in ", year)
    }
    
    # --- Update counters ---
    hourly_counter <- hourly_counter + 1
    daily_counter <- daily_counter + 1
    
    # --- Pause every 2 requests ---
    if ((hourly_counter %% batch_size) == 0) {
      message("Pausing for ", pause_between_batches, " seconds to avoid rate limit...")
      Sys.sleep(pause_between_batches)
    }
  }
  
  # --- Save combined CSV for this county ---
  if (nrow(all_years_df) > 0) {
    file_name <- paste0(row$location_id, "_2.csv")
    file_path <- file.path(output_dir, file_name)
    write.csv(all_years_df, file_path, row.names = FALSE)
    message("✅ Saved combined file for ", row$location_id, ": ", file_path)
  } else {
    warning("⚠️ No data collected for ", row$location_id)
  }
}
