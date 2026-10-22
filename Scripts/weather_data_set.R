library(dplyr)
library(readr)
library(lubridate)

# Define metadata and file paths
stations <- list(
  ENG = list(
    name = "Engelberg", elevation = 1036,
    lat = 46.821639, lon = 8.410514,
    region = "Central Alpine north slope", canton = "OW",
    file = "data/raw/Engelberg_weather_data.csv"
  ),
  LUZ = list(
    name = "Luzern", elevation = 454,
    lat = 47.036439, lon = 8.301022,
    region = "Central plateau", canton = "LU",
    file = "data/raw/Luzern_weather_data.csv"
  ),
  MER = list(
    name = "Meiringen", elevation = 589,
    lat = 46.732222, lon = 8.169247,
    region = "Western Alpine north slope", canton = "BE",
    file = "data/raw/Meiringen_weather_data.csv"
  )
)


# Function to load, filter, and annotate weather data
process_station <- function(station) {
  df <- read_delim(station$file, delim = ";", show_col_types = FALSE)
  
  # Check if "date" column exists
  if (!"date" %in% names(df)) {
    stop(paste("No 'date' column found in file:", station$file))
  }
  
  # Filter and enrich data
  df <- df %>%
    mutate(date = ymd(date)) %>%
    filter(date >= as.Date("2024-11-01") & date <= as.Date("2024-11-30")) %>%
    mutate(
      station_name = station$name,
      station_id = station$id,
      wmo = station$wmo,
      start_date = station$start_date,
      elevation = station$elevation,
      x = station$x,
      y = station$y,
      lat = station$lat,
      lon = station$lon,
      region = station$region,
      canton = station$canton
    )
  
  return(df)
}

# Process all stations and combine into one dataframe
all_data <- bind_rows(lapply(stations, process_station))

# Rename columns to more readable names
all_data <- all_data %>%
  rename(
    station_code      = `station/location`,
    date              = date,
    solar_radiation_wm2 = gre000d0,
    snow_depth_cm     = hto000d0,
    cloud_cover_pct   = nto000d0,
    pressure_hpa      = prestad0,
    precip_mm         = rre150d0,
    sunshine_min      = sre000d0,
    temp_avg_c        = tre200d0,
    temp_min_c        = tre200dn,
    temp_max_c        = tre200dx,
    humidity_pct      = ure200d0
  )


# View result
print(all_data)

# Write to CSV
write_csv(all_data, "weather_november_2024_combined.csv")
