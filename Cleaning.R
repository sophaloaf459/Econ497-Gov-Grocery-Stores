library(tidyverse)
library(tidyr)
library(readr) 



# Store Info -------------------------------------------------------------
#Latitude: 39.0601° N Or 39.06903775959004, -94.55448775544315
#Longitude: 94.5450° W
#Address: 3110 Prospect Ave, Kansas City, Missouri ----> OR 31st and Prospect
#Hours: 07:00 – 21:00
#Zip 64128 0r 64127

# Crime Cleaning ----------------------------------------------------------
#formatting date and time
KCPD_Reported_Crime_Data <- read_csv("KCPD_Reported_Crime_Data.csv", 
          col_types = cols(Reported_Date = col_datetime(format
          = "%Y %b %d %I:%M:%S %p"), 
          Reported_Time = col_time(format = "%H:%M:%S")))

                  
#seperating date, to year month day
KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |> 
  separate(Reported_Date , 
           into = c("year", "month", "day"),
           sep = "-", remove = FALSE) |> 
  mutate(across(c(year, month, day), as.numeric))


#removing columns not needed & rename
KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |> 
  filter(year>2014,Reported_Date>2014) |> 
  select(-Suspect_Count,-Victim_Count,-Arrestee_Count,-Deceased_Count)

KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |>
  select(-Location,-City,-Area)

KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |> select(-IBRS)  

KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |>
  rename(zip= Zip_Code)

KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |>
 rename(date = Reported_Date, time = Reported_Time)


# Unemployment Cleaning ---------------------------------------------------
KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  select(-AreaCode,-AreaName,-PeriodType,-SeasonalAdjustment,-RowType) 
  
KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  select(-Period)

KC_unemp_data_monthly <- KC_unemp_data_monthly %>%
  mutate(Month = match(as.character(Month), month.name))
# as.character() converts the month column to plain text first
# This ensures match() can work with it properly
# Then match() finds the position in month.name (1-12)

#renaming
KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  rename(month = Month, year = Year)


# Population Cleaning -----------------------------------------------------

#get correct range of zips
MS_population_by_zip_and_year <- MS_population_by_zip_and_year |>
  filter(zip %in% 64101:64192) 


#identify zips we dont want
bad_zips <- c(64103,64104,64107,64115,64118,64121,64122,64135,64138,64140:64144,64148,
              64150,64150,64152,64159:64164,64166:64191)

#see what bad zips are in data set
intersect(bad_zips, MS_population_by_zip_and_year$zip)

#remove zips that are bad
cleaned_pop_by_zip_and_year <- MS_population_by_zip_and_year |>
  filter(!zip %in% c(64118,64138,64150,64152,64161,64163,64164,64166,64167))

# Store Hours organizing --------------------------------------------------

crime_data_sh_nsh <- KCPD_Reported_Crime_Data |> 
  
  # mutate() creates new columns while keeping all existing columns
  mutate(
    
    # Create first column: crime_sh (store hours crime indicator)
    crime_sh = case_when(
      # Check if the hour from 'time' column is >= 7 AND < 21 (7am to 8:59pm)
      hour(time) >= 7 & hour(time) < 21 ~ 1,  # If TRUE, assign 1
      TRUE ~ 0  # Otherwise (for all other cases), assign 0
    ),
    
    # Create second column: crime_nsh (non-store hours crime indicator)
    crime_nsh = case_when(
      # Check if the hour is < 7 OR >= 21 (before 7am or from 9pm onwards)
      time < "07:00" | hour(time) >= 21 ~ 1,  # If TRUE, assign 1
      TRUE ~ 0  # Otherwise, assign 0
    )
  )

# Key functions explained:
# hour() extracts just the hour (0-23) from a time object
# & means AND - both conditions must be true
# | means OR - at least one condition must be true
# case_when() is like multiple if-else statements
# ~ separates the condition from the result
# TRUE ~ 0 is the "catch-all" else clause

# Result: crime_sh and crime_nsh are mutually exclusive binary indicators
# Each row will have exactly one column = 1 and the other = 0

crime_data_sh_nsh |> 
  group_by(year) |> 
  summarise(
    count_sh = sum(crime_sh),
    count_nsh = sum(crime_nsh),
    total = n()
  )

# Organize crime by violient/non violeint ---------------------------------
crime_data_sh_nsh <- crime_data_sh_nsh |> 
  mutate(
    violent = as.numeric(Violent_Flag),           # TRUE becomes 1, FALSE becomes 0
    non_violent = as.numeric(!Violent_Flag)       # ! flips it: FALSE becomes 1, TRUE becomes 0
  )
  


# Merging Data ------------------------------------------------------------

# Merge all 3 datasets
merged_data <- crime_data_sh_nsh |> 
  left_join(cleaned_pop_by_zip_and_year, by = c("year", "zip")) |> 
  left_join(KC_unemp_data_monthly, by = c("year", "month"))



# Distance From Store -----------------------------------------------------

reference_lat <- 39.06903775959004
reference_long <- -94.55448775544315

merged_data <- merged_data |> 
  mutate(
    crime_distance_fs = sqrt((Latitude - reference_lat)^2 + 
                               (Longitude - reference_long)^2)
  )

#Miles
merged_data <- merged_data |> 
  mutate(
    # Calculate difference in lat and long
    lat_diff = Latitude - 39.06903775959004,
    long_diff = Longitude - (-94.55448775544315),
    
    # Convert each to miles (1 deg lat ≈ 69 miles, 1 deg long ≈ 54 miles at 39°N)
    lat_miles = lat_diff * 69,
    long_miles = long_diff * 54,
    
    # Calculate distance in miles using Pythagorean theorem
    crime_distance_miles_fs = sqrt(lat_miles^2 + long_miles^2)
  )


# Interactive Map -------------------------------------------------------------------------

library(leaflet)

# Get unique years
years <- unique(merged_data$year)

# Create a map for each year
for(y in years) {
  map <- merged_data |> 
    filter(crime_distance_miles_fs <= 5, year == y) |> 
    leaflet() |> 
    addTiles() |> 
    addCircleMarkers(
      lng = ~Longitude, 
      lat = ~Latitude,
      radius = 3,
      color = ~colorNumeric("RdYlBu", crime_distance_miles_fs)(crime_distance_miles_fs),
      popup = ~paste("Distance:", round(crime_distance_miles_fs, 2), "miles")
    ) |> 
    addMarkers(
      lng = -94.55448775544315, 
      lat = 39.06903775959004,
      popup = "Reference Point"
    )
  
  print(paste("Map for year:", y))
  print(map)
}

# OR create one interactive map with a layer for each year:
library(leaflet)

merged_data_filtered <- merged_data |> 
  filter(crime_distance_miles_fs <= 5)

# Create base map
map <- leaflet() |> 
  addTiles() |> 
  addMarkers(
    lng = -94.55448775544315, 
    lat = 39.06903775959004,
    popup = "Reference Point"
  )

# Add a layer for each year
for(y in sort(unique(merged_data_filtered$year))) {
  year_data <- merged_data_filtered |> filter(year == y)
  
  map <- m |> 
    addCircleMarkers(
      data = year_data,
      lng = ~Longitude, 
      lat = ~Latitude,
      radius = 3,
      group = as.character(y),
      color = ~colorNumeric("RdYlBu", crime_distance_miles_fs)(crime_distance_miles_fs),
      popup = ~paste("Year:", year, "<br>Distance:", round(crime_distance_miles_fs, 2), "miles")
    )
}

# Add layer control so you can toggle years on/off
map |> addLayersControl(
  overlayGroups = as.character(sort(unique(merged_data_filtered$year))),
  options = layersControlOptions(collapsed = FALSE)
)

rm(m)