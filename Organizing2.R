library(tidyverse)
library(dplyr)
library(lubridate)


#Organizing unemployment data
New_KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  filter(Year>2014) |> 
  select(-Month, -PeriodType, -RowType,-AreaCode,-AreaName) |> 
  rename(month = Period) 

#UnemplClean
unemp_clean <- New_KC_unemp_data_monthly %>%
  rename(
    year = Year,
    unemployment_rate = UnemploymentRate,
    employment = Employment,
    unemployment = Unemployment,
    labor_force = LaborForce
  ) |> 
  select(-SeasonalAdjustment)

str(unemp_clean)

unemp_clean %>%
  count(year, month) %>%
  filter(n > 1)

unemp_clean <- unemp_clean %>%
  mutate(month = as.integer(month))

str(unemp_clean)


#organizing crime data
New_KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data %>%
  mutate(
    Reported_Date = parse_date_time(
      Reported_Date,
      orders = "Y b d I:M:S p"
    ),
    Date  = as_date(Reported_Date),
    year  = year(Date),
    month = month(Date),
    day   = day(Date)   # ← THIS is what you want
  ) %>%
  select(-Reported_Date,-Date)

crime <- New_KCPD_Reported_Crime_Data %>%
  rename(
    description = Description,
    zipcode     = Zip_Code,
    geoloc     =  GeoLoc,
    address    =  Address,
    )

crime <- crime %>%
  mutate(
    
    description = replace_na(as.character(description), ""),
    description = str_to_lower(str_squish(description)),
    
    description = str_to_lower(str_squish(as.character(description))),
    robbery = as.integer(str_detect(description, "robbery|rob|robery")),
    burglary = as.integer(str_detect(description, "burglary|burglar|burgalary|burg")),
    property_damage = as.integer(str_detect(description, "property damage|vandal|vandalism|destruction|damage")),
    assault =         as.integer(str_detect(description, "assault|aggravated assault|agg")),
    stealing = as.integer(str_detect(description, "steal|stealing|theft|stolen|larceny|shoplift|shoplifting|shop")),
    other = as.integer(str_detect(description, "other|trespass|disorderly|swindle|false pretenses|forgery|counterfeit|disorderly|intimidation|offenses"))
  )

crime <- crime %>%
  mutate(
    incident_total = as.integer((robbery + burglary + property_damage + stealing + assault + other) > 0)
  )

crime_monthly <- crime %>%
  group_by(year, month, day, zipcode, geoloc, address) %>%
  summarise(
    robbery         = sum(robbery, na.rm = TRUE),
    burglary        = sum(burglary, na.rm = TRUE),
    property_damage = sum(property_damage, na.rm = TRUE),
    assault         = sum(assault, na.rm = TRUE),
    stealing        = sum(stealing, na.rm = TRUE),
    other           = sum(other, na.rm = TRUE),
    total_crime     = sum(incident_total, na.rm = TRUE),  # <-- THIS is the count-once total
    .groups = "drop"
  )


#merge data 
final_data <- crime_monthly %>%
  left_join(unemp_clean, by = c("year", "month"))

# check unemployment actually merged
summary(final_data$unemployment_rate)

# check there aren't a ton of missing values
mean(is.na(final_data$unemployment_rate))

final_data <- final_data %>%
  filter(year >= 2015)


#for grpahing
yearly_data <- final_data %>%
  group_by(year) %>%
  summarise(
    total_crime = sum(total_crime, na.rm = TRUE),
    robbery = sum(robbery, na.rm = TRUE),
    burglary = sum(burglary, na.rm = TRUE),
    property_damage = sum(property_damage, na.rm = TRUE),
    stealing = sum(stealing, na.rm = TRUE),
    assault = sum(assault, na.rm = TRUE),
    other = sum(other, na.rm = TRUE),
    unemployment_rate = mean(unemployment_rate, na.rm = TRUE),
    .groups = "drop"
  )

yearly_long <- yearly_data %>%
  pivot_longer(
    cols = c(robbery, burglary, property_damage, stealing, assault, other),
    names_to = "crime_type",
    values_to = "count"
  )





yearly_data <- yearly_data |> 
  mutate(
    total_crime = robbery + burglary + property_damage + stealing + assault + other
  )


#crime by zip
crime_by_zip <- final_data %>%
  mutate(zipcode = if_else(is.na(zipcode), "Unknown", as.character(zipcode))) %>%
  group_by(zipcode,year,month) %>%
  summarise(
    total_crime = sum(total_crime, na.rm = TRUE),
    .groups = "drop"
  )

#crime by geoloc
crime_by_geoloc <- final_data %>%
  mutate(
    geoloc = if_else(is.na(geoloc), "Unknown", as.character(geoloc))
  ) %>%
  group_by(geoloc,year,month) %>%
  summarise(
    total_crime = sum(total_crime, na.rm = TRUE),
    .groups = "drop"
  )




###### sanity checks

crime %>%
  mutate(
    matches = robbery + burglary + property_damage + assault + stealing + other,
    total_crime = as.integer(matches > 0)
  ) %>%
  summarise(
    total_keyword_hits = sum(matches, na.rm = TRUE),
    total_incidents_counted_once = sum(total_crime, na.rm = TRUE),
    rows_total = n(),
    rows_with_any_match = sum(matches > 0, na.rm = TRUE)
  )

#####
rm(crime_3110prospect,crime_wabash)

crime_prospect <- final_data %>%
  filter(
    str_detect(
      str_to_lower(address),
      "3110\\s+prospect|31st\\s+st.*prospect|prospect.*31st\\s+st"
    )
  )

crime_prospect_monthly <- crime_prospect %>%
  group_by(year, month) %>%
  summarise(
    total_crime = sum(total_crime, na.rm = TRUE),
    .groups = "drop"
  )

crime_prospect_yearly <- crime_prospect %>%
  group_by(year) %>%
  summarise(
    total_crime = sum(total_crime, na.rm = TRUE),
    .groups = "drop"
  )



test


