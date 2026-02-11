library(tidyverse)
library(tidyr)
library(readr) 


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


#removing columns not needed
KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |> 
  filter(year>2014,Reported_Date>2014) |> 
  select(-Suspect_Count,-Victim_Count,-Arrestee_Count,-Deceased_Count)

KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |>
  select(-Location,-City,-Area)

KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |> select(-IBRS)



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








