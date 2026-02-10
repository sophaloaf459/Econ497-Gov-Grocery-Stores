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