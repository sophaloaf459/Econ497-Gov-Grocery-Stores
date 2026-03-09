# ####
library(tidyverse)
library(tidyr)
library(readr) 
library(readxl)
library(dplyr)
library(broom)
library(modelsummary)
library(knitr)
install.packages("kableExtra")
library(kableExtra)
library(grid)
library(gridExtra)


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
KC_unemp_data_monthly <- read_excel("KC_unemp_data monthly.xlsx")
KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  select(-AreaCode,-AreaName,-PeriodType,-SeasonalAdjustment,-RowType) 
  
KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  select(-Period)

KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  mutate(Month = match(as.character(Month), month.name))
# as.character() converts the month column to plain text first
# This ensures match() can work with it properly
# Then match() finds the position in month.name (1-12)

#renaming
KC_unemp_data_monthly <- KC_unemp_data_monthly |> 
  rename(month = Month, year = Year)


# Population Cleaning -----------------------------------------------------

MS_population_by_zip_and_year <- read_excel("MS population by zip and year.xlsx")

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
    long_miles = long_diff * 69 * cos(reference_lat * pi / 180),  # changed from 54
    
    crime_distance_miles_fs = sqrt(lat_miles^2 + long_miles^2)
  )




#Create columns for theft ####

merged_data <- merged_data |> 
  mutate(
    theft = if_else(
      str_detect(
        Description,
        regex(
          paste(
            "theft",
            "steal",                 # captures STEALING, STEALIG, etc.
            "larceny",
            "shoplift",
            "robbery",
            "stolen",
            "purse",
            "pickpocket",
            "pocket-picking",
            "motor vehicle theft",
            "auto theft",
            "Burglary",
            sep = "|"
          ),
          ignore_case = TRUE
        )
      ),
      1,
      0
    )
  )


#close or not close ####
merged_data <- merged_data |>
  mutate(
    close = as.numeric(crime_distance_miles_fs <= 0.25),
    far = as.numeric(crime_distance_miles_fs > 0.25),
    
    close_0_5  = as.numeric(crime_distance_miles_fs <= 0.5),
    far_0_5    = as.numeric(crime_distance_miles_fs > 0.5)
  )


merged_data |> count(close, far)

#after open or not

merged_data <- merged_data |>
  mutate(
    datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S"),
    after_open = as.numeric(date >= as.Date("2018-06-16")),
    before_open = as.numeric(date < as.Date("2018-06-16"))
  )


# Remove NAs and ensure 8 rows per day ----------------------------------
all_combos <- expand.grid(
  close = c(0, 1),
  theft = c(0, 1),
  violent = c(0, 1)
)

all_dates <- data.frame(date = unique(merged_data$date))

full_grid <- merge(all_dates, all_combos)

daily_counts <- merged_data |>
  filter(!is.na(close), !is.na(theft), !is.na(violent)) |>
  group_by(date, close, theft, violent) |>
  summarise(TotalCrimes = n(), after_open = max(after_open), .groups = "drop")

daily_dataset_no_na <- full_grid |>
  left_join(daily_counts, by = c("date", "close", "theft", "violent")) |>
  mutate(
    TotalCrimes = ifelse(is.na(TotalCrimes), 0, TotalCrimes),
    after_open = ifelse(is.na(after_open), as.numeric(date >= as.Date("2018-06-16")), after_open)
  ) |>
  arrange(date, close, theft, violent)

# fun ---------------------------------------------------------------------



#Model 1: Violent - No, Theft - Yes ####

model1 <- lm(
  TotalCrimes ~ close + after_open + close:after_open,
  data = daily_dataset_no_na,
  subset = (violent == 0 & theft == 1)
)

summary(model1)


#Model 2: Violent - Yes, Theft - Yes####
model2 <- lm(
  TotalCrimes ~ close + after_open + close:after_open,
  data = daily_dataset_no_na,
  subset = (violent == 1 & theft == 1)
)

summary(model2)

#Model 3: Violent - Yes, Theft - No####

model3 <- lm(
  TotalCrimes ~ close + after_open + close:after_open,
  data = daily_dataset_no_na,
  subset = (violent == 1 & theft == 0)
)

summary(model3)


#Model 4: Violent - no, theft - no####
model4 <- lm(
  TotalCrimes ~ close + after_open + close:after_open,
  data = daily_dataset_no_na,
  subset = (violent == 0 & theft == 0)
)

summary(model4)

# Table 1: (should include models 1-4) - close = 0.25 mile here####


# List of models
models_list <- list(
  "Model 1" = model1,
  "Model 2" = model2,
  "Model 3" = model3,
  "Model 4" = model4
)

# Map models to Theft/Violent flags
model_flags <- list(
  "Model 1" = list(Theft = "Yes", Violent = "No"),
  "Model 2" = list(Theft = "Yes", Violent = "Yes"),
  "Model 3" = list(Theft = "No",  Violent = "Yes"),
  "Model 4" = list(Theft = "No",  Violent = "No")
)

# Function to extract model info
extract_model_info <- function(mod, name) {
  
  coefs <- broom::tidy(mod) |> 
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "close" ~ "Close",
        term == "after_open" ~ "After Store Opened",
        term == "close:after_open" ~ "Close × After",
        TRUE ~ term
      ),
      stars = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      estimate = paste0(round(estimate, 2), stars)
    ) |> 
    select(term, estimate)
  
  stats <- tibble(
    term = c("Theft Only", "Violent Only", "N", "Adj R²"),
    estimate = c(
      model_flags[[name]]$Theft,
      model_flags[[name]]$Violent,
      as.character(floor(nrow(mod$model))),
      as.character(floor(summary(mod)$adj.r.squared*100)/100)
    )
  )
  
  bind_rows(coefs, stats) |> mutate(Model = name)
}

# Build table
table_df <- bind_rows(
  lapply(names(models_list), function(x) extract_model_info(models_list[[x]], x))
)

final_table <- table_df |> 
  pivot_wider(names_from = Model, values_from = estimate) |> 
  rename(Variable = term) |> 
  select(Variable, `Model 1`, `Model 2`, `Model 3`, `Model 4`)

final_table


#Save final table as html####

kable_html <- kable(final_table,
                    caption = "Table 1: Difference-in-Differences Estimate of Effect of Store Opening on Nearby Crime",
                    align = "lcccc") |> 
  kable_styling(full_width = FALSE)

# Save as HTML file
save_kable(kable_html, "Table 1.html")



#Create new daily data for 0.5 mile radius####
daily_dataset_0_5 <- merged_data |>
  group_by(date, close_0_5, theft, violent) |>
  summarise(
    TotalCrimes = n(),
    after_open = max(after_open),
    .groups = "drop"
  ) |>
  arrange(date)

#drop na
daily_dataset_0_5 <- daily_dataset_0_5 |>
  tidyr::drop_na(TotalCrimes, close_0_5, theft, violent, after_open)

#Model 5: 0.5 radius, Violent - no, theft - yes####
model5 <- lm(
  TotalCrimes ~ close_0_5 + after_open + close_0_5:after_open,
  data = daily_dataset_0_5,
  subset = (violent == 0 & theft == 1)
)
#Model 6: 0.5 radius, violent yes, theft yes####

model6<- lm(
  TotalCrimes ~ close_0_5 + after_open + close_0_5:after_open,
  data = daily_dataset_0_5,
  subset = (violent == 1 & theft == 1)
)

#Model 7: 0.5 radius. violent yes, theft no####
model7 <- lm(
  TotalCrimes ~ close_0_5 + after_open + close_0_5:after_open,
  data = daily_dataset_0_5,
  subset = (violent == 1 & theft == 0)
)

#Model 8: 0.5 mile radius, violent - no, theft - no####
model8 <- lm(
  TotalCrimes ~ close_0_5 + after_open + close_0_5:after_open,
  data = daily_dataset_0_5,
  subset = (violent == 0 & theft == 0)
)


#Create table 2: (the 0.5 mile radius)####

# List of models
models_list <- list(
  "Model 5" = model5,
  "Model 6" = model6,
  "Model 7" = model7,
  "Model 8" = model8
)

# Map models to Theft/Violent flags
model_flags <- list(
  "Model 5" = list(Theft = "Yes", Violent = "No"),
  "Model 6" = list(Theft = "Yes", Violent = "Yes"),
  "Model 7" = list(Theft = "No",  Violent = "Yes"),
  "Model 8" = list(Theft = "No",  Violent = "No")
)

# Function to extract model info
extract_model_info <- function(mod, name) {
  
  coefs <- broom::tidy(mod) |> 
    mutate(
      term = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "close" ~ "Close",
        term == "after_open" ~ "After Store Opened",
        term == "close:after_open" ~ "Close × After",
        TRUE ~ term
      ),
      stars = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      estimate = paste0(round(estimate, 2), stars)
    ) |> 
    select(term, estimate)
  
  stats <- tibble(
    term = c("Theft Only", "Violent Only", "N", "Adj R²"),
    estimate = c(
      model_flags[[name]]$Theft,
      model_flags[[name]]$Violent,
      as.character(floor(nrow(mod$model))),
      as.character(floor(summary(mod)$adj.r.squared*100)/100)
    )
  )
  
  bind_rows(coefs, stats) |> mutate(Model = name)
}

# Build table
table_df2 <- bind_rows(
  lapply(names(models_list), function(x) extract_model_info(models_list[[x]], x))
)

final_table2 <- table_df2 |> 
  pivot_wider(names_from = Model, values_from = estimate) |> 
  rename(Variable = term) |> 
  select(Variable, `Model 5`, `Model 6`, `Model 7`, `Model 8`)

final_table2

# Save table 2####
kable_html2 <- kable(final_table2,
                    caption = "Table 2: Difference-in-Differences Estimate of Effect of Store Opening on Nearby Crime (0.5 mile radius)",
                    align = "lcccc") |> 
  kable_styling(full_width = FALSE)

# Save as HTML file
save_kable(kable_html2, "Table 2.html")


# Interactive Map -------------------------------------------------------------------------

library(leaflet)

# Define color based on crime density (violent + theft = most dense)
merged_data_filtered <- merged_data |> 
  filter(crime_distance_miles_fs <= 5) |>
  filter(!is.na(violent), !is.na(theft)) |>
  mutate(
    crime_type = case_when(
      violent == 1 & theft == 1 ~ "Violent Theft",
      violent == 1 & theft == 0 ~ "Violent Non-Theft",
      violent == 0 & theft == 1 ~ "Non-Violent Theft",
      violent == 0 & theft == 0 ~ "Non-Violent Non-Theft"
    ),
    color = case_when(
      crime_type == "Violent Theft" ~ "#8B0000",
      crime_type == "Violent Non-Theft" ~ "#FF4500",
      crime_type == "Non-Violent Theft" ~ "#FF8C00",
      crime_type == "Non-Violent Non-Theft" ~ "#FFD700"
    )
  )

# Create base map
map <- leaflet() |> 
  addTiles() |> 
  addMarkers(
    lng = -94.55448775544315, 
    lat = 39.06903775959004,
    popup = "Reference Point (Store)"
  )

# Add a layer for each year
for(y in sort(unique(merged_data_filtered$year))) {
  year_data <- merged_data_filtered |> filter(year == y)
  
  map <- map |> 
    addCircleMarkers(
      data = year_data,
      lng = ~Longitude, 
      lat = ~Latitude,
      radius = 3,
      group = as.character(y),
      color = ~color,
      fillOpacity = 0.7,
      popup = ~paste(
        "Date:", date, "<br>",
        "Type:", crime_type, "<br>",
        "Description:", Description, "<br>",
        "Distance:", round(crime_distance_miles_fs, 2), "miles", "<br>",
        "Violent:", violent, "<br>",
        "Theft:", theft, "<br>",
        "Year:", year
      )
    )
}

# Add legend
map <- map |> 
  addLegend(
    position = "bottomright",
    colors = c("#8B0000", "#FF4500", "#FF8C00", "#FFD700"),
    labels = c("Violent Theft", "Violent Non-Theft", "Non-Violent Theft", "Non-Violent Non-Theft"),
    title = "Crime Type"
  )

# Add layer control to toggle years
map |> addLayersControl(
  overlayGroups = as.character(sort(unique(merged_data_filtered$year))),
  options = layersControlOptions(collapsed = FALSE)
)


library(htmlwidgets)
saveWidget(map, "crime_map.html")


# ggplot fun --------------------------------------------------------------

merged_data |>
  filter(close == 1) |>
  group_by(date) |>
  summarise(total_crimes = n(), after_open = max(after_open)) |>
  ggplot(aes(x = date, y = total_crimes)) +
  geom_line(color = "steelblue", alpha = 0.3) +
  geom_smooth(aes(group = factor(after_open), color = factor(after_open)), 
              method = "lm", se = TRUE) +
  geom_vline(xintercept = as.Date("2018-06-16"), color = "red", 
             linetype = "dashed", linewidth = 1) +
  annotate("text", x = as.Date("2018-06-16"), 
           y = max(merged_data |> filter(close == 1) |> group_by(date) |> summarise(n = n()) |> pull(n)), 
           label = "Store Opens", color = "red", hjust = -0.1, size = 4) +
  scale_color_manual(values = c("0" = "darkblue", "1" = "darkorange"),
                     labels = c("0" = "Before Opening", "1" = "After Opening"),
                     name = "") +
  labs(
    title = "Daily Crime Within 0.25 Miles of Store",
    subtitle = "Separate trends before and after store opening",
    x = "Date",
    y = "Total Crimes"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the plot
ggsave("crime_plot.png", width = 10, height = 6, dpi = 300)
