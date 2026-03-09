# ============================================================
# Maps: Change in Crime Rate 2017 vs 2019 by Zip Code
# + Cleaned Regression Tables saved as PNG
# ============================================================
# For each crime type, map shows: (2019 rate - 2017 rate) per zip
# Lighter color = rate went down, Darker color = rate went up
# Note: Jan-Mar 2019 excluded (missing data), so 2019 = Apr-Dec only
#       to keep comparison fair, 2017 is also Apr-Dec
# ============================================================

library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(broom)
library(knitr)
library(kableExtra)
library(leaflet)
library(htmlwidgets)
library(sf)
library(tigris)   # downloads zip code shapefiles
library(webshot2) # saves HTML tables as PNG
options(tigris_use_cache = TRUE)

# Install any missing packages
required_packages <- c("tigris", "sf", "webshot2")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}
library(tigris)
library(sf)
library(webshot2)


# ============================================================
# STEP 1: Load & Clean Data (same as 12_models.R)
# ============================================================

KCPD_Reported_Crime_Data <- read_csv("KCPD_Reported_Crime_Data.csv",
  col_types = cols(
    Reported_Date = col_datetime(format = "%Y %b %d %I:%M:%S %p"),
    Reported_Time = col_time(format = "%H:%M:%S")
  )
)

KCPD_Reported_Crime_Data <- KCPD_Reported_Crime_Data |>
  separate(Reported_Date,
    into = c("year", "month", "day"),
    sep = "-", remove = FALSE
  ) |>
  mutate(across(c(year, month, day), as.numeric)) |>
  filter(year > 2014) |>
  select(-Suspect_Count, -Victim_Count, -Arrestee_Count, -Deceased_Count,
         -Location, -City, -Area, -IBRS) |>
  rename(zip = Zip_Code, date = Reported_Date, time = Reported_Time)

MS_population_by_zip_and_year <- read_excel("MS population by zip and year.xlsx") |>
  filter(zip %in% 64101:64192) |>
  filter(!zip %in% c(64118, 64138, 64150, 64152, 64161, 64163, 64164, 64166, 64167))

merged_data <- KCPD_Reported_Crime_Data |>
  left_join(MS_population_by_zip_and_year, by = c("year", "zip"))


# ============================================================
# STEP 2: Create Crime Type Indicators
# ============================================================

merged_data <- merged_data |>
  mutate(
    desc = str_to_lower(Description),

    domestic_crime = as.numeric(str_detect(desc, "dome")),

    violent_sex_crime = as.numeric(
      str_detect(desc, "forci") |
      str_detect(desc, "sexual assault") |
      str_detect(desc, "fondle") |
      str_detect(desc, "rape")
    ),

    other_sex_crime = as.numeric(
      (str_detect(desc, "sex") & !str_detect(desc, "forci") &
         !str_detect(desc, "sexual assault") & !str_detect(desc, "fondle") &
         !str_detect(desc, "rape")) |
      str_detect(desc, "prostit") |
      str_detect(desc, "porn") |
      str_detect(desc, "peep")
    ),

    agg_assault_crime = as.numeric(
      str_detect(desc, "ssau") &
      !str_detect(desc, "non") &
      !str_detect(desc, "simple")
    ),

    nonagg_assault_crime = as.numeric(
      (str_detect(desc, "ssau") & str_detect(desc, "non")) |
      str_detect(desc, "simple")
    ),

    taking_crime = as.numeric(
      str_detect(desc, "heft")    |
      str_detect(desc, "teal")    |
      str_detect(desc, "pocket")  |
      str_detect(desc, "larceny") |
      str_detect(desc, "stole")   |
      str_detect(desc, "burg")    |
      str_detect(desc, "shoplif") |
      str_detect(desc, "rob")
    )
  )


# ============================================================
# STEP 3: Annual Crime Rates by Zip for 2017 and 2019
# Using Apr-Dec only for both years (Jan-Mar 2019 missing)
# ============================================================

kc_zips <- unique(MS_population_by_zip_and_year$zip)

annual_rates <- merged_data |>
  filter(
    year %in% c(2017, 2019),
    month %in% 4:12,          # Apr-Dec only for fair comparison
    zip %in% kc_zips,
    !is.na(pop)
  ) |>
  group_by(year, zip) |>
  summarise(
    population       = mean(pop, na.rm = TRUE),
    domestic         = sum(domestic_crime,       na.rm = TRUE),
    violent_sex      = sum(violent_sex_crime,    na.rm = TRUE),
    other_sex        = sum(other_sex_crime,      na.rm = TRUE),
    agg_assault      = sum(agg_assault_crime,    na.rm = TRUE),
    nonagg_assault   = sum(nonagg_assault_crime, na.rm = TRUE),
    taking           = sum(taking_crime,         na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    rate_domestic      = (domestic      / population) * 1000,
    rate_violent_sex   = (violent_sex   / population) * 1000,
    rate_other_sex     = (other_sex     / population) * 1000,
    rate_agg_assault   = (agg_assault   / population) * 1000,
    rate_nonagg_assault= (nonagg_assault/ population) * 1000,
    rate_taking        = (taking        / population) * 1000,
    across(starts_with("rate_"), ~ if_else(is.nan(.) | is.infinite(.), NA_real_, .))
  )

# Calculate change: 2019 rate minus 2017 rate per zip
rate_change <- annual_rates |>
  select(year, zip, starts_with("rate_")) |>
  pivot_longer(starts_with("rate_"), names_to = "crime_type", values_to = "rate") |>
  pivot_wider(names_from = year, values_from = rate, names_prefix = "yr_") |>
  mutate(change = yr_2019 - yr_2017)   # positive = went up, negative = went down


# ============================================================
# STEP 4: Download KC Zip Code Shapefile
# ============================================================

message("Downloading KC zip code shapefiles...")
# Download Missouri zctas and filter to KC zips
mo_zips <- zctas(year = 2019, cb = TRUE) |>
  filter(ZCTA5CE10 %in% as.character(kc_zips)) |>
  rename(zip_char = ZCTA5CE10) |>
  mutate(zip = as.numeric(zip_char))


# ============================================================
# STEP 5: Make One Map Per Crime Type
# ============================================================

crime_types <- c(
  "rate_domestic"       = "Domestic Crime",
  "rate_violent_sex"    = "Violent Sex Crime",
  "rate_other_sex"      = "Other Sex Crime",
  "rate_agg_assault"    = "Aggravated Assault",
  "rate_nonagg_assault" = "Non-Aggravated Assault",
  "rate_taking"         = "Taking Crimes (Theft/Burg/Rob)"
)

# Store zip and neighbor zips for reference markers
store_zips <- c(64128, 64126, 64127, 64129, 64130, 64109, 64110)

for (ct in names(crime_types)) {

  crime_label <- crime_types[[ct]]

  # Get change data for this crime type
  map_data <- rate_change |>
    filter(crime_type == ct) |>
    select(zip, change, yr_2017, yr_2019)

  # Join to shapefile
  map_sf <- mo_zips |>
    left_join(map_data, by = "zip") |>
    filter(!is.na(change))

  if (nrow(map_sf) == 0) {
    message("Skipping ", crime_label, " — no data after join")
    next
  }

  # Color palette: blue = decrease, white = no change, red = increase
  pal <- colorNumeric(
    palette = c("#2166ac", "#f7f7f7", "#d6604d"),
    domain  = map_sf$change,
    na.color = "#cccccc"
  )

  # Build leaflet map
  m <- leaflet(map_sf) |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      fillColor   = ~pal(change),
      fillOpacity = 0.8,
      color       = "white",
      weight      = 1,
      popup = ~paste0(
        "<b>Zip: </b>", zip, "<br>",
        "<b>2017 Rate: </b>", round(yr_2017, 2), " per 1,000<br>",
        "<b>2019 Rate: </b>", round(yr_2019, 2), " per 1,000<br>",
        "<b>Change: </b>", round(change, 2),
        if_else(change > 0, " ▲ (increased)", " ▼ (decreased)")
      ),
      label = ~paste0("Zip ", zip, ": ", round(change, 2))
    ) |>
    # Mark the store location
    addCircleMarkers(
      lng = -94.55448775544315,
      lat =  39.06903775959004,
      radius = 8,
      color = "black", fillColor = "gold",
      fillOpacity = 1, weight = 2,
      popup = "Store Location (3110 Prospect Ave)"
    ) |>
    addLegend(
      position = "bottomright",
      pal      = pal,
      values   = ~change,
      title    = paste0(crime_label, "<br>Change in Rate<br>2019 vs 2017<br>(per 1,000 residents)"),
      labFormat = labelFormat(suffix = "")
    ) |>
    addControl(
      html = paste0("<b>", crime_label, "</b><br>Change in Crime Rate: 2019 vs 2017<br>
                    Blue = Decreased &nbsp; Red = Increased"),
      position = "topleft"
    )

  # Save map
  filename <- paste0("map_", gsub("rate_", "", ct), ".html")
  saveWidget(m, filename, selfcontained = TRUE)
  message("Saved: ", filename)
}

message("All maps saved!")
message("Open each map_*.html file in your browser.")
message("Click any zip code to see exact 2017 rate, 2019 rate, and change.")


# ============================================================
# STEP 6: Clean Regression Tables — Re-run 12 Models & Save as PNG
# ============================================================

KC_unemp_data_monthly <- read_excel("KC_unemp_data monthly.xlsx") |>
  select(-AreaCode, -AreaName, -PeriodType, -SeasonalAdjustment, -RowType, -Period) |>
  mutate(Month = match(as.character(Month), month.name)) |>
  rename(month = Month, year = Year, unemp = UnemploymentRate)

merged_data <- merged_data |>
  left_join(KC_unemp_data_monthly, by = c("year", "month")) |>
  mutate(
    open = as.numeric(date >= as.Date("2018-06-16")),
    z64128 = as.numeric(zip == 64128),
    store_and_neighbors = as.numeric(zip %in% c(64128,64126,64127,64129,64130,64109,64110))
  )

monthly_zip <- merged_data |>
  group_by(year, month, zip, open, z64128, store_and_neighbors, unemp) |>
  summarise(
    population           = mean(pop,                  na.rm = TRUE),
    count_domestic       = sum(domestic_crime,        na.rm = TRUE),
    count_violent_sex    = sum(violent_sex_crime,     na.rm = TRUE),
    count_other_sex      = sum(other_sex_crime,       na.rm = TRUE),
    count_agg_assault    = sum(agg_assault_crime,     na.rm = TRUE),
    count_nonagg_assault = sum(nonagg_assault_crime,  na.rm = TRUE),
    count_taking         = sum(taking_crime,          na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    rate_domestic        = (count_domestic        / population) * 1000,
    rate_violent_sex     = (count_violent_sex      / population) * 1000,
    rate_other_sex       = (count_other_sex        / population) * 1000,
    rate_agg_assault     = (count_agg_assault      / population) * 1000,
    rate_nonagg_assault  = (count_nonagg_assault   / population) * 1000,
    rate_taking          = (count_taking           / population) * 1000,
    across(starts_with("rate_"), ~ if_else(is.nan(.) | is.infinite(.), NA_real_, .)),
    across(starts_with("rate_"), ~ if_else(year == 2019 & month %in% c(1,2,3), NA_real_, .))
  ) |>
  filter(!is.na(unemp), !is.na(population))

crime_rate_vars <- c(
  "rate_domestic", "rate_violent_sex", "rate_other_sex",
  "rate_agg_assault", "rate_nonagg_assault", "rate_taking"
)
crime_labels_vec <- c(
  "Domestic", "Violent Sex", "Other Sex",
  "Agg. Assault", "Non-Agg. Assault", "Taking"
)

models_a <- lapply(crime_rate_vars, function(cr) {
  lm(as.formula(paste(cr, "~ z64128 + open + z64128:open + unemp")),
     data = monthly_zip, na.action = na.exclude)
})
names(models_a) <- crime_rate_vars

models_b <- lapply(crime_rate_vars, function(cr) {
  lm(as.formula(paste(cr, "~ store_and_neighbors + open + store_and_neighbors:open + unemp")),
     data = monthly_zip, na.action = na.exclude)
})
names(models_b) <- crime_rate_vars

# Function to extract one row per model
extract_row <- function(mod, model_label, crime_label, treated_var) {
  t <- broom::tidy(mod) |>
    mutate(
      stars = case_when(p.value < 0.01 ~ "***", p.value < 0.05 ~ "**",
                        p.value < 0.10 ~ "*", TRUE ~ ""),
      disp  = paste0(round(estimate, 3), stars, " (", round(std.error, 3), ")")
    )
  get <- function(nm) { r <- t |> filter(term == nm); if (nrow(r)==0) "—" else r$disp }
  tibble(
    Model          = model_label,
    `Crime Type`   = crime_label,
    Treated        = get(treated_var),
    `After Open`   = get("open"),
    `Treated×Open` = get(paste0(treated_var, ":open")),
    Unemployment   = get("unemp"),
    N              = as.character(nobs(mod)),
    `Adj R²`       = sprintf("%.3f", summary(mod)$adj.r.squared)
  )
}

rows_a <- mapply(extract_row, mod = models_a,
                 model_label = rep("A: Store Zip Only (64128)", 6),
                 crime_label = crime_labels_vec,
                 treated_var = rep("z64128", 6),
                 SIMPLIFY = FALSE)

rows_b <- mapply(extract_row, mod = models_b,
                 model_label = rep("B: Store + Neighbors", 6),
                 crime_label = crime_labels_vec,
                 treated_var = rep("store_and_neighbors", 6),
                 SIMPLIFY = FALSE)

results <- bind_rows(c(rows_a, rows_b)) |>
  arrange(`Crime Type`, Model)

# Save as clean HTML then screenshot to PNG
tbl_html <- kable(results,
  caption = paste(
    "Diff-in-Diff Estimates: Effect of Store Opening on Monthly Crime Rates per 1,000 Residents.",
    "Standard errors in parentheses. * p<0.10, ** p<0.05, *** p<0.01"
  ),
  align = "llccccc"
) |>
  kable_styling(
    full_width        = FALSE,
    bootstrap_options = c("striped", "hover", "condensed"),
    font_size         = 13
  ) |>
  column_spec(5, bold = TRUE, background = "#eaf3fb") |>
  pack_rows("Domestic Crime",          1,  2) |>
  pack_rows("Violent Sex Crime",       3,  4) |>
  pack_rows("Other Sex Crime",         5,  6) |>
  pack_rows("Aggravated Assault",      7,  8) |>
  pack_rows("Non-Aggravated Assault",  9, 10) |>
  pack_rows("Taking Crimes",          11, 12)

save_kable(tbl_html, "regression_table.html")
message("Regression table saved to regression_table.html")

# Save as PNG using webshot2
webshot2::webshot("regression_table.html", "regression_table.png",
                  vwidth = 1100, vheight = 800, zoom = 2)
message("Regression table PNG saved to regression_table.png")
message("Done! Files created:")
message("  - map_domestic.html")
message("  - map_violent_sex.html")
message("  - map_other_sex.html")
message("  - map_agg_assault.html")
message("  - map_nonagg_assault.html")
message("  - map_taking.html")
message("  - regression_table.html")
message("  - regression_table.png")
