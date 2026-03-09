# ============================================================
# Bryan's 12 Diff-in-Diff Models
# 6 crime types x 2 treated group definitions = 12 models
#
# Model A (original): treated = zip 64128 only
# Model B (neighbors): treated = zip 64128 + neighboring zips
#
# Formula: crime_rate = b0 + b1*treated + b2*open + b3*treated*open + b4*unemp
# ============================================================

library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(broom)
library(knitr)
library(kableExtra)


# ============================================================
# STEP 1: Load & Clean Data
# (Same cleaning as main script)
# ============================================================

# --- Crime Data ---
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

# --- Unemployment Data ---
KC_unemp_data_monthly <- read_excel("KC_unemp_data monthly.xlsx") |>
  select(-AreaCode, -AreaName, -PeriodType, -SeasonalAdjustment, -RowType, -Period) |>
  mutate(Month = match(as.character(Month), month.name)) |>
  rename(month = Month, year = Year)

# NOTE: Check what the unemployment rate column is called in your Excel file.
# It is likely named "Value" or "unemployment_rate". Update the rename below if needed.
# To check, run: names(KC_unemp_data_monthly)
KC_unemp_data_monthly <- KC_unemp_data_monthly |>
  rename(unemp = UnemploymentRate)

# --- Population Data ---
MS_population_by_zip_and_year <- read_excel("MS population by zip and year.xlsx") |>
  filter(zip %in% 64101:64192) |>
  filter(!zip %in% c(64118, 64138, 64150, 64152, 64161, 64163, 64164, 64166, 64167))


# ============================================================
# STEP 2: Merge Datasets
# ============================================================

merged_data <- KCPD_Reported_Crime_Data |>
  left_join(MS_population_by_zip_and_year, by = c("year", "zip")) |>
  left_join(KC_unemp_data_monthly, by = c("year", "month"))


# ============================================================
# STEP 3: Create Crime Type Indicators
# (Translated from Bryan's Stata code)
# ============================================================

merged_data <- merged_data |>
  mutate(
    desc = str_to_lower(Description),   # lowercase once, reuse everywhere

    # Domestic crimes
    domestic_crime = as.numeric(str_detect(desc, "dome")),

    # Violent sex crimes
    violent_sex_crime = as.numeric(
      str_detect(desc, "forci") |
      str_detect(desc, "sexual assault") |
      str_detect(desc, "fondle") |
      str_detect(desc, "rape")
    ),

    # Other sex crimes: sex-related but NOT violent sex
    other_sex_crime = as.numeric(
      (str_detect(desc, "sex") & violent_sex_crime == 0) |
      str_detect(desc, "prostit") |
      str_detect(desc, "porn") |
      str_detect(desc, "peep")
    ),

    # Aggravated assault: contains "ssau" but NOT "non" or "simple"
    agg_assault_crime = as.numeric(
      str_detect(desc, "ssau") &
      !str_detect(desc, "non") &
      !str_detect(desc, "simple")
    ),

    # Non-aggravated assault: "ssau" + "non", OR contains "simple"
    nonagg_assault_crime = as.numeric(
      (str_detect(desc, "ssau") & str_detect(desc, "non")) |
      str_detect(desc, "simple")
    ),

    # "Taking" crimes: theft + burglary + shoplifting + robbery combined
    # (Bryan's note: "taking" combines all stealing, burglary, shoplifting, etc.)
    taking_crime = as.numeric(
      str_detect(desc, "heft")     |   # theft
      str_detect(desc, "teal")     |   # steal/stealing
      str_detect(desc, "pocket")   |   # pickpocket
      str_detect(desc, "larceny")  |   # larceny
      str_detect(desc, "stole")    |   # stolen
      str_detect(desc, "burg")     |   # burglary
      str_detect(desc, "shoplif")  |   # shoplifting
      str_detect(desc, "rob")          # robbery
    )
  )


# ============================================================
# STEP 4: Create Treated Group Indicators
# ============================================================

# Store opened June 16, 2018
merged_data <- merged_data |>
  mutate(
    # After store opened (1 = yes, 0 = no)
    open = as.numeric(date >= as.Date("2018-06-16")),

    # Model A treated group: store zip only
    z64128 = as.numeric(zip == 64128),

    # Model B treated group: store zip + neighbors
    store_and_neighbors = as.numeric(
      zip %in% c(64128, 64126, 64127, 64129, 64130, 64109, 64110)
    )
  )


# ============================================================
# STEP 5: Aggregate to Monthly Crime RATES by zip
# ============================================================
# Crime rate = (monthly crime count / population) * 1000
# January, February, March 2019 have missing data so we set those to NA

# First join population into merged_data so we can use it here
# (population comes from cleaned_pop_by_zip_and_year, already merged)

monthly_zip <- merged_data |>
  group_by(year, month, zip, open, z64128, store_and_neighbors, unemp) |>
  summarise(
    population          = mean(pop,                 na.rm = TRUE),
    count_domestic      = sum(domestic_crime,       na.rm = TRUE),
    count_violent_sex   = sum(violent_sex_crime,    na.rm = TRUE),
    count_other_sex     = sum(other_sex_crime,      na.rm = TRUE),
    count_agg_assault   = sum(agg_assault_crime,    na.rm = TRUE),
    count_nonagg_assault= sum(nonagg_assault_crime, na.rm = TRUE),
    count_taking        = sum(taking_crime,         na.rm = TRUE),
    .groups = "drop"
  ) |>
  # Calculate crime rates per 1,000 residents
  mutate(
    rate_domestic       = (count_domestic        / population) * 1000,
    rate_violent_sex    = (count_violent_sex      / population) * 1000,
    rate_other_sex      = (count_other_sex        / population) * 1000,
    rate_agg_assault    = (count_agg_assault      / population) * 1000,
    rate_nonagg_assault = (count_nonagg_assault   / population) * 1000,
    rate_taking         = (count_taking           / population) * 1000
  ) |>
  # Convert NaN and Inf to NA (happens when population = 0 or missing)
  mutate(
    across(starts_with("rate_"), ~ if_else(is.nan(.) | is.infinite(.), NA_real_, .))
  ) |>
  # Set Jan, Feb, Mar 2019 to NA — missing data those months
  mutate(
    across(
      starts_with("rate_"),
      ~ if_else(year == 2019 & month %in% c(1, 2, 3), NA_real_, .)
    )
  ) |>
  filter(!is.na(unemp)) |>       # drop rows with no unemployment match
  filter(!is.na(population))     # drop rows with no population (non-KC zips)


# ============================================================
# STEP 6: Run the 12 Models
# ============================================================
# Model A formula: crime_rate ~ z64128 + open + z64128:open + unemp
# Model B formula: crime_rate ~ store_and_neighbors + open + store_and_neighbors:open + unemp

crime_rates <- c(
  "rate_domestic",
  "rate_violent_sex",
  "rate_other_sex",
  "rate_agg_assault",
  "rate_nonagg_assault",
  "rate_taking"
)

crime_labels <- c(
  "Domestic",
  "Violent Sex",
  "Other Sex",
  "Agg. Assault",
  "Non-Agg. Assault",
  "Taking (Theft/Burg/Rob)"
)

# Run Model A (z64128) for each crime type
# na.exclude skips NA rows (Jan-Mar 2019 missing data) without erroring
models_a <- lapply(crime_rates, function(cr) {
  formula <- as.formula(paste(cr, "~ z64128 + open + z64128:open + unemp"))
  lm(formula, data = monthly_zip, na.action = na.exclude)
})
names(models_a) <- paste0("A_", crime_rates)

# Run Model B (store_and_neighbors) for each crime type
models_b <- lapply(crime_rates, function(cr) {
  formula <- as.formula(paste(cr, "~ store_and_neighbors + open + store_and_neighbors:open + unemp"))
  lm(formula, data = monthly_zip, na.action = na.exclude)
})
names(models_b) <- paste0("B_", crime_rates)


# ============================================================
# STEP 7: Build Clean Results Table
# ============================================================

extract_did <- function(mod, model_label, crime_label, treated_var) {

  tidy_mod <- broom::tidy(mod) |>
    mutate(
      stars = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      display = paste0(round(estimate, 4), stars, "\n(", round(std.error, 4), ")")
    )

  # Pull key coefficients
  get_val <- function(term_name) {
    row <- tidy_mod |> filter(term == term_name)
    if (nrow(row) == 0) return(NA_character_)
    row$display
  }

  interaction_term <- paste0(treated_var, ":open")

  tibble(
    `Model`        = model_label,
    `Crime Type`   = crime_label,
    `Treated`      = get_val(treated_var),
    `After Open`   = get_val("open"),
    `Treated×Open` = get_val(interaction_term),   # <-- this is the DiD estimate
    `Unemployment` = get_val("unemp"),
    `N`            = as.character(nobs(mod)),
    `Adj R²`       = as.character(round(summary(mod)$adj.r.squared, 3))
  )
}

# Build table A rows
rows_a <- mapply(
  extract_did,
  mod          = models_a,
  model_label  = rep("A: Store Zip Only (64128)", 6),
  crime_label  = crime_labels,
  treated_var  = rep("z64128", 6),
  SIMPLIFY     = FALSE
)

# Build table B rows
rows_b <- mapply(
  extract_did,
  mod          = models_b,
  model_label  = rep("B: Store + Neighbors", 6),
  crime_label  = crime_labels,
  treated_var  = rep("store_and_neighbors", 6),
  SIMPLIFY     = FALSE
)

# Combine into one table, alternating A and B by crime type
results_table <- bind_rows(c(rows_a, rows_b)) |>
  arrange(`Crime Type`, `Model`)


# ============================================================
# STEP 8: Print & Save Table
# ============================================================

print(results_table)

# Save as HTML
kable_out <- kable(
  results_table,
  caption = "Diff-in-Diff Estimates: Effect of Store Opening on Monthly Crime Rates per 1,000 Residents\nCoefficients shown with standard errors in parentheses. * p<0.10, ** p<0.05, *** p<0.01",
  align   = "llcccccc"
) |>
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) |>
  column_spec(5, bold = TRUE, background = "#f0f7ff") |>   # highlight DiD estimate column
  pack_rows("Domestic Crime",         1, 2) |>
  pack_rows("Violent Sex Crime",      3, 4) |>
  pack_rows("Other Sex Crime",        5, 6) |>
  pack_rows("Aggravated Assault",     7, 8) |>
  pack_rows("Non-Aggravated Assault", 9, 10) |>
  pack_rows("Taking Crimes",          11, 12)

save_kable(kable_out, "bryan_table.html")

message("Done! Results saved to bryan_table.html")
message("The key column is 'Treated×Open' — that is your DiD estimate.")
message("It tells you how much crime changed in the treated area AFTER the store opened,")
message("compared to the control area.")
