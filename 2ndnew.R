# PART 3: QUANTIFYING - making data clean and stuff
# 02_data_cleaning.R
# cleaning up the messy data and making new things

# Load necessary packages
library(dplyr)

# Load the data (run 01_data_loading.R first obviously)
OECD_new <- read.csv("OECDnew.csv", stringsAsFactors = FALSE)
avoidable_death <- read.csv("avoidable death.csv", stringsAsFactors = FALSE)

# 3.1 Clean up the Data

# Step 1: Only keep the years 2019-2022
OECD_new_clean <- OECD_new %>%
  filter(TIME_PERIOD %in% c(2019, 2020, 2021, 2022)) %>%
  select(
    country = `Reference area`,
    gender = Sex,  # Assuming 'Sex' is the column name for gender
    year = TIME_PERIOD,
    tobacco_rate = OBS_VALUE
  )

avoidable_death_clean <- avoidable_death %>%
  filter(TIME_PERIOD %in% c(2019, 2020, 2021, 2022)) %>%
  select(
    country = `Reference area`,
    gender = Sex,  # Assuming 'Sex' is also in this dataset for gender
    year = TIME_PERIOD,
    mortality_rate = OBS_VALUE
  )

# Step 2: Remove rows with missing data (NA values)
OECD_new_clean <- OECD_new_clean %>%
  filter(!is.na(tobacco_rate))

avoidable_death_clean <- avoidable_death_clean %>%
  filter(!is.na(mortality_rate))

# Step 3: Only keep 'Male' and 'Female' for gender
OECD_new_clean <- OECD_new_clean %>%
  filter(gender %in% c("Male", "Female"))

avoidable_death_clean <- avoidable_death_clean %>%
  filter(gender %in% c("Male", "Female"))

# Step 4: Merge the two datasets by 'country', 'gender', and 'year'
merged_data <- OECD_new_clean %>%
  left_join(avoidable_death_clean, by = c("country", "gender", "year"))

# 3.2 Create New Variables

# New variable 1: Create mortality categories based on mortality_rate
merged_data <- merged_data %>%
  mutate(
    mortality_category = case_when(
      mortality_rate <= 1 ~ "variable1",
      mortality_rate <= 10 ~ "variable10",
      mortality_rate <= 50 ~ "variable50",
      TRUE ~ "Very high mortality"
    )
  )

# New variable 2: Add COVID period classification
merged_data <- merged_data %>%
  mutate(
    covid_period = case_when(
      year == 2019 ~ "Pre-COVID",
      year %in% c(2020, 2021, 2022) ~ "COVID Period"
    )
  )

# New variable 3: Classify tobacco consumption levels
merged_data <- merged_data %>%
  mutate(
    tobacco_level = case_when(
      is.na(tobacco_rate) ~ "No Data",
      tobacco_rate <= 10 ~ "Low Smoking",
      tobacco_rate <= 20 ~ "Moderate Smoking", 
      tobacco_rate <= 30 ~ "High Smoking",
      TRUE ~ "Very High Smoking"
    )
  )

# Save the cleaned data so we can use it later
write.csv(merged_data, "cleaned_data.csv", row.names = FALSE)
