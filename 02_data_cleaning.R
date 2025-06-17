# PART 3: QUANTIFYING - making data clean and stuff
# 02_data_cleaning.R
# cleaning up the messy data and making new things

# get packages we need
library(dplyr)

# get the data (you gotta run 01_data_loading.R first obviosly)
OECD_mortality <- read.csv("OECD_mortality.csv", stringsAsFactors = FALSE)
OECD2 <- read.csv("OECD2.csv", stringsAsFactors = FALSE)

# 3.1 clean up the Data

# step 1: only get the new years (2019-2022) cuz old stuff is boring
mortality_clean <- OECD_mortality %>%
  filter(TIME_PERIOD %in% c(2019, 2020, 2021, 2022)) %>%
  select(
    country = Reference.area,
    gender = Sex,
    year = TIME_PERIOD,
    mortality_rate = OBS_VALUE,
    cause_of_death = Cause.of.death
  )

tobacco_clean <- OECD2 %>%
  filter(TIME_PERIOD %in% c(2019, 2020, 2021, 2022)) %>%
  select(
    country = Reference.area,
    gender = Sex,
    year = TIME_PERIOD,
    tobacco_rate = OBS_VALUE
  )

# step 2: get rid of missing stuff cuz we dont need it
mortality_clean <- mortality_clean %>%
  filter(!is.na(mortality_rate))

tobacco_clean <- tobacco_clean %>%
  filter(!is.na(tobacco_rate))

# step 3: only keep male and female (remove wierd other categorys)
mortality_clean <- mortality_clean %>%
  filter(gender %in% c("Male", "Female"))

tobacco_clean <- tobacco_clean %>%
  filter(gender %in% c("Male", "Female"))

# step 4: put the datasets together like puzzle peices
merged_data <- mortality_clean %>%
  left_join(tobacco_clean, by = c("country", "gender", "year"))

# 3.2 make New Variables
# new thing 1: death categories or whatever
merged_data <- merged_data %>%
  mutate(
    mortality_category = case_when(
      mortality_rate <= 1 ~ "variable1",
      mortality_rate <= 10 ~ "variable10",
      mortality_rate <= 50 ~ "variable50"
    )
  )

# new thing 2: before and after covid thing
merged_data <- merged_data %>%
  mutate(
    covid_period = case_when(
      year == 2019 ~ "Pre-COVID",
      year %in% c(2020, 2021, 2022) ~ "COVID Period"
    )
  )

# new thing 3: how much people smoke levels
merged_data <- merged_data %>%
  mutate(
    tobacco_level = case_when(
      is.na(tobacco_rate) ~ "No Data",
      tobacco_rate <= 10 ~ "variable1 Smoking",
      tobacco_rate <= 20 ~ "variable10 Smoking", 
      tobacco_rate <= 30 ~ "variable50 Smoking",
      TRUE ~ "Very variable50 Smoking"
    )
  )

# save the clean data so we can use it later
write.csv(merged_data, "cleaned_data.csv", row.names = FALSE)