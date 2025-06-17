# PART 3: QUANTIFYING - making pretty pictures
# 03_visualizations.R
# creating plots for time trends, country differences, and sub-population analysis

# Load necessary packages
library(dplyr)
library(ggplot2)

# Load the cleaned data (run 02_data_cleaning.R first obviously)
data <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

# 3.3 Time-based visualizations: Average deaths by year and gender

# Calculate average mortality rate by year and gender
temporal_variation <- data %>%
  group_by(year, gender) %>%
  summarise(
    avg_mortality = mean(mortality_rate, na.rm = TRUE),
    .groups = 'drop'
  )

# Create line plot showing the average death rate over time (2019-2022)
plot_temporal_variation <- ggplot(temporal_variation, aes(x = year, y = avg_mortality, color = gender)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Mortality Rates Over Time (2019-2022)",
    x = "Year",
    y = "Average Mortality Rate (per 100,000)",
    color = "Gender"
  ) +
  theme_minimal()

# Save the plot for temporal variation
ggsave("plot_temporal_variation.png", plot_temporal_variation, width = 10, height = 6)

# 3.4 Country-level visualizations: Death rates by country in 2022

# Filter data for the latest year (2022)
spatial_data <- data %>%
  filter(year == 2022) %>%
  group_by(country, gender) %>%
  summarise(
    avg_mortality = mean(mortality_rate, na.rm = TRUE),
    .groups = 'drop'
  )

# Find top 10 countries with the highest death rates
top_countries <- spatial_data %>%
  group_by(country) %>%
  summarise(total_mortality = mean(avg_mortality, na.rm = TRUE)) %>%
  arrange(desc(total_mortality)) %>%
  top_n(10) %>%
  pull(country)

# Create bar chart for the top 10 countries with the highest death rates in 2022
plot_spatial <- spatial_data %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = reorder(country, avg_mortality), y = avg_mortality, fill = gender)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Death Rates by Country (2022)",
    subtitle = "Top 10 countries with highest death rates",
    x = "Country",
    y = "Average Mortality Rate (per 100,000)",
    fill = "Gender"
  ) +
  theme_minimal()

# Save the spatial variation plot
ggsave("plot_spatial_variation.png", plot_spatial, width = 10, height = 8)

# 3.5 Sub-population visualizations: Death rates by smoking level and COVID period

# Create boxplot showing death rate spread by COVID period and gender
plot_subpop <- data %>%
  filter(!is.na(mortality_rate)) %>%
  ggplot(aes(x = covid_period, y = mortality_rate, fill = gender)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Death Rate Spread by COVID Time Period and Gender",
    x = "Time Period", 
    y = "Death Rate (per 100,000, log scale)",
    fill = "Gender"
  ) +
  theme_minimal()

# Save the sub-population variation plot
ggsave("plot_subpopulation_variation.png", plot_subpop, width = 10, height = 6)

# Additional plot: Death rates by smoking level
plot_tobacco <- data %>%
  filter(tobacco_level != "No Data") %>%
  ggplot(aes(x = tobacco_level, y = mortality_rate, fill = tobacco_level)) +
  geom_boxplot() +
  facet_wrap(~gender) +
  labs(
    title = "Death Rates by Smoking Level",
    x = "Smoking Level",
    y = "Death Rate (per 100,000)",
    fill = "Smoking Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# Save the tobacco-related mortality plot
ggsave("plot_tobacco_mortality.png", plot_tobacco, width = 12, height = 6)
