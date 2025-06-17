# PART 3: QUANTIFYING - making pretty pictures
# 03_visualizations.R
# making plots for time stuff, country stuff, and people groups

# get packages for making plots
library(dplyr)
library(ggplot2)

# get the clean data (run 02_data_cleaning.R first duh)
data <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

# 3.3 make pictures showing Time stuff

# calculate average deaths by year and boy/girl
temporal_variation <- data %>%
  group_by(year, gender) %>%
  summarise(
    avg_mortality = mean(mortality_rate, na.rm = TRUE),
    .groups = 'drop'
  )

# make line plot showing whats happening over time
plot_temporal_variation <- ggplot(temporal_variation, aes(x = year, y = avg_mortality, color = gender)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Average Death Rates Over Time (2019-2022)",
    x = "Year",
    y = "Average Death Rate (per 100,000)",
    color = "Gender"
  ) +
  theme_minimal()

# save the plot thing
ggsave("plot_temporal_variation.png", plot_temporal_variation, width = 10, height = 6)

# 3.4 make pictures showing Country differences

# get data for the newest year (2022)
spatial_data <- data %>%
  filter(year == 2022) %>%
  group_by(country, gender) %>%
  summarise(
    avg_mortality = mean(mortality_rate, na.rm = TRUE),
    .groups = 'drop'
  )

# find top 10 countrys with most deaths
top_countries <- spatial_data %>%
  group_by(country) %>%
  summarise(total_mortality = mean(avg_mortality, na.rm = TRUE)) %>%
  arrange(desc(total_mortality)) %>%
  top_n(10) %>%
  pull(country)

# make bar chart for top countrys
plot_spatial <- spatial_data %>%
  filter(country %in% top_countries) %>%
  ggplot(aes(x = reorder(country, avg_mortality), y = avg_mortality, fill = gender)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Death Rates by Country (2022)",
    subtitle = "Top 10 countrys with most deaths",
    x = "Country",
    y = "Average Death Rate (per 100,000)",
    fill = "Gender"
  ) +
  theme_minimal()

# save this plot too
ggsave("plot_spatial_variation.png", plot_spatial, width = 10, height = 8)

# 3.5 make pictures showing Sub-Population differences

# make boxplot showing stuff by boy/girl and covid time
plot_subpop <- data %>%
  filter(!is.na(mortality_rate)) %>%
  ggplot(aes(x = covid_period, y = mortality_rate, fill = gender)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "Death Rate spread by COVID time and Gender",
    x = "Time Period", 
    y = "Death Rate (per 100,000, log scale)",
    fill = "Gender"
  ) +
  theme_minimal()

# save this one also
ggsave("plot_subpopulation_variation.png", plot_subpop, width = 10, height = 6)

# extra plot: deaths by smoking level

plot_tobacco <- data %>%
  filter(tobacco_level != "No Data") %>%
  ggplot(aes(x = tobacco_level, y = mortality_rate, fill = tobacco_level)) +
  geom_boxplot() +
  facet_wrap(~gender) +
  labs(
    title = "Death Rates by how much people Smoke",
    x = "Smoking Level",
    y = "Death Rate (per 100,000)",
    fill = "Smoking Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# save this plot to
ggsave("plot_tobacco_mortality.png", plot_tobacco, width = 12, height = 6)


top_mortality <- spatial_data %>%
  group_by(country) %>%
  summarise(avg_mortality = mean(avg_mortality)) %>%
  arrange(desc(avg_mortality)) %>%
  head(5)

covid_comparison <- data %>%
  group_by(covid_period, gender) %>%
  summarise(
    avg_mortality = mean(mortality_rate, na.rm = TRUE),
    median_mortality = median(mortality_rate, na.rm = TRUE),
    .groups = 'drop'
  )