# PART 3: QUANTIFYING - looking at events and stuff
# 04_event_analysis.R
# figuring out what COVID did to death rates

# Load necessary packages
library(dplyr)
library(ggplot2)
library(tidyr)

# Load the cleaned data (run 02_data_cleaning.R first obviously)
data <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

# 3.6 Event Analysis - what COVID did

# Print the start of the analysis
print("starting event analysis stuff...")

# Step 1: Average deaths before and after COVID
covid_analysis <- data %>%
  group_by(covid_period, gender) %>%
  summarise(
    avg_mortality = mean(mortality_rate, na.rm = TRUE),
    median_mortality = median(mortality_rate, na.rm = TRUE),
    count = n(),
    .groups = 'drop'
  )

print("COVID stuff Summary:")
print(covid_analysis)

# Step 2: Compare death rates before and during COVID using boxplot
plot_covid_impact <- data %>%
  group_by(covid_period, gender, country) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = covid_period, y = avg_mortality, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Death Rates Before and During COVID-19",
    subtitle = "Comparing before COVID (2019) vs COVID time (2020-2022)",
    x = "Time Period",
    y = "Average Death Rate (per 100,000)",
    fill = "Gender"
  ) +
  theme_minimal()

# Save the COVID impact plot
ggsave("plot_covid_impact.png", plot_covid_impact, width = 10, height = 6)

# Step 3: Country-level COVID impact analysis
country_covid_impact <- data %>%
  group_by(country, covid_period) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE), .groups = 'drop') %>%
  pivot_wider(names_from = covid_period, values_from = avg_mortality) %>%
  mutate(
    covid_change = `COVID Period` - `Pre-COVID`,
    percent_change = (covid_change / `Pre-COVID`) * 100
  ) %>%
  filter(!is.na(covid_change)) %>%
  arrange(desc(covid_change))

print("Countries with biggest increases in deaths during COVID:")
print(head(country_covid_impact, 10))

# Step 4: Plot showing change in deaths by country
top_impacted_countries <- head(country_covid_impact, 15)$country

plot_country_covid <- data %>%
  filter(country %in% top_impacted_countries) %>%
  group_by(country, covid_period) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = country, y = avg_mortality, fill = covid_period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "COVID-19 Impact by Country",
    subtitle = "Top 15 countries with biggest death increases",
    x = "Country",
    y = "Average Death Rate (per 100,000)",
    fill = "Period"
  ) +
  theme_minimal()

# Save the country COVID impact plot
ggsave("plot_country_covid_impact.png", plot_country_covid, width = 10, height = 8)

# Step 5: Year-by-year death trend analysis
yearly_analysis <- data %>%
  group_by(year, gender) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE), .groups = 'drop')

print("Year-by-year death trends:")
print(yearly_analysis)

# Plot showing year-by-year trends
plot_yearly <- ggplot(yearly_analysis, aes(x = year, y = avg_mortality, color = gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 2019.3, y = max(yearly_analysis$avg_mortality) * 0.9, 
           label = "COVID-19\nstarts", hjust = 1, color = "red") +
  labs(
    title = "Death Trends: Before and During COVID-19",
    x = "Year",
    y = "Average Death Rate (per 100,000)",
    color = "Gender"
  ) +
  theme_minimal()

# Save the year-by-year COVID trend plot
ggsave("plot_yearly_covid_trends.png", plot_yearly, width = 10, height = 6)

# Step 6: Statistical Test - t-test to check if the difference in mortality rate is significant
pre_covid_data <- data %>% filter(covid_period == "Pre-COVID") %>% pull(mortality_rate)
covid_data <- data %>% filter(covid_period == "COVID Period") %>% pull(mortality_rate)

# Perform t-test
t_test_result <- t.test(covid_data, pre_covid_data)

# Print t-test results
print("T-test results:")
print(paste("Before COVID mean:", round(mean(pre_covid_data, na.rm = TRUE), 2)))
print(paste("COVID period mean:", round(mean(covid_data, na.rm = TRUE), 2)))
print(paste("p-value:", round(t_test_result$p.value, 4)))

# Interpretation of the t-test result
if(t_test_result$p.value < 0.05) {
  print("The difference IS real (p < 0.05)")
} else {
  print("The difference is NOT real (p >= 0.05)")
}

# Final Summary
print("=== EVENT ANALYSIS SUMMARY ===")
print("Event we looked at: COVID-19 virus (starting 2020)")
print("Before period: 2019")
print("After period: 2020-2022")
print("Summary:")
print(paste("- Average deaths went up from", 
            round(mean(pre_covid_data, na.rm = TRUE), 2),
            "to", round(mean(covid_data, na.rm = TRUE), 2)))
print("- Made 3 plots showing what happened")
print("- Performed a t-test for statistical significance")

print("Event analysis completed!")
