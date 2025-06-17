# PART 3: QUANTIFYING - looking at events and stuff
# 04_event_analysis.R
# figuring out what covid did to death rates

# get packages we need
library(dplyr)
library(ggplot2)
library(tidyr)

# get the clean data (run 02_data_cleaning.R first obviosly)
data <- read.csv("cleaned_data.csv", stringsAsFactors = FALSE)

print("starting event analysis stuff...")

# 3.6 Event Analysis - what COVID did

# the thing we're looking at: COVID-19 virus thing
# before: 2019 (before COVID)
# after: 2020-2022 (when COVID happend)

print("looking at what COVID did to death rates...")

# calculate average deaths before and during covid
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

# make before/after comparison plot
plot_covid_impact <- data %>%
  group_by(covid_period, gender, country) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = covid_period, y = avg_mortality, fill = gender)) +
  geom_boxplot() +
  labs(
    title = "Death Rates Before and During COVID-19",
    subtitle = "comparing before COVID (2019) vs COVID time (2020-2022)",
    x = "Time Period",
    y = "Average Death Rate (per 100,000)",
    fill = "Gender"
  ) +
  theme_minimal()

# save the plot
ggsave("plot_covid_impact.png", plot_covid_impact, width = 10, height = 6)
print("saved: plot_covid_impact.png")

# country specific COVID stuff
print("looking at COVID stuff by country...")

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

print("countrys with biggest increases in deaths during COVID:")
print(head(country_covid_impact, 10))

# plot showing change in deaths by country
top_impacted_countries <- head(country_covid_impact, 15)$country

plot_country_covid <- data %>%
  filter(country %in% top_impacted_countries) %>%
  group_by(country, covid_period) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE), .groups = 'drop') %>%
  ggplot(aes(x = country, y = avg_mortality, fill = covid_period)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "COVID-19 affect by Country",
    subtitle = "top 15 countrys with biggest death increases",
    x = "Country",
    y = "Average Death Rate (per 100,000)",
    fill = "Period"
  ) +
  theme_minimal()

# save this plot
ggsave("plot_country_covid_impact.png", plot_country_covid, width = 10, height = 8)
print("saved: plot_country_covid_impact.png")

# year by year analysis to show what happend
yearly_analysis <- data %>%
  group_by(year, gender) %>%
  summarise(avg_mortality = mean(mortality_rate, na.rm = TRUE), .groups = 'drop')

print("year by year death trends:")
print(yearly_analysis)

# plot showing year by year trends
plot_yearly <- ggplot(yearly_analysis, aes(x = year, y = avg_mortality, color = gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2019.5, linetype = "dashed", color = "red", alpha = 0.7) +
  annotate("text", x = 2019.3, y = max(yearly_analysis$avg_mortality) * 0.9, 
           label = "COVID-19\nstarts", hjust = 1, color = "red") +
  labs(
    title = "Death trends: Before and During COVID-19",
    x = "Year",
    y = "Average Death Rate (per 100,000)",
    color = "Gender"
  ) +
  theme_minimal()

# save this plot
ggsave("plot_yearly_covid_trends.png", plot_yearly, width = 10, height = 6)
print("saved: plot_yearly_covid_trends.png")

# math test to see if the differance is real
print("testing if COVID affect is actualy real...")

pre_covid_data <- data %>% filter(covid_period == "Pre-COVID") %>% pull(mortality_rate)
covid_data <- data %>% filter(covid_period == "COVID Period") %>% pull(mortality_rate)

# do t-test thing
t_test_result <- t.test(covid_data, pre_covid_data)
print("t-test results:")
print(paste("before COVID mean:", round(mean(pre_covid_data, na.rm = TRUE), 2)))
print(paste("COVID period mean:", round(mean(covid_data, na.rm = TRUE), 2)))
print(paste("p-value:", round(t_test_result$p.value, 4)))

if(t_test_result$p.value < 0.05) {
  print("the differance IS real (p < 0.05)")
} else {
  print("the differance is NOT real (p >= 0.05)")
}

# summary for the report thing
print("=== EVENT ANALYSIS SUMMARY ===")
print("event we looked at: COVID-19 virus (starting 2020)")
print("before period: 2019")
print("after period: 2020-2022")
print("")
print("what we found:")
print(paste("- average deaths went up from", 
            round(mean(pre_covid_data, na.rm = TRUE), 2),
            "to", round(mean(covid_data, na.rm = TRUE), 2)))
print("- made 3 plots showing what happend")
print("- did math test for significance")

print("event analysis done!") 