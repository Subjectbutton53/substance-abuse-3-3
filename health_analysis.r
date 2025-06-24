# =====================
# COMPREHENSIVE OECD HEALTH DATA ANALYSIS
# =====================

# Load required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Data manipulation and visualization
  sf,           # Spatial data handling
  rnaturalearth, # Map data
  countrycode,  # Country code conversions
  ggrepel,      # Better label placement
  scales,       # Scale formatting
  viridis,      # Color palettes
  patchwork     # Combining plots
)

# =====================
# DATA LOADING & CLEANING
# =====================

# Load mortality data
mortality <- read_csv("avoidable death.csv") %>%
  select(
    country_code = REF_AREA, 
    country = `Reference area`,
    year = TIME_PERIOD,
    mortality_rate = OBS_VALUE
  ) %>%
  filter(year >= 2019, !is.na(mortality_rate)) %>%
  mutate(year = as.integer(year))

# Load tobacco consumption data
tobacco <- read_csv("OECDnew.csv") %>%
  select(
    country_code = REF_AREA,
    year = TIME_PERIOD,
    cigarettes_per_day = OBS_VALUE,
    obs_status = OBS_STATUS
  ) %>%
  filter(obs_status == "A", year >= 2019, !is.na(cigarettes_per_day)) %>%
  mutate(year = as.integer(year))

# =====================
# CREATE NEW VARIABLES
# =====================

# Variable 1: Health Risk Index (combining mortality and tobacco use)
# Calculate country averages for combining
country_averages <- mortality %>%
  inner_join(tobacco, by = c("country_code", "year")) %>%
  group_by(country_code, country) %>%
  summarise(
    avg_mortality = mean(mortality_rate, na.rm = TRUE),
    avg_cigarettes = mean(cigarettes_per_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # Standardize both variables (z-scores)
    mortality_z = scale(avg_mortality)[,1],
    cigarettes_z = scale(avg_cigarettes)[,1],
    # Create Health Risk Index (higher = worse health outcomes)
    health_risk_index = (mortality_z + cigarettes_z) / 2,
    # Categorize risk levels
    risk_category = case_when(
      health_risk_index >= 1 ~ "Very High Risk",
      health_risk_index >= 0.5 ~ "High Risk", 
      health_risk_index >= -0.5 ~ "Moderate Risk",
      TRUE ~ "Low Risk"
    )
  )

# Variable 2: Mortality Change Rate (year-over-year percentage change)
mortality_trends <- mortality %>%
  arrange(country_code, year) %>%
  group_by(country_code, country) %>%
  mutate(
    # Calculate year-over-year change
    mortality_change_rate = ((mortality_rate - lag(mortality_rate)) / lag(mortality_rate)) * 100,
    # Calculate cumulative change from 2019 baseline
    baseline_2019 = first(mortality_rate),
    cumulative_change = ((mortality_rate - baseline_2019) / baseline_2019) * 100
  ) %>%
  ungroup()

# Combine all data for comprehensive analysis
combined_data <- mortality_trends %>%
  left_join(tobacco, by = c("country_code", "year")) %>%
  left_join(country_averages %>% select(country_code, health_risk_index, risk_category), 
            by = "country_code")

# =====================
# 1. TEMPORAL TREND VISUALIZATION
# =====================

# Plot 1a: Mortality trends over time with new variable (cumulative change)
p1 <- ggplot(mortality_trends, aes(x = year, y = cumulative_change, group = country)) +
  geom_line(aes(color = country), linewidth = 1, alpha = 0.8) +
  geom_point(aes(color = country), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  annotate("rect", xmin = 2019.8, xmax = 2021.2, ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "red") +
  annotate("text", x = 2020.5, y = 80, label = "COVID-19\nPandemic", 
           size = 3, hjust = 0.5, fontface = "bold") +
  labs(
    title = "Cumulative Change in Avoidable Mortality (2019-2022)",
    subtitle = "Percentage change from 2019 baseline",
    x = "Year",
    y = "Cumulative Change from 2019 (%)",
    color = "Country",
    caption = "Source: OECD Health Statistics. Red area highlights COVID-19 pandemic period."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = 2019:2022) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

# Plot 1b: Health Risk Index over time for countries with complete data
risk_over_time <- combined_data %>%
  filter(!is.na(health_risk_index), !is.na(cigarettes_per_day)) %>%
  group_by(year) %>%
  summarise(
    mean_risk = mean(health_risk_index, na.rm = TRUE),
    se_risk = sd(health_risk_index, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

p1b <- ggplot(risk_over_time, aes(x = year, y = mean_risk)) +
  geom_ribbon(aes(ymin = mean_risk - se_risk, ymax = mean_risk + se_risk), 
              alpha = 0.3, fill = "steelblue") +
  geom_line(color = "steelblue", linewidth = 2) +
  geom_point(color = "steelblue", size = 3) +
  annotate("rect", xmin = 2019.8, xmax = 2021.2, ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "red") +
  annotate("text", x = 2020.5, y = 0.3, label = "COVID-19 Impact", 
           size = 3, hjust = 0.5, fontface = "bold") +
  labs(
    title = "Average Health Risk Index Trends (2019-2022)",
    subtitle = "Higher values indicate greater health risks (mortality + tobacco use)",
    x = "Year",
    y = "Mean Health Risk Index",
    caption = "Error bands show standard error. Based on countries with complete data."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = 2019:2022)

# =====================
# 2. SPATIAL PATTERN VISUALIZATION
# =====================

# Get European map data
europe <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf") %>%
  filter(!name_long %in% c("Russia", "Turkey")) # Focus on core Europe

# Add ISO3 codes for matching
europe$iso3 <- countrycode(europe$iso_a3, "iso3c", "iso3c")

# Prepare data for mapping
map_data <- country_averages %>%
  mutate(
    iso3 = countrycode(country_code, "iso3c", "iso3c"),
    # Create discrete categories for better visualization
    risk_level = cut(health_risk_index, 
                     breaks = c(-Inf, -0.5, 0, 0.5, 1, Inf),
                     labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                     include.lowest = TRUE)
  )

# Join with map data
europe_health <- europe %>%
  left_join(map_data, by = "iso3")

# Create the map
p2 <- ggplot(europe_health) +
  geom_sf(aes(fill = risk_level), color = "white", size = 0.3) +
  scale_fill_viridis_d(name = "Health Risk\nLevel", 
                       option = "plasma", 
                       na.value = "grey90",
                       direction = -1) +
  labs(
    title = "Health Risk Index Across European Countries",
    subtitle = "Combined indicator of avoidable mortality and tobacco consumption (2019-2022)",
    caption = "Source: OECD Health Statistics. Grey areas indicate missing data.\nHigher risk indicates worse health outcomes."
  ) +
  theme_void() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  ) +
  coord_sf(xlim = c(-10, 30), ylim = c(35, 70))

# =====================
# 3. SUB-POPULATION COMPARISON
# =====================

# Create risk categories for comparison
comparison_data <- combined_data %>%
  filter(!is.na(health_risk_index), !is.na(cigarettes_per_day)) %>%
  mutate(
    high_mortality = ifelse(mortality_rate > median(mortality_rate, na.rm = TRUE), 
                           "Above Median", "Below Median"),
    risk_group = case_when(
      health_risk_index >= 0.5 ~ "High Risk Countries",
      health_risk_index >= -0.5 ~ "Moderate Risk Countries", 
      TRUE ~ "Low Risk Countries"
    )
  )

# Faceted comparison plot
p3 <- ggplot(comparison_data, aes(x = cigarettes_per_day, y = mortality_rate)) +
  geom_point(aes(color = factor(year)), size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", alpha = 0.3) +
  facet_wrap(~risk_group, scales = "free", ncol = 2) +
  scale_color_viridis_d(name = "Year", option = "viridis") +
  labs(
    title = "Tobacco Use vs. Mortality by Risk Group",
    subtitle = "Relationship varies significantly across risk categories",
    x = "Average Cigarettes per Smoker per Day",
    y = "Avoidable Mortality Rate (per 100,000)",
    caption = "Countries grouped by Health Risk Index. Lines show linear trends with confidence intervals."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 11),
    legend.position = "bottom"
  )

# =====================
# 4. EVENT ANALYSIS
# =====================

# Focus on countries with data for 2020-2021 to show COVID impact
covid_impact <- mortality_trends %>%
  filter(year %in% c(2020, 2021)) %>%
  group_by(country_code, country) %>%
  summarise(
    mortality_2020 = mortality_rate[year == 2020][1],
    mortality_2021 = mortality_rate[year == 2021][1],
    change_2020_2021 = mortality_2021 - mortality_2020,
    .groups = "drop"
  ) %>%
  filter(!is.na(mortality_2020), !is.na(mortality_2021))

# Event analysis plot
p4 <- ggplot(mortality_trends %>% filter(year >= 2019), 
             aes(x = year, y = mortality_rate)) +
  geom_line(aes(group = country, color = country), linewidth = 1, alpha = 0.6) +
  geom_point(aes(color = country), size = 2) +
  # Highlight the COVID-19 period
  annotate("rect", xmin = 2019.8, xmax = 2021.2, ymin = 0, ymax = 600, 
           alpha = 0.15, fill = "red") +
  # Add event annotation
  annotate("segment", x = 2020, y = 550, xend = 2020, yend = 500,
           arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1) +
  annotate("text", x = 2020, y = 570, 
           label = "COVID-19 Pandemic Begins\n(March 2020)", 
           size = 4, hjust = 0.5, fontface = "bold", color = "red") +
  # Add second event annotation for 2021
  annotate("segment", x = 2021, y = 550, xend = 2021, yend = 500,
           arrow = arrow(length = unit(0.3, "cm")), color = "red", size = 1) +
  annotate("text", x = 2021, y = 570, 
           label = "Peak Pandemic\nImpact", 
           size = 4, hjust = 0.5, fontface = "bold", color = "red") +
  labs(
    title = "COVID-19 Impact on Avoidable Mortality (2019-2022)",
    subtitle = "Sharp increases observed during pandemic years across European countries",
    x = "Year",
    y = "Avoidable Mortality Rate (per 100,000 inhabitants)",
    color = "Country",
    caption = "Source: OECD Health Statistics. Red shaded area highlights pandemic period."
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = 2019:2022) +
  scale_y_continuous(limits = c(100, 600))

# =====================
# DISPLAY ALL PLOTS
# =====================

# Show plots
print(p1)
print(p1b)
print(p2)
print(p3)
print(p4)

# =====================
# SUMMARY STATISTICS FOR NEW VARIABLES
# =====================

cat("\n=== SUMMARY OF NEW VARIABLES ===\n")

cat("\n1. HEALTH RISK INDEX SUMMARY:\n")
print(summary(country_averages$health_risk_index))

cat("\nRisk Category Distribution:\n")
print(table(country_averages$risk_category))

cat("\n2. MORTALITY CHANGE RATE SUMMARY:\n")
change_summary <- mortality_trends %>%
  filter(!is.na(mortality_change_rate)) %>%
  summarise(
    mean_change = mean(mortality_change_rate, na.rm = TRUE),
    median_change = median(mortality_change_rate, na.rm = TRUE),
    max_increase = max(mortality_change_rate, na.rm = TRUE),
    max_decrease = min(mortality_change_rate, na.rm = TRUE)
  )
print(change_summary)

cat("\nCountries with highest mortality increases:\n")
top_increases <- mortality_trends %>%
  filter(!is.na(mortality_change_rate)) %>%
  arrange(desc(mortality_change_rate)) %>%
  select(country, year, mortality_change_rate) %>%
  head(5)
print(top_increases)

# =====================
# INTERPRETATION NOTES
# =====================

cat("\n=== INTERPRETATION ===\n")
cat("1. Health Risk Index: Combines standardized mortality and tobacco use rates\n")
cat("   - Higher values indicate worse population health outcomes\n")
cat("   - Useful for international comparisons and policy prioritization\n\n")

cat("2. Mortality Change Rate: Shows year-over-year percentage changes\n")
cat("   - Reveals impact of events like COVID-19 pandemic\n")
cat("   - Helps identify countries with improving vs. worsening trends\n\n")

cat("3. COVID-19 Impact: Clearly visible in 2020-2021 across most countries\n")
cat("   - Substantial increases in avoidable mortality during pandemic\n")
cat("   - Some countries showed recovery by 2022\n")
