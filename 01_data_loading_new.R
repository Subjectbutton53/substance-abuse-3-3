# PART 2: DATA SOURCING
# 01_data_loading.R
# loading and looking at OECD health stuff

# Load necessary packages
library(dplyr)
library(readr)

# 2.1 Get Data

# Read the OECDnew dataset (Tobacco Consumption data)
OECD_new <- read_csv("OECDnew.csv")

# Read the avoidable death dataset
avoidable_death <- read_csv("avoidable death.csv")

# 2.2 Look at the datasets
print("OECDnew dataset:")
head(OECD_new)

print("avoidable death dataset:")
head(avoidable_death)

# Print column names to check the structure
print("OECDnew dataset columns:")
print(colnames(OECD_new))

print("avoidable death dataset columns:")
print(colnames(avoidable_death))

# Check the dimensions of the datasets
print("Dimensions of OECDnew dataset:")
print(dim(OECD_new))

print("Dimensions of avoidable death dataset:")
print(dim(avoidable_death))

# 2.3 Describe the variables in the datasets
print("OECDnew dataset has:")
print("- Country and gender information")
print("- Time period (years)")
print("- Tobacco consumption data (%)")

print("avoidable death dataset has:")
print("- Country and gender information")
print("- Time period (years)")
print("- Avoidable mortality data (death rate per 100,000)")

# You may want to view the datasets visually in RStudio to get a better idea of their structure
View(OECD_new)
View(avoidable_death)

