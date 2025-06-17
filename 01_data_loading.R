# PART 2: DATA SOURCING
# 01_data_loading.R
# loading and looking at OECD health stuff

# get the packages we need i guess
library(dplyr)

# 2.1 get Data
# read the death data thing
Oecd

lol

# read the smoking data too
OECD2 <- read.csv("OECD2.csv", stringsAsFactors = FALSE)

library(readr)
OECD2 <- read_csv("projectP4E/OECD2.csv")
View(OECD2)

OECD_mortality <- read_csv("projectP4E/OECD_mortality.csv")
View(OECD_mortality)



# 2.2 look at Dataset(s) or whatever

head(OECD2)
head(OECD_mortality)


#coonent

print("first couple rows:")
print(head(OECD2))

print("what columns we got:")
print(colnames(OECD2))

print("how big is this thing:")
print(dim(OECD2))

# 2.3 describe whats in the Variables
print("death data has:")
print("- countrys and stuff")
print("- boy or girl categories") 
print("- when it happend (years)")
print("- how many people died (deaths per 100,000)")
print("- why they died")

print("smoking data has:")
print("- countrys again")
print("- boy or girl again")
print("- years again")  
print("- how much people smoke (% who smoke every day)")