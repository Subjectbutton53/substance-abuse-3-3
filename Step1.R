OECD <- read_csv("Downloads/OECD.ELS.HD,DSD_HEALTH_LVNG@DF_HEALTH_LVNG_TC,+all.csv")

#make adjusments


write_csv(OECD, "OECD2.csv")

OECD_mortality <- read_csv("~/Downloads/OECD.ELS.HD,DSD_HEALTH_STAT@DF_COM,1.0+all.csv")


write_csv(OECD_mortality, "OECD_mortality.csv")


head(OECD)

head(OECD_mortality)
library(data.table)
uniqueN(OECD_mortality$REF_AREA)

head(OECD_mortality)

library(tidyverse)


inner_join(OECD, OECD_mortality, by = "Reference area")


#git init





