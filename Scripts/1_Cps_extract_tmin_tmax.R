# Get info on hottest/coldest localities to help fine tune temp stress params 
# CliMond data are weekly (1960-1990) and have a 10' resolution
# WorldClim v2 data are monthly (1970-2000) and have a 10' resolution
# This anlaysis is conducted only for Eurasian localities, so that North American
# localities can be used for model validation
library(tidyverse)
library(raster)
library(openxlsx)

# Create output folder if it doesn't already exist
if (!file.exists(here("Records", "Analysis"))) {
  dir.create(here("Records", "Analysis"))
}

# Get bioclimatic data
bio5 <- raster(here("CliMond_raw", "CM10_1975H_Bio05_V1.2.txt"))
bio6 <- raster(here("CliMond_raw", "CM10_1975H_Bio06_V1.2.txt"))

# Locality records
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) %>%
  filter(Continent %in% c("Europe", "Asia"))
coordinates(recs) <- ~ Longitude + Latitude

# Maximum temp of warmest week
# Just order by bio5.w - results are same for both (same 15 sites)
top15_hottest <- data.frame(do.call(cbind, list(recs, 
  data.frame("bio5" = raster::extract(bio5, recs))))) %>%
  arrange(bio5) %>%
  tail(n = 15L)
write.xlsx(top15_hottest, here("Records", "Analysis", "Top15_Hottest.xlsx"), 
           row.names = FALSE, overwrite = TRUE)

# Minimum temp of coldest week
top15_coldest <- data.frame(do.call(cbind, list(recs, 
  data.frame("bio6" = raster::extract(bio6, recs))))) %>%
  arrange(bio6) %>%
  head(n = 15L)
write.xlsx(top15_coldest, here("Records", "Analysis", "Top15_Coldest.xlsx"), 
           row.names = FALSE, overwrite = TRUE)

rm(list = ls())