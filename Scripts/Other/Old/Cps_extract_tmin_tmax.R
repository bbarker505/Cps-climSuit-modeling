# Get info on hottest/coldest localities to help fine tune temp stress params 
# CliMond data are weekly (1960-1990) and have a 10' resolution
# WorldClim v2 data are monthly (1970-2000) and have a 10' resolution
# This anlaysis is conducted only for Eurasian localities, so that North American
# localities can be used for model validation
library(tidyverse)
library(raster)
library(openxlsx)

# Get bioclimatic data
bio5.w <- raster("C:/Users/barkebri/Documents/GIS/Data/Worldclim/Bio1_19/rast/bio5")
bio5.m <- raster("C:/Users/barkebri/Documents/GIS/Data/Worldclim_v2/wc2.1_2.5m_bio_5.tif")
bio6.w <- raster("C:/Users/barkebri/Documents/GIS/Data/Worldclim/Bio1_19/rast/bio6")
bio6.m <- raster("C:/Users/barkebri/Documents/GIS/Data/Worldclim_v2/wc2.1_2.5m_bio_6.tif")

# Locality records
recs <- read.xlsx(here("Records", "boxwood_locations_updated_Apr2021.xlsx")) %>%
  filter(Continent %in% c("Europe", "Asia", "New Zealand")) 
coordinates(recs) <- ~ Longitude + Latitude

# Maximum temp of warmest week/month
# Just order by bio5.w - results are same for both (same 15 sites)
top15_hottest <- data.frame(do.call(cbind, list(recs, 
  data.frame("bio5.w" = raster::extract(bio5.w, recs)),
  data.frame("bio5.m" = raster::extract(bio5.m, recs))))) %>%
  arrange(bio5.w) %>%
  tail(n = 15L)
write.xlsx(top15_hottest, here("Records", "Analysis", "Top15_Hottest.xlsx"), 
           row.names = F)

# Minimum temp of colest week/month
top15_coldest <- data.frame(do.call(cbind, list(recs, 
  data.frame("bio6.w" = raster::extract(bio6.w, recs)),
  data.frame("bio6.m" = raster::extract(bio6.m, recs))))) %>%
  arrange(bio6.w) %>%
  head(n = 15L)
write.xlsx(top15_coldest, here("Records", "Analysis", "Top15_Coldest.xlsx"), 
           row.names = F)
