## Script to extract ecoclimatic index (EI) values from Calonectria
## pseudonaviculata occurrence records for North America as a form of model
## validation

# Libraries
library(raster)
library(sf)
library(tidyverse)

# Create folder for analysis results if it doesn't already exist
if (!file.exists(here("CLIMEX", "Final_outfls", "Analysis"))) {
  dir.create(here("CLIMEX", "Final_outfls", "Analysis"))
}

# Rasters (also extract growth index values [GI])
EI_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI_World.tif"))
GI_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "GI_World.tif"))

# Occurrence records
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
coordinates(recs) <- ~Longitude + Latitude
crs(recs) <- CRS("+proj=longlat +datum=WGS84")
recs_sf <- st_as_sf(recs)

# Extract results for each record (includes duplicate cells)
recs_extract <- data.frame("EI" = raster::extract(EI_rast, recs_sf),
                           "GI" = raster::extract(GI_rast, recs_sf)) %>%
  mutate("Continent" = recs_sf$Continent, 
         "Lat" = recs_sf$Latitude, "Long" = recs_sf$Longitude,
         "Site" = recs_sf$Site, "Country" = recs_sf$Country,
         "State" = recs_sf$Region) 

# Summarize range of EI and GI values for continents and countries
sum_stats.set <- recs_extract %>% 
  mutate(Set = ifelse(
    Continent %in% c("North America", "New Zealand"), "Validation", "Fitting")) %>%
  group_by(Set) %>%
  filter(!is.na(EI)) %>%
  summarise(
    mean_EI = round(mean(EI)), min_EI = min(EI), max_EI = max(EI),
    mean_GI = round(mean(GI)), min_GI = min(GI), max_GI = max(GI))
write.xlsx(sum_stats.set, here("CLIMEX", "Final_outfls", "Analysis", 
                               "Sumstats_Fit_v_Valid_EI_GI.xlsx"))

sum_stats.cntry <- recs_extract %>% 
  group_by(Country) %>%
  filter(!is.na(EI)) %>%
  summarise(
    mean_EI = round(mean(EI)), min_EI = min(EI), max_EI = max(EI),
    mean_GI = round(mean(GI)), min_GI = min(GI), max_GI = max(GI))
write.xlsx(sum_stats.cntry, here("CLIMEX", "Final_outfls", "Analysis", 
                                 "Sumstats_country_EI_GI.xlsx"))

# Tally how many localities have EI less than 10
tally_EILt10 <- recs_extract %>% 
  group_by(Country) %>%
  filter(EI < 10) %>%
  count(., name = "n_Lt10") 
tallyEI0 <- recs_extract %>% 
  group_by(Country) %>%
  filter(EI == 0) %>%
  count(., name = "n_0") 
tally_both <- left_join(tally_EILt10, tallyEI0)
write.xlsx(tally_both, here("CLIMEX", "Final_outfls", "Analysis", 
                            "N_locs_EI_Lt10.xlsx"))

gc()