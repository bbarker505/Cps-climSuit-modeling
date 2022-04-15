## Script to model outputs from Calonectria pseudonaviculata occurrence records
## for North America as a form of model validation

# Libraries
library(raster)
library(sf)
library(tidyverse)

# Create folder for analysis results if it doesn't already exist
if (!file.exists(here("CLIMEX", "Final_outfls", "Analysis"))) {
  dir.create(here("CLIMEX", "Final_outfls", "Analysis"))
}

# CLIMEX rasters (also extract growth index values [GI])
EI_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI_World.tif"))
GI_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "GI_World.tif"))

# Predictions of presence for correlative models
outdir <- "run_PCA_4algs_kfold_prev1_03-29-2022"
world_ens <- raster(here("ENMTML", "Outfiles", outdir, 
                         "Consensus_rasts", "world_ens_consensus.tif"))

# Occurrence records for North American and New Zealand 
recs <- read.xlsx(here("Records", "Cps_locations_subm_Nov2021.xlsx"))
#recs <- read.csv(here("Records", "Subsampled", "Cps_locations_noDups_10min.csv"))
recs_sf <- st_as_sf(recs, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84")
recs_val <- filter(recs_sf, Continent %in% c("North America", "New Zealand"))

## Validate CLIMEX predictions ----
# Extract results for each record 
EI_GI_recs <- data.frame("EI" = raster::extract(EI_rast, recs_sf),
                           "GI" = raster::extract(GI_rast, recs_sf)) %>%
  mutate("Continent" = recs_sf$Continent, 
         "Lat" = recs_sf$Latitude, "Long" = recs_sf$Longitude,
         "Site" = recs_sf$Site, "Country" = recs_sf$Country,
         "State" = recs_sf$Region) 

# Summarize range of EI and GI values for continents and countries
sum_stats.set <- EI_GI_recs %>% 
  mutate(Set = ifelse(
    Continent %in% c("North America", "New Zealand"), 
    "Validation", "Fitting")) %>%
  group_by(Set) %>%
  filter(!is.na(EI)) %>%
  summarise(
    mean_EI = round(mean(EI)), min_EI = min(EI), max_EI = max(EI),
    mean_GI = round(mean(GI)), min_GI = min(GI), max_GI = max(GI))
write.xlsx(sum_stats.set, here("CLIMEX", "Final_outfls", "Analysis", 
                               "Sumstats_Fit_v_Valid_EI_GI.xlsx"), overwrite = TRUE)

sum_stats.cntry <- EI_GI_recs %>% 
  group_by(Country) %>%
  filter(!is.na(EI)) %>%
  summarise(
    mean_EI = round(mean(EI)), min_EI = min(EI), max_EI = max(EI),
    mean_GI = round(mean(GI)), min_GI = min(GI), max_GI = max(GI))
write.xlsx(sum_stats.cntry, here("CLIMEX", "Final_outfls", "Analysis", 
                                 "Sumstats_country_EI_GI.xlsx"), overwrite = TRUE)

# Tally how many localities have EI less than 10
tally_EILt10 <- EI_GI_recs %>% 
  group_by(Country) %>%
  filter(EI < 10) %>%
  count(., name = "n_Lt10") 
tallyEI0 <- EI_GI_recs %>% 
  group_by(Country) %>%
  filter(EI == 0) %>%
  count(., name = "n_0") 
tally_both <- left_join(tally_EILt10, tallyEI0)
write.xlsx(tally_both, here("CLIMEX", "Final_outfls", "Analysis", 
                            "N_locs_EI_Lt10.xlsx"), overwrite = TRUE)

## Validate correlative ensemble model predictions of presence ----
recs_val.corr <- st_transform(recs_val, CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
                 +datum=WGS84 +units=m +no_defs"))
Pres_val_corr <- data.frame("Presence" = raster::extract(world_ens, recs_val.corr)) %>%
  mutate("Continent" = recs_val.corr$Continent, 
         "Lat" = recs_val.corr$Latitude, "Long" = recs_val.corr$Longitude,
         "Site" = recs_val.corr$Site, "Country" = recs_val.corr$Country,
         "State" = recs_val.corr$Region) 
write.xlsx(sum_stats_corr.cntry, 
           here("ENMTML", "Outfiles", outdir, "Pt_validation_allData.xlsx"), overwrite = TRUE)

sum_stats_corr.cntry <- Pres_val_corr %>% 
  group_by(Continent) %>%
  filter(!(is.na(Presence))) %>%
  count(Presence, name = "count") %>%
  mutate(Perc = round(ifelse(Continent == "North America", count/153 * 100, count/6 * 100), 1))
write.xlsx(sum_stats_corr.cntry, 
           here("ENMTML", "Outfiles", outdir, "Pt_validation_summary.xlsx"), overwrite = TRUE)


  
#rm(list = ls())