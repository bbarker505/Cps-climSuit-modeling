## Script to model outputs from Calonectria pseudonaviculata occurrence records
## for North America as a form of model validation

# Libraries
library(raster)
library(sf)
library(tidyverse)
library(tigris)
library(here)
library(openxlsx)

# Create folder for analysis results if it doesn't already exist
if (!file.exists(here("CLIMEX", "Final_outfls", "Analysis"))) {
  dir.create(here("CLIMEX", "Final_outfls", "Analysis"))
}

# CLIMEX rasters (also extract growth index values [GI])
EI_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI_World.tif"))
GI_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "GI_World.tif"))

# Predictions of presence for correlative models
outdir <- "run_PCA_4algs_kfold_prev1_03-29-2022"
val_dir <- paste0(outdir, "/Validation")
world_pres <- raster(here("ENMTML", "Outfiles", outdir, "Projection", "World", 
                          "Ensemble", "PCA", "calonectria_pseudonaviculata.tif"))
world_pres[world_pres >= 0.3] <- 1
world_pres[world_pres < 0.3] <- 0

# Occurrence records for North American and New Zealand 
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2022_noORcoords.xlsx"))
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
Pres_val_corr <- data.frame("Presence" = raster::extract(world_pres, recs_val)) %>%
  mutate("Continent" = recs_val$Continent, 
         "Lat" = recs_val$Latitude, "Long" = recs_val$Longitude,
         "Site" = recs_val$Site, "Country" = recs_val$Country,
         "State" = recs_val$Region) 
write.xlsx(Pres_val_corr, 
           here("ENMTML", "Outfiles", val_dir, "Pt_validation_allData.xlsx"), overwrite = TRUE)

sum_stats_corr.cntry <- Pres_val_corr %>% 
  group_by(Continent) %>%
  filter(!(is.na(Presence))) %>%
  count(Presence, name = "count") %>%
  mutate(Perc = round(ifelse(Continent == "North America", count/153 * 100, count/6 * 100), 1))
write.xlsx(sum_stats_corr.cntry, 
           here("ENMTML", "Outfiles", val_dir, "Pt_validation_summary.xlsx"), overwrite = TRUE)

## Check that predictions are consistent for city- and county-level records for CONUS
# Convert EI rast to binary form
EI_rast[EI_rast < 10] <- 0
EI_rast[EI_rast >= 10] <- 1

# Spatial info
county_sf <- tigris::counties(year = 2018, cb = TRUE) 
city_sf <- tigris::core_based_statistical_areas(year = 2018, cb = TRUE) 
recs_conus <- filter(recs_val, Country == "United States") %>%
  st_transform(., st_crs(county_sf))

# Intersect point data with county and city information
# Result is point object with all columns from sf objects
county_recs <- filter(recs_conus, grepl("County", Site))
county_int <- st_intersection(county_sf, county_recs)
county_sf2 <- st_join(county_sf, county_int, left = FALSE)
county_sp <- as(county_sf2, "Spatial")

city_recs <- recs_conus %>%
  filter(!grepl("County", Site)) %>%
  filter(!(Type == "GBIF"))
city_int <- st_intersection(county_sf, city_recs)
city_sf2 <- st_join(city_sf, city_int, left = FALSE)
city_sp <- as(city_sf2, "Spatial")

# Function to extract predictions
RasterExtr <- function(rast, feat_sp, feat_df) {
  extr <- raster::extract(rast, feat_sp, na.rm = TRUE)
  extr_dfs <- lapply(extr, function(x) { 
    df <- data.frame(x) %>% 
      distinct(x)
    if(nrow(df) == 2) {
      df <- data.frame(x = "both")
    }
    return(df)
  })
  extr_dfs2 <- cbind("pres-abs" = do.call("rbind", extr_dfs), 
                      select(feat_df, Region, Site))
}

# Extract predictions for CLIMEX model
# The 0 values for most records are incorrect - the "extract" raster function 
# is converting NA values to 0 and I can't figure out how to change it.
clmx_county <- RasterExtr(EI_rast, county_sp, county_sf2) %>% rename("clmx" = "x")
clmx_city <- RasterExtr(EI_rast, city_sp, city_sf2) %>% rename("clmx" = "x")
ens_county <- RasterExtr(world_pres, county_sp, county_sf2) %>% rename("ens" = "x")
ens_city <- RasterExtr(world_pres, city_sp, city_sf2) %>% rename("ens" = "x")

# Join different model results
both_county <- left_join(clmx_county, ens_county, by = c("Region", "Site", "geometry"))
both_city <- left_join(clmx_city, ens_city, by = c("Region", "Site", "geometry"))

# Remove erroneous results for CLIMEX
mixed_pred <- bind_rows(both_county, both_city) %>%
  filter(clmx == "both" | ens == "both") %>%
  filter(!(Region %in% c("Connecticut", "New Jersey", "New York"))) %>%
  select(clmx, ens, Region, Site)

# Save results
write.xlsx(mixed_pred, 
           here("ENMTML", "Outfiles", val_dir, "CLMX_ens_poly_extract_mixed.xlsx"), overwrite = TRUE)

#rm(list = ls())