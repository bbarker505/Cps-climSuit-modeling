## Script to create Excel file with formatted occurrence records for Calonectria 
## pseudonaviculata, provided as a supporting data file for manuscript reporting
## climate suitability modeling study for the species.
library(tidyverse)
library(here)
library(openxlsx)
library(biogeo)

# Occurrence records - all records and final subset used for correlative models
corr_out_dir <- "run_PCA_08-31-2021"
all_recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx"))
sub_recs <- read.table(
  here("ENMTML", "Outfiles", out_dir, "Occurrences_Cleaned.txt"), 
  header = TRUE) %>%
  rename("Longitude" = "x", "Latitude" = "y") %>%
  mutate("Corr_mod" = 1)

# Convert decimal degrees (DD) to degrees decimal minutes (DDM)
lat_ddm <- dd2dmslat(all_recs$Latitude) %>%
  mutate(min_dd = ymin + ysec/60) %>%
  mutate(lat_ddm = paste0(ydeg, "°", min_dd, "'", NS))

lon_ddm <- dd2dmslong(all_recs$Longitude) %>%
  mutate(min_dd = xmin + xsec/60) %>%
  mutate(lon_ddm = paste0(xdeg, "°", min_dd, "'", EW))

# Join dataset and indicate which were used for correlative models
# Remove accents and other formatting from region names
# Convert DD (degree decimals) to DDM (decimal degree minutes)
all_recs2 <- left_join(all_recs, sub_recs, by = c("Longitude", "Latitude")) %>%
  mutate(Corr_mod = replace_na(Corr_mod, 0),
         Region = iconv(Region, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         Latitude = lat_ddm$lat_ddm, 
         Longitude = lon_ddm$lon_ddm) %>%
  select(Continent, Country, Region, Site, Latitude, Longitude, Corr_mod, Source) 

write.xlsx(all_recs2, here("Records", "Final_MS_table", "Cps_locations_Final_SuppInfo_Sep2021.xlsx"),
           row.names = FALSE, overwrite = TRUE)
