## Script to process CLIMEX output data and produce plots depicting 
## climate suitability and climate stress accumulation for Calonectria 
## pseudonaviculata in Europe, western Asia, and North America.

# Processes CLIMEX output data - produces rasters and figures

# Libraries
pkgs <- c("sp", "rgdal", "raster", "tidyverse", "maptools","RColorBrewer", 
          "knitr", "cowplot","sf","spData", "here", "openxlsx", "ggalt", 
          "rnaturalearth")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("Scripts", "Cps_model_functions.R"))

## CLIMEX model ---

## Get CLIMEX outputs, assign extents, rasterize outputs, and export rasters
CLMX_noIrrig_csv <- read.csv(here("CLIMEX", "CSVs",
                          "BB_run9_SMDS0.2_CS-9_0.005_DV09.csv"))
CLMX_irrig_csv <- read.csv(here("CLIMEX", "CSVs",  
                          "BB_run9_SMDS0.2_CS-9_0.005_DV09_2.5mmIrrig.csv")) %>%
  rename(c("DS.ir" = "DS", "GI.ir" = "GI", "EI.ir" = "EI")) %>%
  dplyr::select("GI.ir", "EI.ir")

#  Combine non-irrig and irrig outputs, keeping only columns that are different
# (Different columns are DS, GI, and EI). Define extent and res of full dataset.
CLMX_csv <- cbind(CLMX_noIrrig_csv, CLMX_irrig_csv)

ext <- raster(xmn = min(CLMX_csv$Longitude), xmx = max(CLMX_csv$Longitude), 
              ymn = min(CLMX_csv$Latitude, ymx = max(CLMX_csv$Latitude)))
res(ext) <- 0.18

# Records used for CLIMEX model fitting and validation
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
coordinates(recs) <- ~Longitude + Latitude
crs(recs) <- CRS("+proj=longlat +datum=WGS84")
recs_sf <- st_as_sf(recs)

# Export rasters for world, and create data frame for each 
out_names <- c("GI", "EI.ir", "EI", "CS", "HS", "DS")

for (nam in out_names) {
  result <- CLMX_csv %>% dplyr::select(Longitude, Latitude, nam)
  rast <- rasterize(result[, c('Longitude', 'Latitude')], ext, result[, 3],
                    crs="+proj=longlat +datum=WGS84")

writeRaster(rast, file = here("CLIMEX", "TIF_files", "run9",
                              paste0(nam, "_World.tif")),
           format= "GTiff", overwrite=TRUE)
  
  # Save rasters for plotting below
  assign(paste0(nam, "_rast"), rast)

}

rm(rast)

