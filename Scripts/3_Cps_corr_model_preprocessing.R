## Script to subsample Calonectria pseudonaviculata occurrence records to 
## avoid effects of biased sampling in correlative niche models.
library(sf)
library(gtools)
library(here)
library(ggplot2)
library(dplyr)
library(sp)
library(spatialEco)
library(dismo)
library(openxlsx)

# Load functions
source(here("scripts", "Cps_model_functions.R"))

# Create "Subsampled" subdirectory if it doesn't already exist
if (!file.exists(here("Records", "Subsampled"))) {
  dir.create(here("Records", "Subsampled"))
}

# Import and format data ----
pts <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
output_dir <- here("Records")

# Apply function to the points
pts <- pts %>%
  mutate(utm = long2UTM(Longitude)) %>%
  mutate(ID_orig = as.numeric(rownames(.)))

# Split out points by UTM zone. Each group will be analyzed in next step.
pts_df_grps <- pts %>% 
  group_by(utm) %>%
  group_split(.)

# CRS for records
wgs <- "+proj=longlat +datum=WGS84"

# Remove points in same 10' grid cell ("dismo" package)
# IMPORTANT: Longitude must come before Latitude in the "gridsample" function
r <- raster(here("CliMond_raw", "CM10_1975H_Bio01_V1.2.txt"))
crs(r) <- wgs
pts_1x.coords <- gridSample(dplyr::select(pts, Longitude, Latitude), r, n = 1) 
pts_1x <- left_join(pts_1x.coords, pts, by = c("Latitude", "Longitude"))
write.csv(pts_1x, row.names = F, 
          here("Records", "Subsampled", "Cps_locations_noDups_10min.csv"))

# The point process random subsample ('pp.subsample' function of the 'spatEco'
# package) generates a random subsample based on density estimates of 
# observations. This will produce a slightly different occurrence data set than
# the one presented in the manuscript because of the random process. 

# Extract coordinate data, convert to a spatial points dataframe
regions <- list(c("Europe", "Asia"), c("North America"), c("New Zealand"))
region_name <- c("Eurasia_random", "NA_random", "NZ_random")

for (i in 1:length(regions)) {
  print(i)
  # Filter by region and convert to spdf
  pts_1x_sub<- pts_1x %>%
    dplyr::filter(Continent %in% regions[[i]])
  xy <- dplyr::select(pts_1x_sub, Longitude, Latitude) # Coordinates
  spdf <- SpatialPointsDataFrame(
    coords = xy, 
    data = dplyr::select(pts_1x_sub, -Longitude, -Latitude),
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  # Percentage of points to keep
  n <- round(length(spdf) * 0.8, digits=0)
  
  # Subsample data, adding the row number back to get the original ID
  # and then filter the original input
  spdf.rand <- pp.subsample(spdf, n = n, window='hull') 
  crs(spdf.rand) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  spdf.rand_jn <- st_join(st_as_sf(spdf.rand), st_as_sf(spdf))
  pts_1x_rand <- spdf.rand_jn %>%
    mutate(Latitude = st_coordinates(.)[,2],
           Longitude = st_coordinates(.)[,1]) %>%
    as.data.frame() %>%
    dplyr::select(-geometry, -KDE, -ID_orig, -utm) %>%
    dplyr::select(Latitude, Longitude, everything())
    
  assign(region_name[i], pts_1x_rand)
}

# Combine results for all regions and save
pts_1x_rand.all <- rbind(Eurasia_random, NA_random, NZ_random) %>%
  arrange(Continent)
  
# Write results
write.csv(pts_1x_rand.all,
          here("Records", "Subsampled", "Cps_locations_noDups_rand80p_10min.csv"),
          row.names = FALSE)

# Save sets to go in the ENMTML locations folder
pts_1x_rand.all_noNZ <- pts_1x_rand.all %>% 
  filter(Continent %in% c("Asia", "Europe", "North America")) # No NZ
pts_1x_rand.all_noNZ_noUS <- pts_1x_rand.all %>% # No NZ or North America
  filter(Continent %in% c("Asia", "Europe"))
                           
WriteTable(pts_1x_rand.all, "Cps_all_sites") 
WriteTable(pts_1x_rand.all_noNZ, "Cps_noNZ_sites")
WriteTable(pts_1x_rand.all_noNZ_noUS, "Cps_EurAsia_only_sites")

# Format input climate data ----

# Import all CliMond data (global scale)
# "mixedsort" from the gtools library orders list according to number in name
s <- stack(
  mixedsort(list.files(here("CliMond_raw"),
                       pattern = ".txt$",
                       full.names = TRUE))
)

# Drop radiation variables and crop to calibration area (Eurasia/CONUS)
# Also produce layers for Eurasia only as the calibration area
s_sub <- dropLayer(s, 20:27) 
names(s_sub) <- paste0("bio", c(1:19, 28:35))
s_sub_crp <- crop(s_sub, 
                  extent(c(xmin = -170, xmax = 61.9, ymin= 25.5, ymax = 71.3)))
s_sub_crp2 <- crop(s_sub, 
                   extent(c(xmin = -11.5, xmax = 61.9, ymin = 25.5, ymax = 71.3)))

# Export variables to ENMTML folders 

# Save cropped bioclimatic variables (proj + calibration area)
infls <- list(s_sub, s_sub_crp, s_sub_crp2)
dirs <- c("Projection", "Predictors", "Predictors")
flds <- c("World", "EUR_NA", "EUR_only")

for (i in seq_along(infls)) {
  
  s <- infls[[i]]
  
  for (j in seq_along(1:nlayers(s))) {
    nam <- names(s)[[j]]
    writeRaster(s[[j]], 
      filename = here("ENMTML", "All_vars", dirs[i], flds[i], paste0(nam, ".asc")),
      format = "ascii", overwrite = TRUE)
  }
  
}

# Subset of minimally correlated bioclimatic variables:
# bio5, bio6, bio7, bio15, bio33
s_sub2 <- raster::subset(s_sub, grep("bio5|bio6|bio7|bio15|bio33", 
                                     names(s_sub), value = TRUE))
s_sub_crp3 <- raster::subset(s_sub_crp, grep("bio5|bio6|bio7|bio15|bio33", 
                                        names(s_sub_crp), value = TRUE))
  
for (i in seq_along(1:nlayers(s_sub2))) {
  nam <- names(s_sub2)[[i]]
  writeRaster(s_sub2[[i]], 
    filename = here("ENMTML", "Sub_vars", "Projection", "World", paste0(nam, ".asc")),
    format = "ascii", overwrite = TRUE)
}

# Training background (calibration area)
for (i in seq_along(1:nlayers(s_sub_crp3))) {
  nam <- names(s_sub_crp3)[[i]]
  writeRaster(s_sub_crp3[[i]], 
    filename = here("ENMTML", "Sub_vars", "Predictors", paste0(nam, ".asc")),
    format = "ascii", overwrite = TRUE)
}

rm(list = ls())