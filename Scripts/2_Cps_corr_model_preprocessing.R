## Script to subsample Calonectria pseudonaviculata occurrence records to 
## avoid effects of biased sampling in correlative niche models.
library(sf)
library(here)
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
pts <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) %>%
  rename("x" =  "Longitude", "y" = "Latitude")
output_dir <- here("Records")

# CRS for records
wgs <- "+proj=longlat +datum=WGS84"

# Remove points in same 10' grid cell ("dismo" package)
# IMPORTANT: Longitude must come before Latitude in the "gridsample" function
r <- raster(here("CliMond_raw", "CM10_1975H_Bio01_V1.2.txt"))
crs(r) <- wgs
pts_1x.coords <- gridSample(dplyr::select(pts, x, y), r, n = 1) 
pts_1x <- left_join(pts_1x.coords, pts, by = c("x", "y"))
write.csv(pts_1x, row.names = FALSE, 
          here("Records", "Subsampled", "Cps_locations_noDups_10min.csv"))

# The point process random subsample ('pp.subsample' function of the 'spatEco'
# package) generates a random subsample based on density estimates of 
# observations. This will produce a slightly different occurrence data set than
# the one presented in the manuscript because of the random process. 

# Extract coordinate data, convert to a spatial points data frame
regions <- list(c("Europe", "Asia"), c("North America"), c("New Zealand"))
region_name <- c("Eurasia_random", "NA_random", "NZ_random")

for (i in 1:length(regions)) {
  print(i)
  # Filter by region and convert to spdf
  pts_1x_sub <- pts_1x %>%
    dplyr::filter(Continent %in% regions[[i]])
  xy <- dplyr::select(pts_1x_sub, x, y) # Coordinates
  spdf <- SpatialPointsDataFrame(
    coords = xy, 
    data = dplyr::select(pts_1x_sub, -x, -y),
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  # Percentage of points to keep
  n <- round(length(spdf) * 0.7, digits=0)
  
  # Subsample data, adding the row number back to get the original ID
  # and then filter the original input
  spdf.rand <- pp.subsample(spdf, n = n, window='hull') 
  crs(spdf.rand) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  spdf.rand_jn <- st_join(st_as_sf(spdf.rand), st_as_sf(spdf))
  pts_1x_rand <- spdf.rand_jn %>%
    mutate(y = st_coordinates(.)[,2],
           x = st_coordinates(.)[,1]) %>%
    as.data.frame() %>%
    dplyr::select(-geometry, -KDE) %>%
    dplyr::select(x,y, everything())
    
  assign(region_name[i], pts_1x_rand)
}

png(paste(here("ENMTML", "Locations", "ppsubsample_maps", "ppsubsample_0.7_hull3.png")))
plot(dplyr::select(filter(pts_1x, Continent %in% c("Europe", "Asia")), x, y), pch=20, col='black', cex=1)
points(dplyr::select(Eurasia_random, x, y), pch=20, col='red', cex=1, add=TRUE)
box()
legend('topright', legend=c('Original sample', 'Subsample'), 
       col=c('black','red'),pch=c(20,20),bg="white")
dev.off()

# Combine results for all regions and save
pts_1x_rand.all <- rbind(Eurasia_random, NA_random, NZ_random) %>%
  arrange(Continent) %>%
  rename("longitude" = x, "latitude" = y)
  
# Write results
write.csv(pts_1x_rand.all,
          here("Records", "Subsampled", "Cps_locations_noDups_rand70p_10min.csv"),
          row.names = FALSE)

# Save sets to go in the ENMTML locations folder
pts_1x_rand.all_noNZ <- pts_1x_rand.all %>% 
  filter(Continent %in% c("Asia", "Europe", "North America")) # No NZ
pts_1x_rand.all_noNZ_noUS <- pts_1x_rand.all %>% # No NZ or North America
  filter(Continent %in% c("Asia", "Europe"))

# Keep track of versions w/ date
WriteTable(pts_1x_rand.all, "Cps_all_sites")
WriteTable(pts_1x_rand.all_noNZ, "Cps_noNZ_sites")
WriteTable(pts_1x_rand.all_noNZ_noUS, "Cps_EurAsia_only_sites")

# Format input climate data ----

# Import all CliMond data (global scale)
# "mixedsort" from the gtools library orders list according to number in name
s_all <- stack(
  mixedsort(list.files(here("CliMond_raw"),
                       pattern = ".txt$",
                       full.names = TRUE))
)

# Drop radiation variables and crop
s_sub <- dropLayer(s_all, 20:27) 
names(s_sub) <- paste0("bio", c(1:19, 28:35))
# Europe and North America
s_sub_EurNA <- crop(s_sub, extent(c(xmin = -170, xmax = 61.9, ymin= 25.5, ymax = 71.3)))
# Europe only
s_sub_Eur <- crop(s_sub, extent(c(xmin = -11.5, xmax = 57, ymin = 35.6, ymax = 71.3)))

# Export variables to ENMTML folders 

# Save cropped bioclimatic variables 
infls <- list(s_sub, s_sub_EurNA, s_sub_Eur)
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
s_5vars <- raster::subset(s_sub, grep("bio5|bio6|bio7|bio15|bio33", 
                                     names(s_sub), value = TRUE))
s_5vars_EurNA <- raster::subset(s_sub_EurNA, grep("bio5|bio6|bio7|bio15|bio33", 
                                        names(s_sub_EurNA), value = TRUE))
s_5_vars_Eur <- raster::subset(s_sub_Eur, grep("bio5|bio6|bio7|bio15|bio33", 
                                                 names(s_sub_Eur), value = TRUE))

# Save cropped bioclimatic variables  
infls <- list(s_5vars, s_5vars_EurNA, s_5_vars_Eur)
dirs <- c("Projection", "Predictors", "Predictors")
flds <- c("World", "EUR_NA", "EUR_only")

for (i in seq_along(infls)) {
  
  s <- infls[[i]]
  
  for (j in seq_along(1:nlayers(s))) {
    nam <- names(s)[[j]]
    writeRaster(s[[j]], 
                filename = here("ENMTML", "Sub_vars", dirs[i], flds[i], paste0(nam, ".asc")),
                format = "ascii", overwrite = TRUE)
  }
  
}

rm(list = ls())