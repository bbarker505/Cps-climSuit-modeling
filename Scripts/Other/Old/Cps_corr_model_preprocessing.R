## Script to import and format climate data used for correlative climate 
## suitaiblity models for Calonectria pseudonaviculata

# Import and format climate data for boxwood blight correlative models
library(raster)
library(tidyverse)
library(here)
library(gtools)

# Format occurrence records for ENMTML ----

# Subsampled records
recs <- read.csv(here("Records", "Subsampled", "Cps_locations_noDups_rand80p_10min.csv")) %>%
  mutate(species = "calonectria_pseudonaviculata")

# Keep and export records for Europe, western Asia, and North America
areas <- list(c("Europe", "Asia"), c("North America"), 
              c("Europe", "Asia", "North America"))
nams <- c("eur", "conus", "all")

lapply(seq_along(areas), function(i) {
  recs %>% 
    filter(Continent %in% areas[[i]]) %>%
    dplyr::select(species, "longitude" = Longitude, "latitude" = Latitude) %>%
    write.table(., here("ENMTML", "locations", 
                        paste0("Cps_", nams[i], "_sites_", Sys.Date(),".txt")), 
                sep = "\t", quote = FALSE, row.names = FALSE) 
})

# Create masks to define training background ----
# Save polygon of study area for ENMTML
prj <- c("+init=epsg:4326 +proj=longlat")

# Create a mask (bounding box of areas)
world <- raster("C:/Users/barkebri/Documents/GIS/Data/Worldclim_v2/10min/ascii/bio1.asc")
values(world) <- NA

eur <- crop(world, extent(c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)))
writeRaster(eur, here("ENMTML", "masks", "BB_Eur_bbox_10min.asc"), 
            format = "ascii", overwrite = TRUE)

conus <- crop(world, extent(c(xmin = -170, xmax = -51, ymin= 25.5, ymax = 52)))
writeRaster(conus, here("ENMTML", "masks", "BB_CONUS_bbox_10min.asc"), 
            format = "ascii", overwrite = TRUE)

eur_conus <- crop(world, extent(c(xmin = -170, xmax = 61.9, ymin= 25.5, ymax = 71.3)))
writeRaster(eur_conus, here("ENMTML", "masks", "BB_Eur_CONUS_bbox_10min.asc"), 
            format = "ascii", overwrite = TRUE)

# eur_bbox <- st_bbox(c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3), crs = prj)
# world_rast <- read_stars(
#   "C:/Users/barkebri/Documents/Species/template_world.tif", crs = prj)
# values(world_rast) <- NA
# eur_pol <- st_as_sf(st_crop(world_rast, eur_bbox))
# st_write(eur_pol, here("ENMTML", "masks", "BB_Eur_poly.shp"), append = FALSE)
# 
# conus_bbox <- st_bbox(c(xmin = -170,  xmax = -51, ymin= 24.6, ymax = 71.8), crs = prj)
# conus_pol <- st_as_sf(st_crop(world_rast, conus_bbox))
# st_write(eur_pol, here("ENMTML", "masks", "BB_CONUS_poly.shp"), append = FALSE)

# Crop climate data ----
base_path <- "C:/Users/barkebri/Documents/GIS/Data/CliMond"
fl_pth <- paste0(base_path, "/Climond_world/")

# "mixedsort" from the gtools library orders list according to number in name
s <- stack(
  mixedsort(list.files(paste0(fl_pth, "all_vars"),
    pattern = ".txt$",
    full.names = TRUE))
)

s2 <- dropLayer(s, 20:27) # Drop radiation layers
names(s2) <- paste0("bio", c(1:19, 28:35))
for (i in seq_along(1:nlayers(s2))) {
  nam <- names(s2)[[i]]
  writeRaster(s2[[i]], 
              #filename = paste0(pth, "/asc/World/bio", i, ".asc"),
              filename = paste0(fl_pth, "no_radiation_vars/", nam, ".asc"),
              format = "ascii", overwrite = TRUE)
}

# Crop by region
s_eur <- crop(s2, eur)
s_conus <- crop(s2, conus)
s_eur_conus <- crop(s2, eur_conus)
#s_eur <- crop(s, as(eur_pol, "Spatial"))
#s_conus <- crop(s, as(conus_pol, "Spatial"))
#s2 <- st_as_stars(s)
#st_crs(s2) <- prj
#s3 <-  st_crop(s2, eur_bbox)
#s4 <- as(s3, "Raster") # Not sure how to write multi-layer stars object

# Write to BB climate data folder
fl_pth2 <- paste0(base_path, "/Climond_BOXB/")
lst <- list(s_eur, s_conus, s_eur_conus)
type <- c("Eurasia", "CONUS", "Eurasia_CONUS")

for (j in seq_along(lst)) {
  s_cropped <- lst[[j]]
  fldr <- type[j]
  dir.create(paste0(pth, "/", fldr))
  for (i in seq_along(1:nlayers(s_cropped))) {
    nam <- names(s_cropped)[[i]]
    writeRaster(s_cropped[[i]], 
                #filename = paste0(pth, "/asc/World/bio", i, ".asc"),
                filename = paste0(fl_pth2, fldr, "/", nam, ".asc"),
                format = "ascii", overwrite = TRUE)
  }
}
