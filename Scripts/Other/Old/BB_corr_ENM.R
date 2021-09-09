# Libraries
pkgs <- c("sp", "rgdal", "raster", "lubridate", "dplyr", "stringr", "ggplot2",
          "ggthemes","maptools","RColorBrewer","cowplot","sf","spData","tmap",
          "here", "openxlsx", "rnaturalearth", "gtools", "ENMTML", "stars")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Import and format data ----
#recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) %>%
#  mutate(species = "calonectria_pseudonaviculata") 
recs <- read.csv(here("Records", "Cps_locations_noDups_2.5m.csv")) %>%
  mutate(species = "calonectria_pseudonaviculata")

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
  
# Creating masks to define training background ----
# Save polygon of study area for ENMTML
prj <- c("+init=epsg:4326 +proj=longlat")

# Create a mask (bounding box of areas)
world <- raster("C:/Users/barkebri/Documents/GIS/Data/Worldclim_v2/2.5min/asc/bio1.asc")
values(world) <- NA

eur <- crop(world, extent(c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)))
writeRaster(eur, here("ENMTML", "masks", "BB_Eur_bbox.asc"), 
            format = "ascii", overwrite = TRUE)

conus <- crop(world, extent(c(xmin = -170, xmax = -51, ymin= 25.5, ymax = 52)))
writeRaster(conus, here("ENMTML", "masks", "BB_CONUS_bbox.asc"), 
            format = "ascii", overwrite = TRUE)

eur_conus <- crop(world, extent(c(xmin = -170, xmax = 61.9, ymin= 25.5, ymax = 71.3)))
writeRaster(eur_conus, here("ENMTML", "masks", "BB_Eur_CONUS_bbox.asc"), 
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

# "mixedsort" from the gtools library orders list according to number in name
s <- stack(
  mixedsort(list.files(
    "C:/Users/barkebri/Documents/GIS/Data/Worldclim_v2/2.5min/asc/",
    pattern = ".asc$",
    full.names = TRUE))
)

s_eur <- crop(s, eur)
s_conus <- crop(s, conus)
s_eur_conus <- crop(s, eur_conus)
#s_eur <- crop(s, as(eur_pol, "Spatial"))
#s_conus <- crop(s, as(conus_pol, "Spatial"))
#s2 <- st_as_stars(s)
#st_crs(s2) <- prj
#s3 <-  st_crop(s2, eur_bbox)
#s4 <- as(s3, "Raster") # Not sure how to write multi-layer stars object

# Write to BB climate data folder
pth <- "C:/Users/barkebri/Documents/GIS/Data/Worldclim_v2/2.5min/BB/"
lst <- list(s_eur, s_conus, s_eur_conus)
type <- c("Eurasia", "CONUS", "Eurasia_CONUS")

for (j in seq_along(lst)) {
  s <- lst[[j]]
  fldr <- type[j]
  for (i in seq_along(1:19)) {
    writeRaster(s[[i]], 
                #filename = paste0(pth, "/asc/World/bio", i, ".asc"),
                filename = paste0(pth, type[j], "/bio", i, ".asc"),
                format = "ascii", overwrite = TRUE)
  }
}

# ENMTML
pred_dir <- "C:/Users/barkebri/Documents/GIS/Data/Worldclim_v2/2.5min/BB/Eurasia_CONUS"
result_dir <- here("ENMTML", "outfiles", "test")
occ_file <- here("ENMTML", "locations", "Cps_all_sites_2021-04-28.txt")
 
#sp_area <- c(method = "MASK", filepath = here("ENMTML", "masks", "BB_Eur_bbox.asc"))
#sp_area <- c(method="BUFFER", type = "1")

if(exists(result_dir)) {
  unlink(result_dir, recursive = TRUE)
}

ENMTML(pred_dir = pred_dir, 
       result_dir = result_dir, 
       occ_file = occ_file,
       sp = "species",
       x = "longitude", 
       y = "latitude", 
       #thin_occ = c(method='MORAN'),
       colin_var = c(method = "PCA"),
       sp_accessible_area = c(method = "BUFFER", type = "2", width = "300"),
       pseudoabs_method = c(method="RND"),
       pres_abs_ratio = 1,
       part = c(method = "BOOT", replicates = "5", proportion = "0.7"),
       algorithm = c("MXD", "BRT", "RDF", "GAM"),
       thr = c(type = c("MAX_TSS")),
       msdm = NULL)

# Plot results

# Get