# Script for formatting Cps records from Georgia (provided by I. Matsah)

# Libraries
pkgs <- c("sp", "GADMTools", "rgdal", "sf", "here", "openxlsx", "spatialEco",
          "spdplyr", "tidyverse")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Get records and make point file
recs <- read.xlsx(here("Records", "boxwood_locations_updated_Apr2021.xlsx")) 
recs <- recs %>% mutate(across(1:2, as.numeric)) %>%
  mutate(across(1:2, round, 2))
xy <- recs[,c(2,1)]
spdf <- SpatialPointsDataFrame(coords = xy, data = recs,
    proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Shapefile with admin boundaries (province/subregion/state) to spatial df
fl <- "C:/Users/barkebri/Documents/GIS/Data/GADM/gadm36_2.shp"
adm2 <- as_Spatial(sf::st_read(fl)) #SpatialPolygonsDataFrame
adm2 <- adm2 %>% 
  select(NAME_0, NAME_1)

# Join records (spdf) to admin data (adm2)
pts.poly <- point.in.poly(spdf, adm2)
head(pts.poly@data)

pts.poly_df <- data.frame(pts.poly)

# Save results!
write.xlsx(pts.poly_df, here("Records", "boxwood_locations_updated_Apr2021.xlsx"),
           row.names = FALSE)

# Convert XY of Georgia data (I. Matsiakh) to lat-long
pts <- read.xlsx(here("Records", "Other_sources", "Georgia", 
                      "BB_coords_Georgia_formatted.xlsx")) 
pts_38N <- pts %>% filter(Zone == "38") %>%
  st_as_sf(coords = c("X", "Y"), crs = "+proj=utm +zone=38 +datum=WGS84") %>%
  st_transform(cord.utm, crs = "+proj=longlat +datum=WGS84")

dists <- st_distance(pts_38N)/1000

pts_37N <- pts %>% filter(Zone == "37") %>%
  st_as_sf(coords = c("X", "Y"), crs = "+proj=utm +zone=37 +datum=WGS84") %>%
  st_transform(cord.utm, crs = "+proj=longlat +datum=WGS84")
  
all_out <- data.frame(bind_rows(pts_38N, pts_37N)) %>%
  mutate(lat = unlist(map(geometry,1)),
         long = unlist(map(geometry,2))) %>%
  select(-geometry) %>%
  mutate(lat = round(lat, 2), long = round(long, 2)) %>%
  left_join(select(pts, Points, X, Y), by = "Points")

write.xlsx(all_out, here("Records", "Other_sources", "Georgia", 
                         "BB_coords_Georgia_formatted.xlsx"))
