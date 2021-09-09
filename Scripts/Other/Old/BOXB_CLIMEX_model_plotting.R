# Script for Boxwood Blight CLIMEX analyses
# Processes CLIMEX output data - produces rasters and figures
# Compares those results to DDRP results

# Libraries
pkgs <- c("sp", "rgdal", "raster", "lubridate", "dplyr", "stringr", "ggplot2",
          "ggthemes","maptools","RColorBrewer","cowplot","sf","spData","tmap",
          "here", "mgsub", "openxlsx", "ggalt", "rnaturalearth")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("Rscripts", "BOXB_plotting.R"))

## Get out files, assign extents, rasterize, and export
## CLIMEX model ---
CLMX_noIrrig_csv <- read.csv(here("CLIMEX", "CSVs", 
                          "BB_run8_noHDS_CS-9_0.005_DV09.csv"))
CLMX_irrig_csv <- read.csv(here("CLIMEX", "CSVs", 
                          "BB_run8_noHDS_CS-9_0.005_DV09_2.5mmIrrig.csv")) %>%
  rename(c("DS.ir" = "DS", "GI.ir" = "GI", "EI.ir" = "EI")) %>%
  select("GI.ir", "EI.ir")

#  Combine non-irrig and irrig outputs, keeping only columns that are different
# (Different columns are DS, GI, and EI). Define extent and res of full dataset.
CLMX_csv <- cbind(CLMX_noIrrig_csv, CLMX_irrig_csv)

ext <- raster(xmn = min(CLMX_csv$Longitude), xmx = max(CLMX_csv$Longitude), 
              ymn = min(CLMX_csv$Latitude, ymx = max(CLMX_csv$Latitude)))
res(ext) <- 0.18

# Records used for model fitting and validation
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
coordinates(recs) <- ~Longitude + Latitude
crs(recs) <- CRS("+proj=longlat +datum=WGS84")
recs_sf <- st_as_sf(recs)

# Export raster for world, and create dataframe for each 
out_names <- c("GI", "EI.ir", "EI", "CS", "HS", "DS")
#out_names <- c("GI", "EI", "CS", "HS", "DS")

for (nam in out_names) {
  result <- CLMX_csv %>% dplyr::select(Longitude, Latitude, nam)
  rast <- rasterize(result[, c('Longitude', 'Latitude')], ext, result[, 3],
                    crs="+proj=longlat +datum=WGS84")

 # writeRaster(rast, file = here("CLIMEX", "TIF_files", "run8", 
  #                              paste0(nam, "_World.tif")),
   #           format= "GTiff", overwrite=TRUE)
  
  # Save rasters for plotting below
  assign(paste0(nam, "_rast"), rast)

}

rm(rast)

# Assign a value to 30 for EI and GI values above 30 for plotting purposes
# Keep original raster for validation calculations
EI_rast.o <- EI_rast
EI.ir_rast.o <- EI.ir_rast

GI_rast[GI_rast > 30] <- 30
GI.ir_rast[GI.ir_rast > 30] <- 30
EI_rast[EI_rast > 30] <- 30
EI.ir_rast[EI.ir_rast > 30] <- 30

## Plot EI for world ----
# Could not figure out how to overlay world polygons with CLIMEX data that were
# in gridded or data frame (geom_tile) format! Overlay of projected features 
# works when CLIMEX data are converted to a polygon.
# This is only an issue for projections such as Mercator, Pseudo-Mercator, and
# World Robinson
  
# Convert CLIMEX data to a polygon (sf) feature
# TO DO: is there a way to avoid using rasterize? Couldn't figure it out.
# ei_sfpt <- st_as_sf(ei, coords = c("Longitude", "Latitude"))
# ei_sfpol <- st_cast(ei_sfpt, "POLYGON" )
# ei_r <- rasterize(ei[, c('Longitude','Latitude')], ext, ei[, 1], fun=median)
# names(ei_r) <- "EI"
# ei_spd <- as(ei_r, 'SpatialPolygonsDataFrame')
# ei_sf <- st_as_sf(ei_spd)
# ei_sf <- st_transform(ei_sf, CRS("+init=epsg:4326"))
#ei_sf <- st_transform(ei_sf, CRS("+proj=robin"))

# World Robinson projection
prj_world <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
                 +datum=WGS84 +units=m +no_defs")
ext_world <- c(xmin = -180,  xmax = 180, ymin= -59.47275, ymax = 83.6341)

# Polygon and line for world countries
world_p <- ne_countries(scale = 10, type = "countries", returnclass = "sf") %>%
  filter(!(admin == "Antarctica")) # Multi-polygon
world_p <- st_transform(world_p, prj_world) 
world_l <- st_cast(world_p, "MULTILINESTRING") # Create line feature

# Crop and project CLIMEX results (convert to raster, then back to data frame)
CLMX_pts_world <- Proj_rasts(list(EI_rast), ext_world, prj_world)[[1]] %>%
  mutate(value = na_if(value, 0))

# Plot EI for world. Here and below create two types of EI maps - one in
# continuous scale and the other in categories (bins).

# Colors for categorical scale for EI and GI
ei_cols <- setNames(c("4"="red","3"="orange","2"="yellow2","1"="cyan"),
                    c("31-100","21-30","11-20","1-10"))

# Plot with continuous scale
ei_world.p <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray85", lwd = 0.3) +
  geom_tile(data = CLMX_pts_world, aes(x = x, y = y, fill = value)) +
  # geom_tile(data = CLMX_pts_world, aes(x = x, y = y, fill = value), 
  #           width = 0.2, height = 0.2) + # Specify w and h, or get white lines
  scale_fill_gradient(low="yellow", high = "darkred", na.value = "transparent",
                      name = "EI", breaks=c(1, 10, 20, 30), 
                      labels=c("1", "10", "20", "30+"),
                      guide = guide_colorbar(
                        frame.colour = "black",
                        barheight = 5,
                        frame.linewidth = 2,
                        ticks.colour = "black",
                        ticks.linewidth = 2)) +
  geom_sf(data = world_l, lwd = 0.2, color = "gray10") + 
  theme_func("right") +
  theme(legend.position = c(0.2, 0.3)) 

ggsave(ei_world.p, file = here("CLIMEX", "Figures", "EI_world_DV09_cont.png"), 
       width = 8, height = 4, units = c('in'), dpi=300)


# Plot with categorical scale
CLMX_pts_world2 <- Format_EI_func(CLMX_pts_world)

ei_world.p2 <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray85", lwd = 0.3) +
  geom_tile(data = CLMX_pts_world2, aes(x = x, y = y, fill = value)) +
  scale_fill_manual(values = ei_cols, name = "EI") +
  geom_sf(data = world_l, lwd = 0.2, color = "gray10") + 
  theme_func("right") +
  theme(legend.position = c(0.2, 0.3),
        legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
        legend.background = element_rect(fill = "white")) 

ggsave(ei_world.p2, file = here("CLIMEX", "Figures", "EI_world_DV09_cats.png"), 
       width = 8, height = 4, units = c('in'), dpi=300)

## Plots for Eurasia ----

# Projection and extent
# Lambert Conformal Conic looks best
ext_eur <- c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)
prj_eur <- CRS("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 
               +ellps=intl +units=m +no_defs")
#prj_eur <- CRS("+proj=bonne +lat0=50")
#prj_eur <- CRS("+proj=moll")

# Filter out Eurasia records, and crop world countries features
# The RegionCrop function does this
recs_eur_sf <- recs_sf %>% 
  filter(Continent %in% c("Europe", "Asia")) %>%
  st_transform(., prj_eur)

# Country features
eur_cntry_feats <- RegionCrop(type = "countries", ext = ext_eur, prj = prj_eur)
eur_cntry_p <- eur_cntry_feats[[1]] # Polygon feature
eur_cntry_l <- eur_cntry_feats[[2]] # Line features
                              
# Crop and project CLIMEX results (convert to raster, then back to data frame)
rast_lst <- list(GI_rast, CS_rast, HS_rast, DS_rast, EI_rast, EI.ir_rast)
CLMX_pts_eur <- Proj_rasts(rast_lst, ext_eur, prj_eur)

# Plot the results
vars <- c("GI", "CS", "HS", "DS", "EI", "EI.ir")

# Continuous scale fill colors
low_cols <- c("lightpink", "lightblue", "mistyrose1", "khaki", "yellow", "yellow")
high_cols <- c("mediumpurple4", "blue2", "red3", "darkorange4", "red2", "red2")

# Categorical scale fill colors
gi_cols <- colorRampPalette(c("mediumpurple4", "lightpink"))(4)
cs_cols<- colorRampPalette(c("lightblue", "blue2"))(4)
hs_cols <- colorRampPalette(c( "mistyrose1", "red3"))(4)
ds_cols <- colorRampPalette(c("khaki", "darkorange4"))(4)
ei_cols <- c("cyan", "yellow2", "orange", "red")
cols_binned <- list(gi_cols, cs_cols, hs_cols, ds_cols, ei_cols, ei_cols)

# Plots for Eurasia
for (i in seq_along(CLMX_pts_eur)) {
  var <- vars[i]
  df <- CLMX_pts_eur[[i]]
  #df <- df %>% mutate(value = na_if(value, 0))
  df <- df %>% filter(!(value == 0))
  
  ## Format for plotting data in categories
  func_outs <- Bin_EI(var, df)
  brks <- func_outs[[1]]
  labs <- func_outs[[2]]
  df2 <- func_outs[[3]]
  
  # Order factors, set color scale
  df2$value_bin <- factor(df2$value_bin, 
                          levels = unique(df2$value_bin[order(df2$value)])) 
  cols2 <- setNames(cols_binned[[i]], levels(df2$value_bin))
  
  # Change legend title for EI irrigated
  if (var == "EI.ir") {
    lgd_titl <- "EI"
  } else {
    lgd_titl <- var
  }
  
  # Plot with continuous scale
  eur.p <- ggplot() + 
    geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray85", lwd = 0.3) +
    geom_tile(data = df, aes(x = x, y = y, fill = value)) +
    #geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = value), 
    #          width = 0.2, height = 0.2) + # Specify w and h, or get white lines
    scale_fill_gradient(low=low_cols[i], high = high_cols[i], 
                        na.value = "transparent",
                        name = lgd_titl, breaks = brks, labels = labs,
                        guide = guide_colorbar(
                          frame.colour = "black",
                          barheight = 3,
                          frame.linewidth = 1.5,
                          ticks.colour = "black",
                          ticks.linewidth = 2)) +
    geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") + 
    geom_sf(data=recs_eur_sf, shape=21, 
            size=1.5, fill= "black", color = "white") +
    theme_func("right") +
    theme(legend.position = c(0.15, 0.78),
          legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
          legend.background = element_rect(fill = "white")) 
  assign(paste0(var, "_eur.p"), eur.p)
  
  # Plot with categorical scale (binned data)
  eur.p2 <- ggplot() + 
      geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray85", lwd = 0.3) +
      geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols2, name = lgd_titl) +
      geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") + 
      geom_sf(data = recs_eur_sf, shape=21, 
              size=1, fill= "black", color = "white") +
      theme_func("right") +
      theme(legend.position = c(0.15, 0.78),
            legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
            legend.background = element_rect(fill = "white"))
    
    assign(paste0(var, "_eur.p2"), eur.p2)
  
}

# Combine plots for the 4 variables
All_eur.p <- plot_grid(GI_eur.p2, CS_eur.p, HS_eur.p, DS_eur.p, EI_eur.p2, EI.ir_eur.p2,
                       ncol = 2, nrow = 3,
                       labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                       label_size = 14, hjust = 0 , vjust = 1)
                       #label_size = 12, hjust = -3 , vjust = 1)
ggsave(All_eur.p, file= here("CLIMEX", "Figures", "All_Eurasia_DV09_1.png"),
       width = 8, height = 9, units = c('in'), dpi=300)
       #width = 5, height = 11, units = c('in'), dpi=300)

## Plots for North America ----

# Filter out North America records, and crop world countries features
recs_na_sf <- recs_sf %>%
  filter(Continent == "North America") %>%
  st_transform(., CRS("+init=epsg:5070")) 

# USA Contiguous Albers Equal Area Conic (epsg:5070)
ext_na <- c(xmin = -170,  xmax = -51, ymin= 24.6, ymax = 71.8)
ext_conus <- c(xmin = -170, xmax = -51, ymin= 25.5, ymax = 52)
prj_na <- CRS("+init=epsg:5070 ")

# Country features
na_cntry_feats <- RegionCrop(type = "countries", ext = ext_na, prj = prj_na)
na_cntry_p <- na_cntry_feats[[1]] # Polygon feature
na_cntry_l <- na_cntry_feats[[2]] # Line features

# State features (inc. Canada provinces) - all NA and then just CONUS
na_states_feats <- RegionCrop(type = "states", ext = ext_na, prj = prj_na)
na_states_p <- na_states_feats[[1]] # Polygon feature
na_states_l <- na_states_feats[[2]] # Line features

conus_states_feats <- RegionCrop(type = "states", ext = ext_conus, prj = prj_na)
conus_states_p <- conus_states_feats[[1]] # Polygon feature
conus_states_l <- conus_states_feats[[2]] # Line features

# Crop and project CLIMEX results (convert to raster, then back to data frame)
rast_lst <- list(GI_rast, CS_rast, HS_rast, DS_rast, EI_rast, EI.ir_rast)
CLMX_pts_na <- Proj_CLIMEX(rast_lst, ext_na, prj_na)
names(CLMX_pts_na) <- c("GI_na_df", "CS_na_df", "HS_na_df", "DS_na_df", 
                        "EI_na_df", "EI.ir_na_df")
CLMX_pts_conus <- Proj_CLIMEX(rast_lst, ext_conus, prj_na)
names(CLMX_pts_conus) <- c("GI_conus_df", "CS_conus_df", "HS_conus_df", 
                              "DS_conus_df", "EI_conus_df", "EI.ir_conus_df")

# Plot the results
df_lst <- list(CLMX_pts_na, CLMX_pts_conus)
pols <- list(na_cntry_p, conus_states_p)
lns <- list(na_cntry_l, conus_states_l)
type <- c("na", "conus")
vars <- c("GI", "CS", "HS", "DS", "EI", "EI.ir")

for (j in seq_along(df_lst)) {
  
  dfs <- df_lst[[j]]
  
  for (i in seq_along(dfs)) {
    df <- dfs[[i]]
    #df <- df %>% mutate(value = na_if(value, 0))
    df <- df %>% filter(!(value == 0))
    var <- vars[i]
    
    ## Format for plotting data in categories
    func_outs <- Bin_EI(var, df)
    brks <- func_outs[[1]]
    labs <- func_outs[[2]]
    df2 <- func_outs[[3]]
    
    # Order factors, set color scale
    df2$value_bin <- factor(df2$value_bin, 
                            levels = unique(df2$value_bin[order(df2$value)])) 
    cols2 <- setNames(cols_binned[[i]], levels(df2$value_bin))
    
    # Change legend title for EI irrigated
    if (var == "EI.ir") {
      lgd_titl <- "EI"
    } else {
      lgd_titl <- var
    }
    
    # Make the plot
    p <- ggplot() + 
      geom_sf(data = pols[[j]], color="gray20",  fill = "gray85", lwd = 0.3) +
      geom_tile(data = df, aes(x = x, y = y, fill = value)) +
      scale_fill_gradient(low=low_cols[i], high = high_cols[i], 
                          na.value = "transparent",
                          name = lgd_titl, breaks = brks, labels = labs, 
                          guide = guide_colorbar(
                            frame.colour = "black",
                            barheight = 3,
                            frame.linewidth = 1.5,
                            ticks.colour = "black",
                            ticks.linewidth = 2)) +
      geom_sf(data = lns[[j]], lwd = 0.2, color = "gray10") + 
      geom_sf(data=recs_na_sf, shape=21, 
              size=1, fill= "black", color = "white") +
      theme_func("right") +
      theme(legend.position = c(0.9, 0.3),
            legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
            legend.background = element_rect(fill = "white")) 
    assign(paste0(var, "_", type[j], ".p"), p)

    # Plot with categorical scale (binned data)
    p2 <- ggplot() + 
      geom_sf(data = pols[[j]], color="gray20",  fill = "gray85", lwd = 0.3) +
      geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols2, name = lgd_titl) +
      geom_sf(data = lns[[j]], lwd = 0.2, color = "gray10") + 
      geom_sf(data = recs_na_sf, shape=21, 
              size=1, fill= "black", color = "white") +
      theme_func("right") +
      theme(legend.position = c(0.9, 0.3),
            legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
            legend.background = element_rect(fill = "white"))
      
    assign(paste0(var, "_", type[j], ".p2"), p2) 
      
  }

}

# Combine plots
All_na.p <- plot_grid(GI_na.p2, CS_na.p, HS_na.p, DS_na.p, 
                      EI_na.p2, EI.ir_na.p2, EI.ir_na.p2,
                       ncol = 2, nrow = 3,
                       labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                       label_size = 12, hjust = 0 , vjust = 1)
ggsave(All_na.p, file= here("CLIMEX", "Figures", "All_NorthAmerica_DV09.png"),
       width = 7, height = 8, units = c('in'), dpi=300)

All_conus.p <- plot_grid(GI_conus.p2, CS_conus.p, HS_conus.p, DS_conus.p, 
                         EI_conus.p2, EI.ir_conus.p2, 
                         ncol = 2, nrow = 3,
                         labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                         label_size = 14, hjust = 0 , vjust = 2)
ggsave(All_conus.p, file= here("CLIMEX", "Figures", "All_CONUS_DV09.png"),
       width = 7, height = 8, units = c('in'), dpi=300)


# Extract EI values from validation localities
# All localities (includes duplicate grid cells)

recs_extract <- data.frame("EI" = raster::extract(EI_rast.o, recs_sf),
                 "GI" = raster::extract(GI_rast.o, recs_sf)) %>%
  mutate("Continent" = recs_sf$Continent, "Site" = recs_sf$Site, 
         "Lat" = recs_sf$Latitude, "Long" = recs_sf$Longitude, 
         "State" = recs_sf$Region) 

recs_val <- recs_extract %>%
  filter(Continent %in% c("North America", "New Zealand")) 

sum_stats <- recs_val %>% 
  filter(!is.na(EI)) %>%
  summarise(
  mean_EI = round(mean(EI)), min_EI = min(EI), max_EI = max(EI),
  mean_GI = round(mean(GI)), min_GI = min(GI), max_GI = max(GI))


GI_eur <- raster::extract(GI_rast, recs_eur)
recs_eur_df <- data.frame(recs_eur) %>%
  select(Site, Latitude, Longitude) %>%
  mutate(GI = GI_eur, EI = EI_eur)

# Tally 
EI_tally <- case_when(EI == 0 ~ "unsuitable",
                                 EI > 0 & EI <= 10, "low suitability",
                                 EI > 10 & EI <= 30, "moderate suitability",
                                 EI > 30, "high suitability" )

