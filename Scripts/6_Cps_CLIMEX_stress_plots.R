## Script to process CLIMEX output data and produce plots depicting 
## climate suitability and climate stress accumulation for Calonectria 
## pseudonaviculata in Europe, western Asia, and North America.

# Processes CLIMEX output data - produces rasters and figures

# Libraries
pkgs <- c("raster", "tidyverse", "maptools",
          "cowplot","sf", "here", "openxlsx", "ggalt", "rnaturalearth")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("Scripts", "Cps_model_functions.R"))

clmx_outs <- here("CLIMEX", "Final_outfls", "TIFs")
clmx_outs <- here("CLIMEX", "TIF_files", "run10")

##  Get CLIMEX model outputs and occurrence records ----
var_lst <- c("GI", "CS", "HS", "DS")
rast_lst <- map(var_lst, function(x) { 
  raster(list.files(path = here(clmx_outs), pattern = x, full.names = TRUE))
})

# Occurrence records
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2022_noORcoords.xlsx")) 
coordinates(recs) <- ~Longitude + Latitude
crs(recs) <- CRS("+proj=longlat +datum=WGS84")
recs_sf <- st_as_sf(recs)

## Extents, projection definitions, and spatial features ----

sf_use_s2(FALSE) # Avoid "Error in s2_geography_from_wkb..."

# Eurasia
ext_eur <- c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)
prj_eur <- CRS("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 
               +ellps=intl +units=m +no_defs")
eur_cntry_feats <- RegionCrop(type = "countries", ext = ext_eur, prj = prj_eur)
eur_cntry_p <- eur_cntry_feats[[1]] # Polygon feature
eur_cntry_l <- eur_cntry_feats[[2]] # Line features

# CONUS
ext_conus <- c(xmin = -170, xmax = -51, ymin= 25.5, ymax = 52)
prj_conus <- CRS("+init=epsg:5070")
conus_states_feats <- RegionCrop(type = "states", 
                                 ext = ext_conus, prj = prj_conus)
conus_states_p <- conus_states_feats[[1]] # Polygon feature
conus_states_l <- conus_states_feats[[2]] # Line features

# World 
ext_world <- c(xmin = -165,  xmax = 175, ymin= -59.47275, ymax = 83.6341)
prj_world <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
                 +datum=WGS84 +units=m +no_defs")

# Polygon and line for world countries
world <- ne_countries(scale = 10, type = "countries", returnclass = "sf") %>%
  filter(!(admin == "Antarctica"))
world <- st_crop(world, ext_world)

# Remove very small countries and convert to poly and line feats
data(wrld_simpl, package = "maptools")
wrld_data <- as.data.frame(wrld_simpl@data)
keep <- filter(wrld_data, FIPS %in% c("TW", "RB", "SV", "WI", "HK", "BE"))
small_cntry <- wrld_data %>% 
  filter(AREA > 400, !FIPS %in% c("FJ", "FP", "WS")) %>%
  bind_rows(., keep)
world2 <- semi_join(world, small_cntry, by = c("fips_10_" = "FIPS")) %>%
  bind_rows(., filter(world, admin == "Norway")) # why does Norway get dropped?
world_p <- st_transform(world2, prj_world)
world_l <- st_cast(world2, "MULTILINESTRING") # Create line feature

# State features (inc. Canada provinces) - all NA and then just CONUS
na_states <- ne_states(returnclass = "sf") %>%
  filter(geonunit %in% c("United States of America", "Canada"))
na_states_p <- st_transform(na_states, prj_world)
na_states_l <- st_cast(na_states, "MULTILINESTRING")

## Crop and project models and records for each region ----

# Eurasia
CLMX_pts_eur <- Rasts_to_df2(rast_lst, ext_eur, prj_eur)
recs_eur_sf <- recs_sf %>% 
  filter(Continent %in% c("Europe", "Asia")) %>%
  st_transform(., prj_eur)

# North America
CLMX_pts_conus <- Rasts_to_df2(rast_lst, ext_conus, prj_conus)
recs_conus_sf <- recs_sf %>%
  filter(Continent == "North America") %>%
  st_transform(., CRS("+init=epsg:5070")) 

# World
CLMX_pts_world <- Rasts_to_df2(rast_lst, ext_world, prj_world)[-1] # Remove GI

## Plots ----

# Themes for plots
mytheme <- theme(plot.margin = unit(c(t=0, b=0, l=0, r=0),"cm"),
                plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
                panel.border = element_blank(),
                panel.background = element_rect(fill = "transparent"), # bg of the panel
                panel.grid.major = element_blank(), # get rid of major grid
                panel.grid.minor = element_blank(), # get rid of minor grid
                axis.title.x = element_blank(), axis.title.y = element_blank(), 
                axis.ticks = element_blank(),
                axis.text.x=element_blank(), axis.text.y=element_blank(), 
                legend.text=element_text(size = 8.5),
                legend.title = element_text(size = 8.5, face = "bold"),
                #legend.position = c(0.15, 0.3),
                legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
                legend.background = element_rect(fill = "white"), # get rid of legend bg
                legend.key = element_rect(fill = "white", colour = NA),
                legend.box.background = element_rect(fill = "white", colour = NA)) # get rid of legend panel bg)

vars <- c("GI", "CS", "HS", "DS")

# Continuous scale fill colors
low_cols <- c("lightyellow", "lightcyan", "lightyellow", "blanchedalmond")
mid_cols <- c("#00d4ab", "dodgerblue", "orange", "#A97263")
high_cols <- c("#003027", "darkblue", "red3", "#160609")
midpoints_eur <- c(25, 500, 500, 50)

# Categorical scale fill colors
gi_cols <- colorRampPalette(c("lightyellow", "#55C668FF", "#010D00"))(4)
cs_cols<- colorRampPalette(c("lightblue", "blue2"))(4)
hs_cols <- colorRampPalette(c( "mistyrose1", "red3"))(4)
ds_cols <- colorRampPalette(c("khaki", "darkorange4"))(4)

cols_binned <- list(gi_cols, cs_cols, hs_cols, ds_cols)

# Plots for Eurasia
eur_maps <- map(1:length(CLMX_pts_eur), function(i) {
  var <- vars[i]
  df <- CLMX_pts_eur[[i]]
  df <- df %>% filter(!(value == 0))
  
  ## Format for plotting data in categories
  func_outs <- Bin_CLMX(var, df)
  brks <- func_outs[[1]]
  labs <- func_outs[[2]]
  df2 <- func_outs[[3]]
  
  # Order factors
  df2$value_bin <- factor(df2$value_bin, 
                          levels = unique(df2$value_bin[order(df2$value)])) 
  
  # Change legend title for EI irrigated
  # if (var == "EI.ir") {
  #   lgd_titl <- "EI"
  # } else {
    lgd_titl <- var
  #}
  
  # Plot with continuous scale
  cols2 <- setNames(cols_binned[[i]], levels(df2$value_bin))
  
  p <- ggplot() + 
    geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray90", lwd = 0.3) +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    #geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = value), 
    #          width = 0.2, height = 0.2) + # Specify w and h, or get white lines
    scale_fill_gradient2(low=low_cols[i], mid = mid_cols[i], high = high_cols[i], 
                        na.value = "transparent", 
                        midpoint = midpoints_eur[i],
                        name = lgd_titl, breaks = sort(brks), labels = labs,
                        guide = guide_colorbar(
                          frame.colour = "black",
                          barheight = 3,
                          frame.linewidth = 1.5,
                          ticks.colour = "black",
                          ticks.linewidth = 2)) +
    geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") + 
    geom_sf(data = recs_eur_sf, shape=19, size=1.75, color = "white") +
    geom_sf(data = recs_eur_sf, shape=21, size=1, fill= "magenta", color = "black") +
    #geom_sf(data=recs_eur_sf, shape=21, 
    #        size=1, fill= "magenta", color = "black") +
    mytheme +
    theme(legend.position = c(0.15, 0.85),
          legend.text=element_text(size = 10),
          legend.key.width = unit(0.55,"line"), 
          legend.key.height = unit(0.65,"line"),
          legend.title = element_blank())
  
  # Put a scale bar and north arrow on first plot ("GI")
  if (var == "GI") {
    
    p <- p + ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("grey60", "white"),
      pad_x = unit(1, "cm"),
      pad_y = unit(0.5, "cm")
    )  + 
      ggspatial::annotation_north_arrow(
        location =  "tr",
        height = unit(0.75, "cm"),
        width = unit(0.75, "cm"),
        pad_x = unit(2, "cm")
      )
      
  } 
  
  return(p)
  
  #assign(paste0(var, "_eur.p"), eur.p)
  
  # Plot with categorical scale (binned data)
  # eur.p2 <- print(ggplot() + 
  #     geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray90", lwd = 0.3) +
  #     geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
  #     scale_fill_manual(values = cols2, name = lgd_titl) +
  #     geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") + 
  #     geom_sf(data = recs_eur_sf, shape=21, size=1, fill= "magenta", color = "black") +
  #     mytheme +
  #     theme(legend.position = c(0.15, 0.85)))
  #   
  #   assign(paste0(var, "_eur.p2"), eur.p2)
  # 
})

# Combine plots and save
Stress_eur.p <- plot_grid(eur_maps[[1]], eur_maps[[2]], eur_maps[[3]], eur_maps[[4]],
                       ncol = 2, nrow = 2,
                       labels = c("(a) Growth index", "(b) Cold stress",
                                  "(c) Heat stress", "(d) Dry stress"),
                       label_size = 14, hjust = 0.005 , vjust = 1.05)
ggsave(Stress_eur.p, file= here("Final_figures", "CLIMEX_4Stress_Eurasia.png"),
       width = 8, height = 6.7, units = c('in'), dpi=300)

## Plots for North America ----
midpoints_conus <- c(20, 500, 500, 60)

con_maps <- map(1:length(CLMX_pts_conus), function(i) {
  var <- vars[i]
  df <- CLMX_pts_conus[[i]]
  df <- df %>% filter(!(value == 0))
  var <- vars[i]
  
  ## Format for plotting data in categories
  func_outs <- Bin_CLMX(var, df)
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
    geom_sf(data = conus_states_p, color="gray20",  fill = "gray90", lwd = 0.3) +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradient2(low=low_cols[i], mid = mid_cols[i], high = high_cols[i], 
                        na.value = "transparent",
                        midpoint = midpoints_conus[i],
                        name = lgd_titl, breaks = brks, labels = labs, 
                        guide = guide_colorbar(
                          frame.colour = "black",
                          barheight = 3,
                          frame.linewidth = 1.5,
                          ticks.colour = "black",
                          ticks.linewidth = 2)) +
    geom_sf(data = conus_states_l, lwd = 0.2, color = "gray10") + 
    geom_sf(data = recs_conus_sf, shape=19, size=1.55, color = "white") +
    geom_sf(data = recs_conus_sf, shape=21, size=1, fill= "magenta", color = "black") +
    #geom_sf(data=recs_conus_sf, shape=21, size=1, fill= "magenta", color = "black") +
    mytheme +
    theme(legend.position = c(0.9, 0.4),
          legend.text=element_text(size = 10),
          legend.key.width = unit(0.55,"line"), 
          legend.key.height = unit(0.65,"line"),
          legend.title = element_blank()) 
  
    # Put a scale bar and north arrow on first plot ("GI")
    if (var == "GI") {
      
      p <- p + ggspatial::annotation_scale(
        location = "br",
        bar_cols = c("grey60", "white"),
        text_cex = 0.6
      )  + 
        ggspatial::annotation_north_arrow(
          location =  "bl",
          height = unit(0.75, "cm"),
          width = unit(0.75, "cm"),
          pad_y = unit(0.5, "cm")
        )
      
    } 
    # 
    # assign(paste0(var, "_conus.p"), p)
    # 
    # # Plot with categorical scale (binned data)
    # p2 <- print(ggplot() + 
    #   geom_sf(data = conus_states_p, color="gray20",  fill = "gray90", lwd = 0.3) +
    #   geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
    #   scale_fill_manual(values = cols2, name = lgd_titl) +
    #   geom_sf(data = conus_states_l, lwd = 0.2, color = "gray10") + 
    #   geom_sf(data = recs_conus_sf, shape=21, 
    #           size=1, fill= "maroon3", color = "black") +
    #   mytheme +
    #   theme(legend.position = c(0.9, 0.35)))
    #   
    # assign(paste0(var, "_conus.p2"), p2) 
    return(p)
})

# Combine plots and save
Stress_conus.p <- plot_grid(con_maps[[1]], con_maps[[2]], con_maps[[3]], con_maps[[4]],
                          ncol = 2, nrow = 2,
                          labels = c("(a) Growth index", "(b) Cold stress",
                                     "(c) Heat stress", "(d) Dry stress"),
                          label_size = 14, hjust = 0 , vjust = 1.05)
ggsave(Stress_conus.p, file= here("Final_figures", "CLIMEX_4Stress_CONUS_DV1-17_DV2-22.png"),
       width = 8, height = 5.1, units = c('in'), dpi=300)


## Plots for global scale ----

world_strs_stk <- stack(CS_rast, HS_rast, DS_rast)

# Input stress factors
vars <- c("CS", "HS", "DS")
nams <- c("Cold stress", "Heat stress", "Dry stress")
low_cols <- c("lightcyan", "lightyellow", "blanchedalmond")
mid_cols <- c("dodgerblue", "orange", "#A97263")
high_cols <- c("darkblue", "red3", "#160609")
midpoints <- c(500, 500, 60)

# Make world plots for each stress factor
world_strs_plots <- map(1:length(vars), function(i) {

  # Project, and process results
  strs_out <- Rasts_to_df2(list(world_strs_stk[[i]]), ext_world, prj_world)[[1]] %>%
    filter(!(value == 0)) %>%
    Bin_CLMX(vars[i], .)
  
  # Plot
  p <- ggplot() + 
    geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
    geom_raster(data = strs_out[[3]], aes(x = x, y = y, fill = value)) +
    scale_fill_gradient2(low = low_cols[i], mid = mid_cols[i], high = high_cols[i], 
                         na.value = "transparent", 
                         midpoint = midpoints[i],
                         name = nams[[i]],
                         breaks = strs_out[[1]], labels = strs_out[[2]],
                         guide = guide_colorbar(
                           frame.colour = "black",
                           barheight = 3,
                           frame.linewidth = 1.5,
                           ticks.colour = "black",
                           ticks.linewidth = 2)) +
    # scale_fill_manual(values = cols_hs, name = c("Heat stress")) +
    geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
    geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
    mytheme + 
    theme(legend.position = c(0.1, 0.3))
  
})

# Arrange plots and save
strs_world.p <- plot_grid(world_strs_plots[[1]], world_strs_plots[[2]], 
                          world_strs_plots[[3]], ncol = 1,
                          labels = c("(a) Cold stress", "(b) Heat stress", 
                                     "(c) Dry stress"),
                          label_size = 11, hjust = -0.5, vjust = 1)

ggsave(strs_world.p, file = here("Final_figures", "World_CLIMEX_Stress.png"),
       width = 7, height = 8.5, units = c('in'), dpi=300)

# Crop white margins
knitr::plot_crop(here("Final_figures", "World_CLIMEX_Stress.png"))

#rm(list = setdiff(ls(), "outdir"))
