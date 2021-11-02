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

## Get CLIMEX model outputs and occurrence records ----
GI_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "GI_World.tif"))
CS_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "CS_World.tif"))
HS_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "HS_World.tif"))
DS_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "DS_World.tif"))
rast_lst <- list(GI_rast, CS_rast, HS_rast, DS_rast)

# Occurrence records
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
coordinates(recs) <- ~Longitude + Latitude
crs(recs) <- CRS("+proj=longlat +datum=WGS84")
recs_sf <- st_as_sf(recs)

## Extents, projection definitions, and spatial features ----

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

## Plots ----

# Themes for plots
mytheme <- theme(plot.margin = unit(c(0.01,0,0,0),"cm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), 
                 panel.border = element_blank(),
                 axis.title.x = element_blank(), axis.title.y = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(), axis.text.y=element_blank(), 
                 legend.text=element_text(size = 9.5),
                 legend.key.width = unit(0.5,"line"), 
                 legend.key.height = unit(0.55,"line"),
                 legend.title = element_text(size = 9.5, face = "bold"),
                 #legend.position = c(0.15, 0.3),
                 legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
                 legend.background = element_rect(fill = "white"))

vars <- c("GI", "CS", "HS", "DS")

# Continuous scale fill colors
low_cols <- c("ivory", "lightcyan", "lightyellow", 
              "blanchedalmond", "lightyellow", "lightyellow")
mid_cols <- c("lightpink", "dodgerblue", "orange", "#80471C", "yellow", "yellow")
high_cols <- c("#000066", "darkblue", "red3", "#3A1F04", "red2", "red2")
midpoints <- c(17, 500, 500, 60, 30, 30)

# Categorical scale fill colors
gi_cols <- colorRampPalette(c("lightyellow", "lightpink", "purple4"))(9)
cs_cols<- colorRampPalette(c("lightblue", "blue2"))(4)
hs_cols <- colorRampPalette(c( "mistyrose1", "red3"))(4)
ds_cols <- colorRampPalette(c("khaki", "darkorange4"))(4)

cols_binned <- list(gi_cols, cs_cols, hs_cols, ds_cols)

# Plots for Eurasia
for (i in seq_along(CLMX_pts_eur)) {
  var <- vars[i]
  df <- CLMX_pts_eur[[i]]
  #df <- df %>% mutate(value = na_if(value, 0))
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
  if (var == "EI.ir") {
    lgd_titl <- "EI"
  } else {
    lgd_titl <- var
  }
  
  # Plot with continuous scale
  cols2 <- setNames(cols_binned[[i]], levels(df2$value_bin))
  
  eur.p <- print(ggplot() + 
    geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray90", lwd = 0.3) +
    geom_tile(data = df, aes(x = x, y = y, fill = value)) +
    #geom_tile(data = df, aes(x = Longitude, y = Latitude, fill = value), 
    #          width = 0.2, height = 0.2) + # Specify w and h, or get white lines
    scale_fill_gradient2(low=low_cols[i], mid = mid_cols[i], high = high_cols[i], 
                        na.value = "transparent", 
                        midpoint = midpoints[i],
                        name = lgd_titl, breaks = sort(brks), labels = labs,
                        guide = guide_colorbar(
                          frame.colour = "black",
                          barheight = 3,
                          frame.linewidth = 1.5,
                          ticks.colour = "black",
                          ticks.linewidth = 2)) +
    geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") + 
    geom_sf(data=recs_eur_sf, shape=21, 
            size=1.5, fill= "black", color = "white") +
    mytheme +
    theme(legend.position = c(0.15, 0.85),
          legend.text=element_text(size = 10),
          legend.key.width = unit(0.55,"line"), 
          legend.key.height = unit(0.65,"line"),
          legend.title = element_blank()))
  
  # Put a scale bar and north arrow on first plot ("GI")
  if (var == "GI") {
    
    eur.p <- eur.p + ggspatial::annotation_scale(
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
  
  assign(paste0(var, "_eur.p"), eur.p)
  
  # Plot with categorical scale (binned data)
  eur.p2 <- print(ggplot() + 
      geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray90", lwd = 0.3) +
      geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols2, name = lgd_titl) +
      geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") + 
      geom_sf(data = recs_eur_sf, shape=21, 
              size=1, fill= "black", color = "white") +
      mytheme +
      theme(legend.position = c(0.15, 0.85)))
    
    assign(paste0(var, "_eur.p2"), eur.p2)
  
}

# Combine plots and save
Stress_eur.p <- plot_grid(GI_eur.p, CS_eur.p, HS_eur.p, DS_eur.p,
                       ncol = 2, nrow = 2,
                       labels = c("(a) Growth index", "(b) Cold stress",
                                  "(c) Heat stress", "(d) Dry stress"),
                       label_size = 14, hjust = 0.05 , vjust = 1)
ggsave(Stress_eur.p, file= here("Final_figures", "CLIMEX_4Stress_Eurasia.png"),
       width = 8, height = 6.7, units = c('in'), dpi=300)

## Plots for North America ----

for (i in seq_along(CLMX_pts_conus)) {
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
  p <- print(ggplot() + 
    geom_sf(data = conus_states_p, color="gray20",  fill = "gray90", lwd = 0.3) +
    geom_tile(data = df, aes(x = x, y = y, fill = value)) +
    scale_fill_gradient2(low=low_cols[i], mid = mid_cols[i], high = high_cols[i], 
                        na.value = "transparent",
                        midpoint = midpoints[i],
                        name = lgd_titl, breaks = brks, labels = labs, 
                        guide = guide_colorbar(
                          frame.colour = "black",
                          barheight = 3,
                          frame.linewidth = 1.5,
                          ticks.colour = "black",
                          ticks.linewidth = 2)) +
    geom_sf(data = conus_states_l, lwd = 0.2, color = "gray10") + 
    geom_sf(data=recs_conus_sf, shape=21, 
            size=1, fill= "black", color = "white") +
    mytheme +
    theme(legend.position = c(0.9, 0.4),
          legend.text=element_text(size = 10),
          legend.key.width = unit(0.55,"line"), 
          legend.key.height = unit(0.65,"line"),
          legend.title = element_blank())) 
  
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
    
    assign(paste0(var, "_conus.p"), p)
    
    # Plot with categorical scale (binned data)
    p2 <- print(ggplot() + 
      geom_sf(data = conus_states_p, color="gray20",  fill = "gray90", lwd = 0.3) +
      geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols2, name = lgd_titl) +
      geom_sf(data = conus_states_l, lwd = 0.2, color = "gray10") + 
      geom_sf(data = recs_conus_sf, shape=21, 
              size=1, fill= "black", color = "white") +
      mytheme +
      theme(legend.position = c(0.9, 0.35)))
      
    assign(paste0(var, "_conus.p2"), p2) 
    
}

# Combine plots and save
Stress_conus.p <- plot_grid(GI_conus.p, CS_conus.p, HS_conus.p, DS_conus.p,
                          ncol = 2, nrow = 2,
                          labels = c("(a) Growth index", "(b) Cold stress",
                                     "(c) Heat stress", "(d) Dry stress"),
                          label_size = 14, hjust = 0 , vjust = 1)
ggsave(Stress_conus.p, file= here("Final_figures", "CLIMEX_4Stress_CONUS.png"),
       width = 8, height = 5.5, units = c('in'), dpi=300)


#rm(list = setdiff(ls(), "outdir"))
