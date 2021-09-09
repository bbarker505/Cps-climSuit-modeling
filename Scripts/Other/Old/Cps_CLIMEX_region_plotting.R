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

## Plots for Eurasia ----

# Projection and extent
# Lambert Conformal Conic looks best
ext_eur <- c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)
prj_eur <- CRS("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 
               +ellps=intl +units=m +no_defs")

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
# Themes for plots
mytheme <- theme(plot.margin = unit(c(0.01,0,0,0),"cm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), panel.border = element_blank(),
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

vars <- c("GI", "CS", "HS", "DS", "EI", "EI.ir")

# Continuous scale fill colors
low_cols <- c("ivory", "lightcyan", "lightyellow", "blanchedalmond", "lightyellow", "lightyellow")
mid_cols <- c("lightpink", "dodgerblue", "orange", "#80471C", "yellow", "yellow")
high_cols <- c("#000066", "darkblue", "red3", "#3A1F04", "red2", "red2")
midpoints <- c(17, 500, 500, 60, 30, 30)

# Categorical scale fill colors
#gi_cols <- colorRampPalette(c("lightpink", "purple4"))(9)
gi_cols <- colorRampPalette(c("lightyellow", "lightpink", "purple4"))(9)
cs_cols<- colorRampPalette(c("lightblue", "blue2"))(4)
hs_cols <- colorRampPalette(c( "mistyrose1", "red3"))(4)
ds_cols <- colorRampPalette(c("khaki", "darkorange4"))(4)
#ei_cols <- rev(brewer.pal(7, "RdYlBu"))
#ei_cols <- c("cyan", "yellow2", "orange", "red")
ei_cols <- c("#313695","#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
             "#EB8B55", "#C8453D", "#A50026")

cols_binned <- list(gi_cols, cs_cols, hs_cols, ds_cols, ei_cols, ei_cols)

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
    geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray85", lwd = 0.3) +
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
      geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray85", lwd = 0.3) +
      geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols2, name = lgd_titl) +
      geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") + 
      geom_sf(data = recs_eur_sf, shape=21, 
              size=1, fill= "black", color = "white") +
      mytheme +
      theme(legend.position = c(0.15, 0.85)))
    
    assign(paste0(var, "_eur.p2"), eur.p2)
  
}

# Need to fix EI plot (make legend smaller for EI)
eur_EI_fix <- map(list(EI_eur.p2, EI.ir_eur.p2), function(x) {
  p2 <- x + 
    theme(legend.text=element_text(size = 8),
          legend.title = element_text(size = 8, face = "bold"))
})

# Combine plots for all outputs (stress plus EI)
All_eur.p <- plot_grid(GI_eur.p, CS_eur.p, HS_eur.p, DS_eur.p, eur_EI_fix[[1]], eur_EI_fix[[2]],
                       ncol = 2, nrow = 3,
                       labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                       label_size = 14, hjust = 0.05 , vjust = 1)
ggsave(All_eur.p, file= here("CLIMEX", "Figures", "All_Eurasia_DV09_SMDS02_SM107.png"),
       width = 8, height = 9.5, units = c('in'), dpi=300)
ggsave(All_eur.p, file= here("Final_figures", "CLIMEX_Eurasia.png"),
       width = 8, height = 9.5, units = c('in'), dpi=300)

# Combine only stress plots
Stress_eur.p <- plot_grid(GI_eur.p, CS_eur.p, HS_eur.p, DS_eur.p,
                       ncol = 2, nrow = 2,
                       labels = c("(a) Growth index", "(b) Cold stress",
                                  "(c) Heat stress", "(d) Dry stress"),
                       label_size = 14, hjust = 0.05 , vjust = 1)
ggsave(Stress_eur.p, file= here("Final_figures", "CLIMEX_4Stress_Eurasia.png"),
       width = 8, height = 6.7, units = c('in'), dpi=300)


## Plots for North America ----

# Filter out North America records, and crop world countries features
recs_conus_sf <- recs_sf %>%
  filter(Continent == "North America") %>%
  st_transform(., CRS("+init=epsg:5070")) 

# USA Contiguous Albers Equal Area Conic (epsg:5070)
ext_conus <- c(xmin = -170, xmax = -51, ymin= 25.5, ymax = 52)
prj_conus <- CRS("+init=epsg:5070 ")

# State features (inc. Canada provinces) - all NA and then just CONUS
conus_states_feats <- RegionCrop(type = "states", ext = ext_conus, prj = prj_conus)
conus_states_p <- conus_states_feats[[1]] # Polygon feature
conus_states_l <- conus_states_feats[[2]] # Line features

# Crop and project CLIMEX results (convert to raster, then back to data frame)
CLMX_pts_conus <- Proj_rasts(rast_lst, ext_conus, prj_conus)
names(CLMX_pts_conus) <- c("GI_conus_df", "CS_conus_df", "HS_conus_df", 
                              "DS_conus_df", "EI_conus_df", "EI.ir_conus_df")

# Plot the results
df_lst <- list(CLMX_pts_na, CLMX_pts_conus)
pols <- list(na_cntry_p, conus_states_p)
lns <- list(na_cntry_l, conus_states_l)
type <- c("na", "conus")
vars <- c("GI", "CS", "HS", "DS", "EI", "EI.ir")

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
    geom_sf(data = conus_states_p, color="gray20",  fill = "gray85", lwd = 0.3) +
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
      geom_sf(data = conus_states_p, color="gray20",  fill = "gray85", lwd = 0.3) +
      geom_tile(data = df2, aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols2, name = lgd_titl) +
      geom_sf(data = conus_states_l, lwd = 0.2, color = "gray10") + 
      geom_sf(data = recs_conus_sf, shape=21, 
              size=1, fill= "black", color = "white") +
      mytheme +
      theme(legend.position = c(0.9, 0.35)))
      
    assign(paste0(var, "_conus.p2"), p2) 
    
}


# CONUS
# Need to fix EI plot (make legend smaller for EI)
conus_EI_fix <- map(list(EI_conus.p2, EI.ir_conus.p2), function(x) {
  p2 <- x + 
    theme(legend.text=element_text(size = 8.5),
          legend.title = element_text(size = 8.5, face = "bold"))
})

# Combine all plots (stress factors and EI)
All_conus.p <- plot_grid(GI_conus.p, CS_conus.p, HS_conus.p, DS_conus.p, 
                         conus_EI_fix[[1]], conus_EI_fix[[2]], 
                         ncol = 2, nrow = 3,
                         labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                         label_size = 14, hjust = 0 , vjust = 2)
ggsave(All_conus.p, file= here("CLIMEX", "Figures", "All_CONUS_DV09_SM2.png"),
       width = 7.5, height = 8.5, units = c('in'), dpi=300)
ggsave(All_conus.p, file= here("Final_figures", "CLIMEX_CONUS.png"),
       width = 8, height = 8.5, units = c('in'), dpi=300)

# Combine only stress plots
Stress_conus.p <- plot_grid(GI_conus.p, CS_conus.p, HS_conus.p, DS_conus.p,
                          ncol = 2, nrow = 2,
                          labels = c("(a) Growth index", "(b) Cold stress",
                                     "(c) Heat stress", "(d) Dry stress"),
                          label_size = 14, hjust = 0 , vjust = 1)
ggsave(Stress_conus.p, file= here("Final_figures", "CLIMEX_4Stress_CONUS.png"),
       width = 8, height = 5.5, units = c('in'), dpi=300)


# Crop white space off final figures
knitr::plot_crop(here("Final_figures", "CLIMEX_Eurasia.png"))
knitr::plot_crop(here("Final_figures", "CLIMEX_CONUS.png"))

# Extract EI values from validation localities
EI_rast <- raster(here("CLIMEX", "TIF_files", "run9", "EI_World.tif"))
GI_rast <- raster(here("CLIMEX", "TIF_files", "run9", "GI_World.tif"))

# All localities (includes duplicate grid cells)
recs_extract <- data.frame("EI" = raster::extract(EI_rast, recs_sf),
                 "GI" = raster::extract(GI_rast, recs_sf)) %>%
  mutate("Continent" = recs_sf$Continent, 
         "Lat" = recs_sf$Latitude, "Long" = recs_sf$Longitude,
         "Site" = recs_sf$Site, "Country" = recs_sf$Country,
         "State" = recs_sf$Region) 

# Summarize range of EI and GI values for continents and countries
sum_stats.set <- recs_extract %>% 
  mutate(Set = ifelse(
    Continent %in% c("North America", "New Zealand"), "Validation", "Fitting")) %>%
  group_by(Set) %>%
  filter(!is.na(EI)) %>%
  summarise(
  mean_EI = round(mean(EI)), min_EI = min(EI), max_EI = max(EI),
  mean_GI = round(mean(GI)), min_GI = min(GI), max_GI = max(GI))
write.xlsx(sum_stats.set, here("CLIMEX", "run9", "Sumstats_Fit_v_Valid_EI_GI.xlsx"))

sum_stats.cntry <- recs_extract %>% 
  group_by(Country) %>%
  filter(!is.na(EI)) %>%
  summarise(
    mean_EI = round(mean(EI)), min_EI = min(EI), max_EI = max(EI),
    mean_GI = round(mean(GI)), min_GI = min(GI), max_GI = max(GI))
write.xlsx(sum_stats.cntry, here("CLIMEX", "run9", "Sumstats_country_EI_GI.xlsx"))

# Tally how many localities have EI less than 10
tally_EILt10 <- recs_extract %>% 
  group_by(Country) %>%
  filter(EI < 10) %>%
  count(., name = "n_Lt10") 
tallyEI0 <- recs_extract %>% 
  group_by(Country) %>%
  filter(EI == 0) %>%
  count(., name = "n_0") 
tally_both <- left_join(tally_EILt10, tallyEI0)
write.xlsx(tally_both, here("CLIMEX", "run9", "N_locs_EI_Lt10.xlsx"))
