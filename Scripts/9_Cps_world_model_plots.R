## Script to produce plots depicting climate suitability and the potential 
## distribution for Calonectria pseudonaviculata at a global scale using outputs
## from CLIMEX model and ensemble correlative model.

# Libraries
pkgs <- c("raster", "tidyverse", "knitr", "maptools", "cowplot","sf",
          "here", "openxlsx", "ggalt", "rnaturalearth")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("scripts", "Cps_model_functions.R"))

# World Robinson projection
prj_world <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
                 +datum=WGS84 +units=m +no_defs")
ext_world <- c(xmin = -165,  xmax = 175, ymin= -59.47275, ymax = 83.6341)

# Polygon and line for world countries
world <- ne_countries(scale = 10, type = "countries", returnclass = "sf") %>%
  filter(!(admin == "Antarctica"))
ext_world <- as(raster::extent(-165, 175, -59.47275, 83.6341), "SpatialPolygons")

# For some reason st_crop is not working right so have to convert to sp obj first
sf_use_s2(FALSE) # Avoid projection issue - unclear why this works but does
world <- st_crop(world, ext_world)
world_p <- st_transform(world, prj_world)
world_l <- st_cast(world, "MULTILINESTRING") # Create line feature

# State features (inc. Canada provinces) - all NA and then just CONUS
na_states <- ne_states(returnclass = "sf") %>%
  filter(geonunit %in% c("United States of America", "Canada"))
na_states_p <- st_transform(na_states, prj_world)
na_states_l <- st_cast(na_states, "MULTILINESTRING")

## Outputs and formatting  ----

# Out files

# CLIMEX - crop, project, and bin EI values
ei_rast <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI_World.tif"))
ei_rast.ir <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI.ir_World.tif"))

ei_dfs <- map(list(ei_rast, ei_rast.ir), function(x) {
  ei_df <- (Rasts_to_df2(list(x), ext_world, prj_world)[[1]]) %>%
    filter(!(value == 0))
  ei_df <- Bin_CLMX("EI", ei_df)[[3]] %>%
    #mutate(EI = factor(value_bin, levels = c("1-10", "11-20", "21-30", "31-100")))
    mutate(value_bin = factor(value_bin, 
                              levels = unique(value_bin[order(value)])))
})

# Ensemble correlative model for world - probability scores + threshold
# Crop and project both layers, and bin probability scores
#out_PCA <- here("ENMTML", "Outfiles", "run_PCA_9-27-2021")

# Ensemble probability scores to plot
ens_type <- c("PCA", "PCA_SUP", "W_MEAN")

ens_dfs <- map(ens_type, function(ens) {
  rast <- raster(here(out_PCA, "Projection", "World", "Ensemble", 
                      ens, "calonectria_pseudonaviculata.tif"))
  prob_df <- Rasts_to_df1(rast, ext_world, prj_world) %>%
    filter(complete.cases(value)) %>%
    filter(!(value < 0.1)) %>%
    Bin_pres(.) %>%
    mutate(value = factor(value_bin,
           levels = unique(value_bin[order(value)]))) 
})

## Plots: CLIMEX vs. correlative ensemble climate suitability model ----

# Theme
mytheme <- theme(plot.margin = unit(c(0.01,0,0,0),"cm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), panel.border = element_blank(),
                 axis.title.x = element_blank(), axis.title.y = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(), axis.text.y=element_blank(), 
                 legend.text=element_text(size = 9.5),
                 legend.key.width = unit(0.6,"line"), 
                 legend.key.height = unit(0.65,"line"),
                 legend.title = element_text(size = 9.5, face = "bold"),
                 legend.position = c(0.1, 0.3),
                 legend.margin=margin(0,0,0,0, unit="mm"),
                 legend.background = element_rect(fill = "white"))


# Colors
cols <- c("#313695","#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
"#EB8B55", "#C8453D", "#A50026")

# CLIMEX plot (EI)
world_ei.p <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
  geom_tile(data = ei_dfs[[2]], aes(x = x, y = y, fill = value_bin)) +
  scale_fill_manual(values = cols, name = "Ecoclimatic\nindex") +
  geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
  mytheme 
ggsave(world_ei.p, file = here("CLIMEX", "Figures", "ei_world_DV09_cats_4.png"), 
       width = 8, height = 4, units = c('in'), dpi=300)

# Ensemble correlative model plots
# Comparison of 3 ensemble model algorithms
names(ens_dfs) <- paste("Ensemble", ens_type, sep = "_")
world_ens_plots <- map(1:length(ens_dfs), function(i) {
  ens_df <- ens_dfs[[i]]
  outfl <- paste0(names(ens_dfs[i]), ".png")
  world_ens.p <- ggplot() + 
    geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
    geom_tile(data = ens_df, aes(x = x, y = y, fill = value_bin)) +
    scale_fill_manual(values = cols, name = "Prob. of\noccurrence") +
    geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
    geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
    mytheme 
  ggsave(world_ens.p, 
         filename = here(out_PCA, outfl),
             width = 8, height = 4, units = c('in'), device = "png", dpi=300) 
  #return(world_ens.p)
  
  # Final plot for manuscript
  if (ens_type[i] == "W_MEAN") {
    Both_world.p <- plot_grid(world_ei.p, world_ens.p, 
                             ncol = 1, label_x = 0.05,
                             labels = c("(a) CLIMEX", "(b) Correlative (ensemble)"),                             
                             label_size = 11, hjust = 0, vjust = 1)
    ggsave(Both_world.p, file = here("Final_figures", "World_CLIMEX_Ensemble.png"),
           width = 8, height = 7, units = c('in'), dpi=300)
    
    # Crop white margins (can't remove using theme settings)
    knitr::plot_crop(here("Final_figures", "World_CLIMEX_Ensemble.png"))
  }

})

## Plots: Presence maps (potential distribution) at global scale ----

# CLIMEX: EI > 10 
ei_pres_dfs <- map(list(ei_rast, ei_rast.ir), function(x) {
  ei_thr_rast <- x >= 10 # Areas w/ EI >= 10 defines potential distribution
  ei_thr_rast[ei_thr_rast <= 0] <- NA # Convert 0 values to NA
  ei_thr_rast_crp <- crop(ei_thr_rast, extent(ext_world)) # Crop and project
  ei_thr_rast_prj <- projectRaster(ei_thr_rast_crp, crs = prj_world, method = "ngb")
  ei_thr_df <- data.frame(rasterToPoints(ei_thr_rast_prj)) %>%
    rename("CLIMEX" = "layer") %>%
    filter(CLIMEX == 1)
})

# Ensemble probability scores to plot
ens_types <- c("PCA", "PCA_SUP", "W_MEAN")
thr_types <- c("MAX_TSS")

# Color table
cols_thres <- c("All models" = "purple", "CLIMEX" = "red3",
                "CLIMEX-irrig" = "orange", "Correlative" = "blue2")

# Plots
thr_dfs <- map(ens_types, function(e) {

  for (t in thr_types) {
    
    # Outfile
    outfl2 <- paste0("CLIMEX_v_", e, "_", t, ".png")
    
    # Process rasters
    thr_rast <- raster(here(out_PCA, "Projection", "World", "Ensemble", 
                            e, t, "calonectria_pseudonaviculata.tif"))
    crs(thr_rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    thr_rast[thr_rast <= 0] <- NA
    thr_rast <- resample(thr_rast, ei_rast) # Same cell size as CLIMEX
    thr_rast[!is.na(thr_rast)] <- 1
    thr_rast_crp <- crop(thr_rast, extent(ext_world))
    thr_rast_prj <- projectRaster(thr_rast_crp, crs = prj_world, method = "ngb")
    # Convert raster to data frame - keep only presence values
    thr_df <-  data.frame(rasterToPoints(thr_rast_prj)) %>%
      rename("Correlative" = "calonectria_pseudonaviculata") %>%
      filter(Correlative == 1)
    
    # Combine all data frames
    df <- list(thr_df, ei_pres_dfs[[1]], ei_pres_dfs[[2]]) %>% 
      reduce(full_join, by = c("x", "y")) %>%
      rename("CLIMEX" = "CLIMEX.x", "CLIMEX.ir" = "CLIMEX.y") %>%
      mutate(sum = rowSums(dplyr::select(., CLIMEX, CLIMEX.ir, Correlative), na.rm = TRUE)) %>%
      replace(is.na(.), 0) %>%
      filter(!(sum == 0)) %>% 
      mutate(
        value = case_when(CLIMEX.ir == 1 & Correlative == 1 ~ "All models",
                          CLIMEX.ir == 0 & CLIMEX == 0 & Correlative == 1 ~ "Correlative",
                          CLIMEX.ir == 1 & CLIMEX == 1 ~ "CLIMEX",
                          CLIMEX.ir == 1 & CLIMEX == 0 & Correlative == 0 ~ "CLIMEX-irrig")
      )
    
    # Plot
    world_thrs.p <- ggplot() + 
      geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
      geom_tile(data = df, aes(x = x, y = y, fill = value)) +
      scale_fill_manual(values = cols_thres, name = "Model type") +
      geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
      geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
      mytheme  + 
      theme(legend.position = c(0.15, 0.3),
            legend.background = element_rect(fill = "white"),
            legend.box.background = element_rect(fill = "white", color = "white"),
            legend.key =  element_rect(fill = "white"))
    ggsave(world_thrs.p, 
           filename = here(out_PCA, outfl2),
           width = 8, height = 4, units = c('in'), device = "png", dpi=300) 
    
    #return(world_thrs.p)
    if (e == "W_MEAN" & t == "MAX_TSS") {
      ggsave(world_thrs.p, 
             filename = here("Final_figures", "World_CLIMEX_v_Corr_threshold.png"),
             width = 8, height = 4, units = c('in'), device = "png", dpi=300) 
      knitr::plot_crop(here("Final_figures", "World_CLIMEX_v_Corr_threshold.png"))
    }
  }
 
})

## Plots: climate stress (CLIMEX) at global scale ----

# Crop and project raster, convert to a data frame, and bin heat stress values
strs_stk <- stack(
  raster(here("CLIMEX", "TIF_files", "run9", "HS_world.tif")),
  raster(here("CLIMEX", "TIF_files", "run9", "CS_world.tif")),
  raster(here("CLIMEX", "TIF_files", "run9", "DS_world.tif"))
)

# Input stress factors
vars <- c("HS", "CS", "DS")
nams <- c("Heat stress", "Cold stress", "Dry stress")
low_cols <- c("lightyellow", "lightcyan", "blanchedalmond")
mid_cols <- c("orange", "dodgerblue", "#80471C")
hi_cols <- c("red3", "darkblue", "#3A1F04")
midpoints <- c(500, 500, 60)
  
# Make world plots for each stress factor
strs_plots <- lapply(1:length(vars), function(i) {
  
  # Project, and process results
  strs_out <- (Rasts_to_df2(list(strs_stk[[i]]), ext_world, prj_world)[[1]] ) %>%
    filter(!(value == 0)) %>%
    Bin_CLMX(vars[i], .)
  
  # Plot
  p <- ggplot() + 
    geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
    geom_tile(data = strs_out[[3]], aes(x = x, y = y, fill = value)) +
    scale_fill_gradient2(low = low_cols[i], mid = mid_cols[i], high = hi_cols[i], 
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
    mytheme 
  
})

# Arrange plots and save
strs_world.p <- plot_grid(strs_plots[[2]], strs_plots[[1]], strs_plots[[3]],
                         ncol = 1,
                         labels = c("(a) Cold stress", "(b) Heat stress", 
                                    "(c) Dry stress"),
                         label_size = 11, hjust = -0.5, vjust = 1)

ggsave(strs_world.p, file = here("Final_figures", "World_CLIMEX_Stress.png"),
       width = 7, height = 8.5, units = c('in'), dpi=300)

# Crop white margins
knitr::plot_crop(here("Final_figures", "World_CLIMEX_Stress.png"))

# MOP output map ----

# High MOP index values indicate high similarity between climates in 
# modeling training and projection areas
MOP <- raster(here(out_PCA, "Projection", "World", 
                   "Extrapolation", "calonectria_pseudonaviculata_MOP.tif"))
crs(MOP) <- CRS("+proj=longlat +datum=WGS84")

# Bin values into 10 bins
MOP_df <- Rasts_to_df1(MOP, ext_world, prj_world) %>%
  Bin_pres2(.) %>%
  drop_na()

# MOP plot
cols2 <- c("navyblue", cols)
cols_bw <- scale_fill_gradient(low = "black", high = "gray90")
MOP.p <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
  geom_tile(data = MOP_df, aes(x = x, y = y, fill = value_bin)) +
  #scale_fill_manual(values = cols, name = "MOP index") +
  scale_fill_manual(values = viridis::magma(10), name = "MOP index") +
  geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
  mytheme 
ggsave(MOP.p, file = here("Final_figures", "MOP_map.png"),
       width = 7, height = 4, units = c('in'), dpi=300)  
knitr::plot_crop(here("Final_figures", "MOP_map.png"))