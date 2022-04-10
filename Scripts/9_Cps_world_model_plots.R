## Script to produce plots depicting climate suitability and the potential 
## distribution for Calonectria pseudonaviculata at a global scale using outputs
## from CLIMEX model and ensemble correlative model.

# Libraries
pkgs <- c("raster", "tidyverse", "knitr", "maptools", "cowplot","sf",
          "here", "openxlsx", "ggalt", "rnaturalearth", "dismo")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("scripts", "Cps_model_functions.R"))

# Output directory
out_PCA <- here("ENMTML", "Outfiles", "run_PCA_4algs_kfold_prev1_03-29-2022")
outdir <- out_PCA

# All output files
all_fls <- list.files(outdir, 
                      pattern = glob2rx("*calonectria_pseudonaviculata*.tif$*"),
                      recursive = TRUE, full.names = TRUE)

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

## Plot themes ----

# Base theme
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

# Legend for MESS and MOP
world_sim_lgd <- theme(legend.position = c(0.15, 0.3),
                       legend.key.height = unit(0.65,"line"),
                       legend.key.width = unit(0.5, "line"),
                       legend.title = element_text(size = 8.5, face = "bold"),
                       legend.text=element_text(size = 8.5))

# Presence maps
world_pres_lgd <- theme(legend.position = c(0.19, 0.2),
                        legend.background = element_rect(fill = "white"),
                        legend.box.background = element_rect(fill = "white", color = "white"),
                        legend.key =  element_rect(fill = "white"),
                        legend.key.width = unit(0.6,"line"), 
                        legend.key.height = unit(0.6,"line"),
                        legend.title = element_text(size = 8, face = "bold"),
                        legend.text=element_text(size = 8))

## Analysis/plots: MESS and MOP ----

# Compute multivariate environmental similarity surface (MESS) as described
# by Elith et al. 2010.
# Environmental layers
env_vars <- stack(list.files(
  here("ENMTML", "All_vars", "Predictors", "Projection_PCA", "World"),
  #here("ENMTML", "Sub_vars", "Projection", "World"), 
  full.names = TRUE))

# Reference points (records used for model fitting)
recs_eur <- read.table(here(outdir, "Occurrences_Cleaned.txt"), 
                       sep = "\t", header = TRUE) %>%
  dplyr::select(x, y) 

# Extract environmental data for points and conduct MESS analysis
# Binning data as factors gives a nicer looking plot
refpt <- raster::extract(env_vars, as.matrix(recs_eur))
mess_out <- dismo::mess(env_vars, refpt, full = FALSE) 
mess_df <- mess_out %>%
  Rasts_to_df1(., ext_world, prj_world) %>%
  mutate(value = as.numeric(ifelse(value == Inf, NA, value))) %>%
  drop_na() %>%
  mutate(value_bin = case_when(value > -800 & value <= -150 ~ "-800 to -150",
                               value > -150 & value <= -100 ~ "-149 to -100",
                               value > -100 & value <= -50 ~ "-99 to -50",
                               value > -50 & value < 0 ~ "-49 to 0",
                               value >= 0 ~ ">0"))
# mutate(value_bin = case_when(value > -400 & value <= -300 ~ "-400 to -300",
#                              value > -300 & value <= -200 ~ "-299 to -200",
#                              value > -200 & value <= -100 ~ "-199 to -100",
#                              value > -100 & value < 0 ~ "-99 to 0",
#                              value >= 0 ~ "> 0"))
mess_df$value_bin <- factor(mess_df$value_bin,
                            levels = unique(mess_df$value_bin[order(mess_df$value)]))

# MESS plot
#mess_lower <- rev(colorRampPalette(c("#7AD151FF", "#28788EFF", "#440154FF"))(4))
#mess_cols <- c(mess_lower, "#FFFF65", "#FFFFCC", "#FFFF65")
##cols <-  rev(colorRampPalette(c("lemonchiffon", "#3CBB75FF", "#440154FF"))(8))
#cols2 <- rev(c(c("#C7E8B3", "lightyellow"), colorRampPalette(c("#C7E8B3","#3DA070", "#440154FF" ))(5)))
mess_cols_magma <- rev(colorRampPalette(c("#FFFFCC", "#EFCC98", "#CA3C97", "#4B2991"))(5))
mess_cols_viridis <- rev(colorRampPalette(c("lightgoldenrodyellow", "palegoldenrod", "#DCE319FF", "#73D055FF", "#29AF7FFF", "#238A8DFF", "#404788FF"))(5))

mess.p <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
  geom_raster(data = mess_df, aes(x = x, y = y, fill = value_bin)) +
  scale_fill_manual(values = mess_cols_magma, name = "MESS index") +
  geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
  mytheme +
  world_sim_lgd
ggsave(mess.p, file = here(outdir, "MESS_map.png"),
       width = 7, height = 4, units = c('in'), dpi=300)

# MOP plot
# High MOP index values indicate high similarity between climates in 
# modeling training and projection areas
mop <- raster(here(outdir, "Projection", "World", 
                   "Extrapolation", "calonectria_pseudonaviculata_MOP.tif"))
crs(mop) <- CRS("+proj=longlat +datum=WGS84")

# Bin values into 10 bins
mop_df <- Rasts_to_df1(mop, ext_world, prj_world) %>%
  #Bin_pres2(.) %>%
  drop_na() %>%
  # mutate(value_bin = case_when(value >= 0 & value <= 0.84 ~ "0 to 0.84",
  #                              value > 0.84 & value <= 0.88 ~ "0.84 to 0.88",
  #                              value > 0.88 & value <= 0.92 ~ "0.88 to 0.92",
  #                              value > 0.92 & value <= 0.96 ~ "0.92 to 0.96",
  #                              value > 0.96 ~ "0.96 to 1"))
mutate(value_bin = case_when(value >= 0 & value <= 0.86 ~ "<0.86",
                           value > 0.86 & value <= 0.88 ~ "0.86 - 0.88",
                           value > 0.88 & value <= 0.9 ~ "0.88 - 0.90",
                           value > 0.9 & value <= 0.92 ~ "0.90 - 0.92",
                           value > 0.92 & value <= 0.94 ~ "0.92 - 0.94",
                           value > 0.94 & value <= 0.96 ~ "0.94 - 0.96",
                           value > 0.96 & value <= 0.98 ~ "0.96 - 0.98",
                           value > 0.98 ~ "0.98 - 1"))
mop_df$value_bin <- factor(mop_df$value_bin,
                           levels = unique(mop_df$value_bin[order(mop_df$value)]))

# Edit turbo color scales so last color isn't so dark
mop_cols <- rev(viridis::turbo(8))
mop_cols <- c(mop_cols[-8], "#482677FF")
#mop_cols1 <-  rev(colorRampPalette(c("#FFFFCC", "#3CBB75FF", "#5A2995"))(4))
#mop_cols <- rev(colorRampPalette(c("#FFFFCC", "#F79C79", "#5A2995"))(8))

# MOP plot
mop.p <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
  geom_raster(data = mop_df, aes(x = x, y = y, fill = value_bin)) +
  scale_fill_manual(values = mop_cols, name = "MOP index") +
  geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
  mytheme + 
  world_sim_lgd

# Save plots
ggsave(mop.p, file = here(outdir, "MOP_map2.png"),
       width = 7, height = 4, units = c('in'), dpi=300)
ggsave(mop.p, file = here("Final_figures", "MOP_map.png"),
       width = 7, height = 4, units = c('in'), dpi=300)  
knitr::plot_crop(here("Final_figures", "MOP_map.png"))

# Save combined plots
mess_mop.p <- plot_grid(mess.p, mop.p, 
                        ncol = 1, label_x = 0.05,
                        labels = c("(a)", "(b)"),                             
                        label_size = 11, hjust = -1, vjust = 1)
ggsave(mess_mop.p, file = here("Final_figures", "MESS_MOP_map.png"),
       width = 7, height = 6.2, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "MESS_MOP_map.png"))

## Plots: CLIMEX vs. correlative ensemble climate suitability model ----

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

# Ensemble probability scores to plot
ens_type <- c("PCA", "PCA_SUP", "W_MEAN")

ens_dfs <- map(ens_type, function(ens) {
  rast <- raster(here(outdir, "Projection", "World", "Ensemble", 
                      ens, "calonectria_pseudonaviculata.tif"))
  prob_df <- Rasts_to_df1(rast, ext_world, prj_world) %>%
    filter(complete.cases(value)) %>%
    filter(!(value < 0.1)) %>%
    Bin_pres(.) %>%
    mutate(value = factor(value_bin,
                          levels = unique(value_bin[order(value)]))) 
})

# Colors

# 9 bins
cols <- c("#313695","#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
"#EB8B55", "#C8453D", "#A50026")

# 9 bins but gray as < 0.2
#cols <- c("gray90", "#313695","#4575B4","#ABD9E9", "#FEF7B3", "#FDD992", "#FDBC71", "#D15E4B", "#A50026")

# CLIMEX plot (EI)
world_ei.p <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
  geom_raster(data = ei_dfs[[1]], aes(x = x, y = y, fill = value_bin)) +
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
    geom_raster(data = ens_df, aes(x = x, y = y, fill = value_bin)) +
    scale_fill_manual(values = cols, name = "Prob. of\noccurrence") +
    geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
    geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
    mytheme 
  ggsave(world_ens.p, 
         filename = here(outdir, outfl),
             width = 8, height = 4, units = c('in'), device = "png", dpi=300) 
  #return(world_ens.p)
  
  # Final plot for manuscript
  if (ens_type[i] == "W_MEAN") {
    Both_world.p <- plot_grid(world_ei.p, world_ens.p, 
                             ncol = 1, label_x = 0.05,
                             labels = c("(A) CLIMEX", "(B) Correlative (ensemble)"),                             
                             label_size = 11, hjust = 0, vjust = 1)
    ggsave(Both_world.p, file = here("Final_figures", "World_CLIMEX_Ensemble.png"),
           width = 8, height = 7, units = c('in'), dpi=300)
    
    # Crop white margins (can't remove using theme settings)
    knitr::plot_crop(here("Final_figures", "World_CLIMEX_Ensemble.png"))
  }

})

## Plots: individual correlative algorithms ----

# Algorithm types
types <- c("BRT", "GAU", "MXS", "RDF")

# Climate suitaiblity outputs
# Output files
proj_fls <- all_fls[grep("Projection", all_fls)]
proj_fls <- proj_fls[-grep("MAX_TSS|LPT|SORENSEN|MAX_KAPPA|JACCARD|Extrapolation|Ensemble", proj_fls)]
proj_dfs <- Rasts_to_df2(map(proj_fls, .f = raster), ext_world, prj_world)

# Convert rasters to data frames
proj_dfs2 <- map(proj_dfs, function(x) {
  df <- x %>% filter(complete.cases(value)) %>%
  filter(!(value < 0.1)) %>%
  Bin_pres(.) %>%
  mutate(value = factor(value_bin, levels = unique(value_bin[order(value)]))) 
})

# Plots
world_alg_plots <- map(1:length(proj_dfs2), function(i) {
  proj_df <- proj_dfs2[[i]]
  outfl <- paste0("World_", types[i], ".png")
  world_alg.p <- ggplot() + 
    geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
    geom_raster(data = proj_df, aes(x = x, y = y, fill = value)) +
    scale_fill_manual(values = cols, name = "Prob. of\noccurrence") +
    geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
    geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
    mytheme 
  ggsave(world_alg.p, 
         filename = here(outdir, outfl),
         width = 8, height = 4, units = c('in'), device = "png", dpi=300) 
})

# Presence map outputs
# Files
world_thres_fls <- all_fls[grep("MAX_TSS|LPT|MAX_KAPPA|SORENSEN|JACCARD", all_fls)]
world_thres_fls <- world_thres_fls[-grep("Algorithm|Ensemble", world_thres_fls)]

# Also plot using a suitability cut-off - ENMTML has a bug that prevents use of 
# the "SENSITIVITY" threshold, or something else is going on
proj_dfs_sens <- map(1:length(proj_dfs), function(i) {
  mutate(proj_dfs[[i]], value = factor(ifelse(value <  0.3, NA, 1))) 
})
names(proj_dfs_sens) <- types

# Create plots of outputs and return raster results as a formatted data frame
LPT_thres_dfs <- Pres_plots_algs("LPT", proj_dfs_sens, "world", ext_world, prj_world)
JACCARD_thres_dfs <- Pres_plots_algs("JACCARD", proj_dfs_sens, "world", ext_world, prj_world)
MAX_TSS_thres_dfs <- Pres_plots_algs("MAX_TSS", proj_dfs_sens, "world", ext_world, prj_world)
MAX_KAPPA_thres_dfs <- Pres_plots_algs("MAX_KAPPA", proj_dfs_sens, "world", ext_world, prj_world)
MAX_SORENSEN_thres_dfs <- Pres_plots_algs("SORENSEN", proj_dfs_sens, "world", ext_world, prj_world)
SENSITIVITY_thres_dfs <- Pres_plots_algs("SENSITIVITY", proj_dfs_sens, "world", ext_world, prj_world)

## Plots: presence maps across 4 correlative algorithms ----

#Create presence plots for final thresholds of interest
col_thres <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(4)
col_thres <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(3)
world_alg_pres_maxtss <- Alg_pres_plot(MAX_TSS_thres_dfs, world_l, world_p, NA,
                                   world_pres_lgd, size = 8, scale = FALSE, 
                                   outfl = "All_algs_World_MaxTSS_presence")
world_alg_pres_sens <- Alg_pres_plot(SENSITIVITY_thres_dfs, world_l, world_p, NA,
                                       world_pres_lgd, size = 8, scale = FALSE, 
                                     outfl = "All_algs_World_Sens0.35_presence_mask_-75")

