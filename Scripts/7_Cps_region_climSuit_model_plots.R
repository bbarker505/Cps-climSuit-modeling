## Script to produce plots depicting climate suitability and the potential
## distribution for Calonectria pseudonaviculata in Europe, western Asia, 
## and North America according to the CLIMEX model and ensemble correlative model. 

memory.limit(size = 50000)

# Libraries
pkgs <- c("raster", "tidyverse", "maptools", "knitr",
          "cowplot","sf", "here", "openxlsx", "ggalt", "rnaturalearth")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("Scripts", "Cps_model_functions.R"))

# Outputs
out_PCA <- here("ENMTML", "Outfiles", "run_PCA_4algs_kfold_prev1_03-29-2022")
outdir <- out_PCA
#out_PCA <- here("ENMTML", "Outfiles", "run_PCA_9-27-2021")
#outdir <- here("ENMTML", "Outfiles", "run_r75_03-24-2022")

## Get model outputs and occurrence records ----
# CLIMEX model
CLMX_mod <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI_World.tif"))
#CLMX_mod.ir <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI.ir_World.tif"))

# Correlative model - ensemble for Eurasia
ens_eur <- raster(here(outdir, "Ensemble", "W_MEAN", "calonectria_pseudonaviculata.tif"))
ens_conus <- raster(here(outdir, "Projection", "World", "Ensemble", "W_MEAN", "calonectria_pseudonaviculata.tif"))

# Correlative model - presence across algorithms
all_fls <- list.files(outdir, 
                      pattern = glob2rx("*calonectria_pseudonaviculata*.tif$*"),
                      recursive = TRUE, full.names = TRUE)
fls <- all_fls[-grep("Projection", all_fls)]

## Extents, records, projection definitions, and spatial features ----
sf_use_s2(FALSE) # Avoid "Error in s2_geography_from_wkb..."

# Extents and spatial features
# Eurasia
ext_eur <- c(xmin = -10.5, xmax = 57, ymin = 35.67, ymax = 71.3)
prj_eur <- CRS("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 
               +ellps=intl +units=m +no_defs")
eur_cntry_feats <- RegionCrop(type = "countries", ext = ext_eur, prj = prj_eur)
eur_cntry_p <- eur_cntry_feats[[1]] # Polygon feature
eur_cntry_l <- eur_cntry_feats[[2]] # Line features

# CONUS
ext_conus <- c(xmin = -129, xmax = -51, ymin= 24.9, ymax = 52)
prj_conus <- CRS("+init=epsg:5070")
conus_states_feats <- RegionCrop(type = "states", ext = ext_conus, prj = prj_conus)
conus_states_p <- conus_states_feats[[1]] # Polygon feature
conus_states_l <- conus_states_feats[[2]] # Line features

# State features (inc. Canada provinces) - all NA and then just CONUS
na_states <- ne_states(returnclass = "sf") %>%
  filter(geonunit %in% c("United States of America", "Canada"))
na_states_p <- st_transform(na_states, prj_world)
na_states_l <- st_cast(na_states, "MULTILINESTRING")

# World
ext_world <- c(xmin = -165,  xmax = 175, ymin= -59.47275, ymax = 83.6341)
prj_world <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
                 +datum=WGS84 +units=m +no_defs")

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
rm(world)

# Occurrence records
# Use models fit to region and subsetted occurrences used for fitting
recs_eur <- read.table(here(outdir, "Occurrences_Cleaned.txt"), 
                       sep = "\t", header = TRUE) 
recs_eur_sf <- recs_eur %>%
  st_as_sf(., coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84") %>%
  st_transform(., prj_eur)

# Need to use world projection for CONUS; plot all (non-duplicate) records (validation)
recs_conus <- read.csv(here("Records", "Subsampled", "Cps_locations_noDups_10min.csv"))%>%
  filter(Continent == "North America")
recs_conus_sf <- st_as_sf(recs_conus, coords = c("Longitude", "Latitude"), 
                          crs = "+proj=longlat +datum=WGS84") %>%
  st_transform(., prj_conus)

## Themes and colors for plots ----
mytheme <- theme(plot.margin = unit(c(t=0, b=0, l=0, r=0),"cm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_rect("white"), 
                 panel.border = element_blank(),
                 axis.title.x = element_blank(), axis.title.y = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(), axis.text.y=element_blank(), 
                 legend.text=element_text(size = 8.5),
                 legend.title = element_text(size = 8.5, face = "bold"),
                 #legend.position = c(0.15, 0.3),
                 legend.margin=margin(0.1,0.1,0,0.1, unit="cm"),
                 legend.background = element_rect(fill = "white"))

# Specifics for legends
eur_lgd <- theme(legend.key.width = unit(0.4,"line"), 
                 legend.key.height = unit(0.3,"line"),
                 legend.position = c(0.15, 0.81))
conus_lgd <- theme(legend.key.width = unit(0.55,"line"), 
                   legend.key.height = unit(0.65,"line"),
                   legend.title = element_text(size = 9, face = "bold"),
                   legend.position = c(0.9, 0.37))
world_lgd <- theme(legend.position = c(0.19, 0.2),
                        legend.background = element_rect(fill = "white"),
                        legend.box.background = element_rect(fill = "white", color = "white"),
                        legend.key =  element_rect(fill = "white"),
                        legend.key.width = unit(0.6,"line"), 
                        legend.key.height = unit(0.6,"line"),
                        legend.title = element_text(size = 8, face = "bold"),
                        legend.text=element_text(size = 8))
# Color scale
cols <- c("#313695","#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
              "#EB8B55", "#C8453D", "#A50026")
#cols <- colorRampPalette(c( "#313695", "#FEF7B3", "#A50026"))(7)
cols <- colorRampPalette(c("#313695", "#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
                   "#EB8B55", "#A50026"))(7)
#cols <- c("gray90", "#313695","#4575B4","#ABD9E9", "#FEF7B3", 
#          "#FDD992", "#FDBC71", "#D15E4B", "#A50026")

cols_thres <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(6)

## Plots: CLIMEX ----

# Make plots
CLMX_eur.p <- CLMX_suit_plots(CLMX_mod, ext_eur, prj_eur, eur_cntry_p, eur_cntry_l, eur_lgd, recs_eur_sf, 1)
#CLMX_eur_ir.p <- CLMX_suit_plots(CLMX_mod.ir, ext_eur, prj_eur, eur_cntry_p, eur_cntry_l, eur_lgd, recs_eur_sf, 1)
CLMX_conus.p <- CLMX_suit_plots(CLMX_mod, ext_conus, prj_conus, conus_states_p, conus_states_l, conus_lgd, recs_conus_sf, 1)
#CLMX_conus_ir.p <- CLMX_suit_plots(CLMX_mod.ir, ext_conus, prj_conus, conus_states_p, conus_states_l, conus_lgd, recs_conus_sf, 1)
CLMX_world.p <- CLMX_suit_plots(CLMX_mod, ext_world, prj_world, world_p, world_l, world_lgd, NA, 0) +
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10")

# Save world plot
ggsave(CLMX_world.p, file = here("Final_figures", "World_CLIMEX.png"), 
       width = 8, height = 4, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "World_CLIMEX.png"))

## MOP plots ----
# Need to mark areas where correlative models are extrapolated 
mop <- raster(here(outdir, "Projection", "World", 
                   "Extrapolation", "calonectria_pseudonaviculata_MOP.tif"))
crs(mop) <- CRS("+proj=longlat +datum=WGS84")

mop_df <- Rasts_to_df1(mop, ext_world, prj_world) %>% 
  drop_na() %>%
  mutate(mop = ifelse(value >= 0.9, 0, 1),
         x = as.integer(x), y = as.integer(y))  %>%
  select(-value)

mop_df2 <-  Rasts_to_df1(mop, ext_world, prj_world) %>% 
  drop_na() %>%
  mutate(value_bin = case_when(value >= 0 & value <= 0.86 ~ "<0.86",
                               value > 0.86 & value <= 0.88 ~ "0.86 - 0.88",
                               value > 0.88 & value <= 0.9 ~ "0.88 - 0.90",
                               value > 0.9 & value <= 0.92 ~ "0.90 - 0.92",
                               value > 0.92 & value <= 0.94 ~ "0.92 - 0.94",
                               value > 0.94 & value <= 0.96 ~ "0.94 - 0.96",
                               value > 0.96 & value <= 0.98 ~ "0.96 - 0.98",
                               value > 0.98 ~ "0.98 - 1"))
mop_df2$value_bin <- factor(mop_df2$value_bin,
                           levels = unique(mop_df2$value_bin[order(mop_df2$value)]))

# Edit turbo color scales so last color isn't so dark
mop_cols <- rev(viridis::turbo(8))
mop_cols <- c(mop_cols[-8], "#482677FF")

# MOP plot
mop.p <- ggplot() + 
  geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
  geom_raster(data = mop_df2, aes(x = x, y = y, fill = value_bin)) +
  scale_fill_manual(values = mop_cols, name = "MOP index") +
  geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
  mytheme +
  world_lgd +
  theme(legend.position = c(0.15, 0.3))

# Save plots
ggsave(mop.p, file = here(outdir, "MOP_map2.png"),
       width = 7, height = 4, units = c('in'), dpi=300)
ggsave(mop.p, file = here("Final_figures", "MOP_map.png"),
       width = 7, height = 4, units = c('in'), dpi=300)  
knitr::plot_crop(here("Final_figures", "MOP_map.png"))

rm(mop_df2, mop.p)

## Plots: climate suitability models for 4 correlative algorithms ----
alg_fls <- all_fls[grep("Algorithm", all_fls)]
alg_fls <- alg_fls[-grep("MAX_TSS|MAX_KAPPA|LPT|SENSITIVITY|JACCARD|SORENSEN", alg_fls)]

# For CONUS, need to crop world projection output files
# Also make plot for weighted mean (ensemble)
proj_fls <- all_fls[grep("Projection", all_fls)]
alg_fls <- proj_fls[-grep("MAX_TSS|LPT|SORENSEN|MAX_KAPPA|JACCARD|Extrapolation", proj_fls)]
alg_fls <- alg_fls[-grep("\\/PCA|\\/PCA_SUP", alg_fls)]
alg_fls <- c(alg_fls[1], alg_fls[3:5], alg_fls[2]) # Reorder

# Format rasters and convert to data frames
alg_outs.eur <- Rasts_to_df2(map(alg_fls, .f = raster), ext_eur, prj_eur)
alg_outs.conus <- Rasts_to_df2(map(alg_fls, .f = raster), ext_conus, prj_conus)
alg_outs.world <- Rasts_to_df2(map(alg_fls, .f = raster), ext_world, prj_world)

# Algorithm types
types <- c("BRT", "GAU", "MXS", "RDF", "Ensemble")
cols <- c("gray90", c(colorRampPalette(c("#313695", "#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
                           "#EB8B55", "#A50026"))(9)))
# Plots
eur_alg_suit_plots <- Alg_suit_plots(alg_outs.eur, types, eur_cntry_l, 
                                     eur_cntry_p, eur_lgd)
conus_alg_suit_plots <- Alg_suit_plots(alg_outs.conus, types, conus_states_l, 
                                       conus_states_p, conus_lgd)
world_alg_suit_plots <- Alg_suit_plots(alg_outs.world, types, world_l, 
                                       world_p, world_lgd)

# Create separate map for ensmble showing areas of extrapolation
ens_suit_msk <- alg_outs.world[[5]] %>%
  mutate(x = as.integer(x), y = as.integer(y)) %>%
  left_join(., mop_df, by = c("x", "y")) %>%
  filter(mop == 0)

ens_suit_extr <- alg_outs.world[[5]] %>%
  mutate(x = as.integer(x), y = as.integer(y)) %>%
  anti_join(., ens_suit_msk, by = c("x", "y")) %>%
  mutate(value = ifelse(value > 0, 1, 0)) 

world_ens.p <- world_alg_suit_plots[[5]] +
  geom_raster(data = ens_suit_extr, aes(x = x, y = y), fill = "gray50") +
  geom_sf(data = world_l, lwd = 0.1, color = "gray10") 

# Arrange plot of CLIMEX vs. ensemble model suitability
Both_suit_world.p <- plot_grid(CLMX_world.p, world_ens.p, 
                          ncol = 1, label_x = 0.05,
                          labels = c("(a) CLIMEX", "(b) Correlative (ensemble)"),                             
                          label_size = 11, hjust = -0.25, vjust = 1)
ggsave(Both_suit_world.p, file = here("Final_figures", "World_CLIMEX_Ensemble.png"),
       width = 8, height = 7, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "World_CLIMEX_Ensemble.png"))

# Arrange and save plots for 4 algorithms (supporting info)
labs1 <- c("(a) BRT", "(b) GAU", "(c) MXS", "(d) RDF")
All_algs_suit_eur.p <- plot_grid(eur_alg_suit_plots[[1]], eur_alg_suit_plots[[2]], 
                       eur_alg_suit_plots[[3]], eur_alg_suit_plots[[4]],
                       ncol = 2, nrow = 2, labels = labs1,
                       label_size = 12, hjust = 0, vjust = 1)
ggsave(All_algs_suit_eur.p, file= paste0(outdir, "/All_algs_suit_Eurasia_noPts.png"),
       width = 7, height = 6.7, units = c('in'), dpi=300)

All_algs_suit_conus.p <- plot_grid(conus_alg_suit_plots[[1]], conus_alg_suit_plots[[2]], 
                         conus_alg_suit_plots[[3]], conus_alg_suit_plots[[4]],
                         ncol = 2, nrow = 2, labels = labs1,
                         label_size = 12, hjust = 0, vjust = 1.1)
ggsave(All_algs_suit_conus.p, file= paste0(outdir, "/All_algs_suit_CONUS_noPts.png"),
       width = 7, height = 5, units = c('in'), dpi=300)

# Create a figure with CLIMEX plot, 4 algorithms, and the ensemble corr. model
labs2 <- c("(a) CLIMEX", "(b) BRT", "(c) GAU", "(d) MXS", "(e) RDF", "(f) ENS-W")

# Europe - add points to last plot
# eur_alg_suit_plots[[5]] <- eur_alg_suit_plots[[5]] +
#   geom_sf(data=recs_eur_sf, shape=21, size=1, fill= "purple", color = "white")
All_models_suit_eur.p <- plot_grid(CLMX_eur.p, eur_alg_suit_plots[[1]], 
                                 eur_alg_suit_plots[[2]], eur_alg_suit_plots[[3]], 
                                 eur_alg_suit_plots[[4]], eur_alg_suit_plots[[5]],
                                 ncol = 2, nrow = 3, labels = labs2,
                                 label_size = 12, hjust = 0, vjust = 1)
ggsave(All_models_suit_eur.p, file= paste0(outdir, "/All_models_climSuit_Eurasia_noPts.png"),
       width = 7, height = 9.5, units = c('in'), dpi=300)

# CONUS - add points to last plot
# conus_alg_suit_plots[[5]] <- conus_alg_suit_plots[[5]] +
#   geom_sf(data=recs_conus_sf, shape=21, size=0.85, fill= "purple", color = "white")
All_models_suit_conus.p <- plot_grid(CLMX_conus.p, conus_alg_suit_plots[[1]], 
                                   conus_alg_suit_plots[[2]], conus_alg_suit_plots[[3]],
                                   conus_alg_suit_plots[[4]], conus_alg_suit_plots[[5]],
                                   ncol = 2, nrow = 3, labels = labs2,
                                   label_size = 12, hjust = 0, vjust = 1.1)
ggsave(All_models_suit_conus.p, file= paste0(outdir, "/All_models_suit_CONUS_noPts.png"),
       width = 7, height = 7.5, units = c('in'), dpi=300)

# Save to "Final_figures" folder
ggsave(All_models_suit_eur.p, file = here("Final_figures", "All_models_suit_Eurasia.png"),
       width = 7, height = 9.5, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "All_models_suit_Eurasia.png"))
ggsave(All_models_suit_conus.p, file= here("Final_figures", "All_models_suit_CONUS.png"),
       width = 7, height = 7.5, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "All_models_suit_CONUS.png"))

rm(CLMX_eur.p, CLMX_conus.p, CLMX_world.p, eur_alg_suit_plots, 
   conus_alg_suit_plots, world_alg_suit_plots)

## Plots: presence maps across 4 correlative algorithms ----

# Choose threshold to use (if sensitivity, need to manually produce presence maps)
thres <- "SENSITIVITY"

if (thres == "SENSITIVITY") {
  thres_fls <- alg_fls
  thres_rasts <- map(thres_fls, raster)
  thres_rasts <- stack(thres_rasts)
  thres_rasts[thres_rasts >= 0.3] <- 1
  thres_rasts[thres_rasts < 0.3] <- 0
  thres_rasts <- as.list(thres_rasts)
} else {
  thres_fls <- fls[grep("MAX_TSS", fls)]
  thres_fls <- thres_fls[-grep("Ensemble", thres_fls)]
  thres_rasts <- map(thres_fls, raster)
}

# Convert to data frames
thres_dfs.eur <- Rasts_to_df2(thres_rasts[-5], ext = ext_eur, prj = prj_eur)
thres_dfs.conus <- Rasts_to_df2(thres_rasts[-5], ext = ext_conus, prj = prj_conus)
thres_dfs.world <- Rasts_to_df2(thres_rasts[-5], ext = ext_world, prj = prj_world)

# Create version of figures that uses ensemble results instead of summing across algorithms,
# for use in comparison to CLIMEX results below
thres_dfs.eur2 <- Rasts_to_df2(thres_rasts[5], ext = ext_eur, prj = prj_eur)
thres_dfs.conus2 <- Rasts_to_df2(thres_rasts[5], ext = ext_conus, prj = prj_conus)
thres_dfs.world2 <- Rasts_to_df2(thres_rasts[5], ext = ext_world, prj = prj_world)

# Plots
# Apply function
eur_alg_pres_plot <- Alg_pres_plot(thres_dfs.eur, eur_cntry_l, eur_cntry_p, recs_eur_sf,
                                "eur", 1, eur_lgd, scale = TRUE, size = 6, "All_algs_Eurasia_presence_0.3")
conus_alg_pres_plot <- Alg_pres_plot(thres_dfs.conus, conus_states_l, conus_states_p, recs_conus_sf,
                                  "conus", 1, conus_lgd, scale = TRUE, size = 6, "All_algs_CONUS_presence_0.3")
world_alg_pres_plot <- Alg_pres_plot(thres_dfs.world, world_l, world_p, NA,
                                     "world", 0, world_lgd, scale = TRUE, size = 6, "All_algs_World_presence_0.3") +
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10")
world_alg_presEns_plot <- Alg_pres_plot(thres_dfs.world2, world_l, world_p, NA,
                                     "world", 0, world_lgd, scale = TRUE, size = 6, "All_algs_World_presence_0.3") +
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10")
ggsave(world_alg_presEns_plot, file = here(outdir, "World_ensemble_presence.png"), device = "png")

# Combine Eurasia and CONUS and save to final figures folder
eur_alg_pres_plot <- eur_alg_pres_plot + 
  theme(legend.position = c(0.13, 0.78))
conus_alg_pres_plot <- conus_alg_pres_plot + 
  theme(legend.position = "none")
both_regions_alg_pres_plot <- plot_grid(eur_alg_pres_plot, conus_alg_pres_plot, 
                                   nrow = 1, labels = c("(a)", "(b)"),
                                   label_size = 12, hjust = -0.5, vjust = 3.5)
ggsave(both_regions_alg_pres_plot, file= here("Final_figures", "All_algs_pres_Eur_v_CONUS_0.3.png"),
       width = 8.5, height = 3.7, units = c('in'), device = "png", dpi=300)
knitr::plot_crop(here("Final_figures", "All_algs_pres_Eur_v_CONUS_0.3.png"))

# Save world map
ggsave(world_alg_pres_plot, file= here("Final_figures", "All_algs_pres_World_0.3.png"),
       width = 8, height = 4, units = c('in'), device = "png", dpi=300)
knitr::plot_crop(here("Final_figures", "All_algs_pres_World_0.3.png"))

## Plots: EI > 10 vs. consensus of 4 correlative models ----

# Algorithm types
types <- c("BRT", "GAU", "MXS", "RDF")

# CLIMEX potential distribution
ei_gt10_r <- CLMX_mod >= 10 
ei_gt10_r <- resample(ei_gt10_r, mop, method = "ngb") # Same cell size as ensemble
rm(CLMX_mod)

# Convert climate suitability surfaces to binary presence maps
eur_all_pres_plot <- Corr_CLMX_plot(thres_dfs.eur, "europe", ext_eur, prj_eur, 
                                    eur_cntry_p, eur_cntry_l, eur_lgd)
eur_ens_pres_plot <- Corr_CLMX_plot(thres_dfs.eur2, "europe_ens", ext_eur, prj_eur, 
                                    eur_cntry_p, eur_cntry_l, eur_lgd)

conus_all_pres_plot <- Corr_CLMX_plot(thres_dfs.conus, "conus", ext_conus, prj_conus, 
                                      conus_states_p, conus_states_l, conus_lgd) +
  theme(legend.text = element_text(size = 7.5), legend.position = c(0.88, 0.37))
conus_ens_pres_plot <- Corr_CLMX_plot(thres_dfs.conus2, "conus_ens", ext_conus, prj_conus, 
                                      conus_states_p, conus_states_l, conus_lgd) +
  theme(legend.text = element_text(size = 7.5), legend.position = c(0.88, 0.37))

world_all_pres_plot <- Corr_CLMX_plot(thres_dfs.world, "world", ext_world, 
                                      prj_world, world_p, world_l, world_lgd)  +
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") 
world_ens_pres_plot <- Corr_CLMX_plot(thres_dfs.world2, "world_ens", ext_world, 
                                      prj_world, world_p, world_l, world_lgd)  +
  geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") 

rm(list = ls(pattern = "thres_dfs"))

# Combine plots of overlap of 4 correlative models with final consensus 
# presence maps (CLIMEX plus Correlative consensus) for Europe and CONUS
both_regions_all_pres_plot <- plot_grid(eur_alg_pres_plot, conus_alg_pres_plot, 
                                        eur_all_pres_plot, conus_all_pres_plot,
                                        nrow = 2, labels = c("(a) Europe and western Asia", "(b) North America", "", ""),
                                        label_size = 12, hjust = 0, vjust = 1)
ggsave(both_regions_all_pres_plot, file= here("Final_figures", "All_models_pres_Eur_v_CONUS_0.3.png"),
       width = 8, height = 6.2, units = c('in'), device = "png", dpi=300)
knitr::plot_crop(here("Final_figures", "All_models_pres_Eur_v_CONUS_0.3.png"))

both_regions_ens_pres_plot <- plot_grid(eur_alg_pres_plot, conus_alg_pres_plot, 
                                        eur_ens_pres_plot, conus_ens_pres_plot,
                                        nrow = 2, labels = c("(a) Europe and western Asia", "(b) North America", "", ""),
                                        label_size = 12, hjust = 0, vjust = 1)
ggsave(both_regions_ens_pres_plot, file= here("Final_figures", "All_models_Ensemble_pres_Eur_v_CONUS_0.3.png"),
       width = 8, height = 6.2, units = c('in'), device = "png", dpi=300)
knitr::plot_crop(here("Final_figures", "All_models_Ensemble_pres_Eur_v_CONUS_0.3.png"))

# Save world plot
ggsave(world_all_pres_plot , filename = here(outdir, "World_CLIMEX_v_Corr_presence_0.3.png"),
       width = 8.5, height = 4, units = c('in'), device = "png", dpi=300) 
ggsave(world_all_pres_plot, 
       filename = here("Final_figures", "World_CLIMEX_v_Corr_presence.png"),
       width = 8.5, height = 4, units = c('in'), device = "png", dpi=300) 
knitr::plot_crop(here("Final_figures", "World_CLIMEX_v_Corr_presence.png"))

ggsave(world_ens_pres_plot , filename = here(outdir, "World_CLIMEX_v_Corr_Ensemble_presence_0.3.png"),
       width = 8.5, height = 4, units = c('in'), device = "png", dpi=300) 
ggsave(world_ens_pres_plot, 
       filename = here("Final_figures", "World_CLIMEX_v_Corr_Ensemble_presence.png"),
       width = 8.5, height = 4, units = c('in'), device = "png", dpi=300) 
knitr::plot_crop(here("Final_figures", "World_CLIMEX_v_Corr_Ensemble_presence.png"))

#rm(list = setdiff(ls(), "out_PCA"))

