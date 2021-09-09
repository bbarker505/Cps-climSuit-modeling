## Script to produce plots depicting climate suitability and the potential
## distribution for Calonectria pseudonaviculata in Europe, western Asia, 
## and North America according to the CLIMEX model and ensemble correlative model. 

# Libraries
pkgs <- c("sp", "rgdal", "raster", "tidyverse", "maptools","RColorBrewer", "knitr",
          "cowplot","sf","spData", "here", "openxlsx", "ggalt", "rnaturalearth")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("Scripts", "Cps_model_functions.R"))

## Get model outputs and occurrence records ----
# CLIMEX model
CLMX_mod <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI_World.tif"))
CLMX_mod.ir <- raster(here("CLIMEX", "Final_outfls", "TIFs", "EI.ir_World.tif"))

# Correlative model - ensemble
#outdir <- here("ENMTML", "Outfiles", "run_PCA_08-31-2021")
ens_mod <- raster(paste0(outdir, "/Ensemble/W_MEAN/calonectria_pseudonaviculata.tif" ))

# Correlative model - presence (Max TSS) across algorithms
all_fls <- list.files(outdir, 
                      pattern = glob2rx("*calonectria_pseudonaviculata*.tif$*"),
                      recursive = TRUE, full.names = TRUE)
fls <- all_fls[-grep("Projection", all_fls)]
fls <- fls[grep("MAX_TSS", fls)]
fls <- fls[-grep("Ensemble", fls)]
thres_outs <- map(fls, raster)

# Occurrence records
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
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
conus_states_feats <- RegionCrop(type = "states", ext = ext_conus, prj = prj_conus)
conus_states_p <- conus_states_feats[[1]] # Polygon feature
conus_states_l <- conus_states_feats[[2]] # Line features

## Themes and colors for plots ----
mytheme <- theme(plot.margin = unit(c(t=0.2, b=0, l=0, r=0),"cm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), panel.border = element_blank(),
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

# Color scale
cols <- c("#313695","#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
              "#EB8B55", "#C8453D", "#A50026")
cols_thres <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(6)

## Crop and project models and records for each region ----
mods <- list(CLMX_mod, CLMX_mod.ir, ens_mod)

# Eurasia
mods_eur <- Rasts_to_df2(mods, ext_eur, prj_eur)
thres_eur <- Rasts_to_df2(thres_outs, ext_eur, prj_eur)
recs_eur_sf <- recs_sf %>% 
  filter(Continent %in% c("Europe", "Asia")) %>%
  st_transform(., prj_eur)

# CONUS
mods_conus <- Rasts_to_df2(mods, ext_conus, prj_conus)
thres_conus <- Rasts_to_df2(thres_outs, ext_conus, prj_conus)
recs_conus_sf <- recs_sf %>%
  filter(Continent == "North America") %>%
  st_transform(., CRS("+init=epsg:5070"))

## Plots for CLIMEX and ensemble model ----
types <- c("CLIMEX", "CLIMEX.ir", "Corr")
regions <- c("Eurasia", "CONUS")
all_mod_lst <- list(mods_eur, mods_conus) # List of lists
poly_lst <- list(eur_cntry_p, conus_states_p)
ln_lst <- list(eur_cntry_l, conus_states_l)
lgd_theme_lst <- list(eur_lgd, conus_lgd)
occ_recs <- list(recs_eur_sf, recs_conus_sf)

for (i in 1:2) {
  #print(i)
  region_mods <- all_mod_lst[[i]] # Models for region
  #names(region_mods) <- types # CLIMEX and Corr model for each region
  poly <- poly_lst[[i]]
  ln <- ln_lst[[i]]
  recs <- occ_recs[[i]]
  lgd_theme <- lgd_theme_lst[[i]]
  region <- regions[i]
  
  for (j in 1:length(types)) {
    
    type <- types[j]
    mod_df <- region_mods[[j]]
    out_plot_nam <- paste0(region, "_", type, ".p")
    
    if (type %in% c("CLIMEX", "CLIMEX.ir")) {
      
      # Remove 0 values and bin EI values
      mod_df <- mod_df %>%
        filter(value > 0) %>%
        Bin_CLMX("EI", .)
      mod_df <- mod_df[[3]]
      lgd_titl <- "Ecoclimatic index"
      
    } else if (type == "Corr") {
      
      # Filter out values under 0.1 and bin suitability scores
      mod_df <- mod_df %>% 
        filter(!(value < 0.1)) %>%
        Bin_pres(.)
      lgd_titl <- "Prob. of occurrence"
        
      }
    
    # Order factor levels
    mod_df$value_bin <- factor(mod_df$value_bin,
                            levels = unique(mod_df$value_bin[order(mod_df$value)]))
    # Plot
    p <- ggplot() + 
      geom_sf(data = poly, color="gray20",  fill = "gray85", lwd = 0.3) +
      geom_tile(data = mod_df, 
                aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols, drop = FALSE, na.value = "transparent") +
      geom_sf(data = ln, lwd = 0.2, color = "gray10") +
      geom_sf(data=recs, shape=21, size=1, fill= "black", color = "white") +
      mytheme +
      theme(legend.title = element_text(angle = 90, size = 8, face = "bold")) +
      lgd_theme_lst[[i]] +
      guides(fill = guide_legend(title.position = "left", title.hjust = 0.5,
                                 title = lgd_titl))
    
    assign(out_plot_nam, p)
    
  }

}

## Put north arrow and scale on first plot for each region

# First plot for Eurasia
Eurasia_CLIMEX.ir.p <- Eurasia_CLIMEX.ir.p +
  ggspatial::annotation_north_arrow(
    location =  "tr",
    height = unit(0.75, "cm"),
    width = unit(0.75, "cm"),
    pad_x = unit(2, "cm")
  ) +
  
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    pad_x = unit(1, "cm"),
    pad_y = unit(0.5, "cm")
  )

# First plot for CONUS
CONUS_CLIMEX.ir.p <- CONUS_CLIMEX.ir.p +
  ggspatial::annotation_scale(
  location = "br",
  bar_cols = c("grey60", "white"),
  text_cex = 0.6
  )
# )  + 
#   ggspatial::annotation_north_arrow(
#     location =  "bl",
#     height = unit(0.75, "cm"),
#     width = unit(0.75, "cm"),
#     pad_y = unit(0.5, "cm")
#   )

## Plots: compare correlative model presence maps across algorithms ----

# Color table
col_thres <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(6)

# Inputs
thres_out_lst <- list(thres_eur, thres_conus)
out_plot_lst2 <- c("Eurasia_presence.p", "CONUS_presence.p")

# Plot for Eurasia and CONUS
for (i in 1:2) {
  
  # Add presence values (=1) across the 6 algorithms to assess agreement
  df <- Reduce(function(...) 
    merge(..., by = c("x", "y")), thres_out_lst[[i]]) %>%
    data.frame(.[1], value = rowSums(.[3:8])) %>% 
    filter(!(value == 0)) %>%
    dplyr::select(x, y, value)
  df$value <- factor(df$value, levels = c("1", "2", "3", "4", "5", "6"))
  
  # Plot
  p <- ggplot() + 
    geom_sf(data = poly_lst[[i]], color="gray20", fill = "gray85", lwd = 0.3) +
    geom_tile(data = df, 
              aes(x = x, y = y, fill = value)) +
    #scale_fill_viridis_d(option = "plasma", direction = -1) +
    scale_fill_manual(values = col_thres,
                      name = c("No. of models\nwith presence")) +
    geom_sf(data = ln_lst[[i]], lwd = 0.2, color = "gray10") +
    geom_sf(data=occ_recs[[i]], shape=21, size=1, fill= "black", color = "white") +
    mytheme  +
    lgd_theme_lst[[i]]  +
    theme(legend.title = element_text(angle = 0, size = 9, face = "bold"),
          legend.text = element_text(angle = 0, size = 9),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank())    
  
  assign(out_plot_lst2[i], p)
  
}

# Reduce font size in presence plot for CONUS
CONUS_presence.p <- CONUS_presence.p +
  theme(legend.text = element_text(size = 8), 
        legend.title = element_text(size = 8))

## Plots: potential distribution (CLIMEX vs. correlative ensemble model) ----
ens_thres <- raster(
  paste0(outdir, "/Ensemble/PCA_SUP/MAX_TSS/calonectria_pseudonaviculata.tif")
  )

# Plots of EI > 10 vs. ensemble model Max TSS threshold maps

# Format results 
ei_gt10 <- CLMX_mod.ir >= 10 # CLIMEX potential distribution
ens_thres[ens_thres <= 0] <- NA # Change 0 values to NA
ens_thres <- resample(ens_thres, ei_gt10) # Need same grid cell size as CLIMEX
ens_thres[!is.na(ens_thres)] <- 1 # Resampling alters some cells - change back to 1

# Eurasia
ens_thres_eur <- Rasts_to_df2(list(ei_gt10, ens_thres), ext_eur, prj_eur)
ens_thres_eur <- map(ens_thres_eur, function(x) {
  x$value <- as.integer(x$value)
  return(x)
})

# CONUS
ens_thres_conus <- Rasts_to_df2(list(ei_gt10, ens_thres), ext_conus, prj_conus)
ens_thres_conus <- map(ens_thres_conus, function(x) {
  x$value <- as.integer(x$value)
  return(x)
})

# Make the plots

# Color table
cols_thres <- c("Both" = "purple", "CLIMEX" = "red2", "Correlative" = "blue2")

# Inputs
regions <- c("Eurasia", "CONUS")
all_thres_lst <- list(ens_thres_eur, ens_thres_conus) # List of lists
poly_lst <- list(eur_cntry_p, conus_states_p)
ln_lst <- list(eur_cntry_l, conus_states_l)
lgd_theme_lst <- list(eur_lgd, conus_lgd)
all_plots <- list()
out_plot_lst3 <- c("Eurasia_potDistro.p", "CONUS_potDistro.p")

# Plots
for (i in 1:2) {
  
  # Merge data sets and create new factors
  df <- Reduce(function(...) 
    merge(..., by = c("x", "y"), all = TRUE), all_thres_lst[[i]]) %>%
    data.frame(.[1], value = rowSums(.[3:4])) %>% 
    replace(is.na(.), 0) %>%
    rename("CLIMEX" = value.x, "Corr" = value.y, "Both" = value) %>%
    mutate(
      value = ifelse(CLIMEX == 1 & Corr == 0, "CLIMEX",
                     ifelse(CLIMEX == 0 & Corr == 1, "Correlative",
                            ifelse(CLIMEX == 0 & Corr == 0, NA,
                                   "Both")))) %>%
    filter(!is.na(value))
  df$value <- factor(df$value, levels = c("Both", "CLIMEX", "Correlative"))
  
  # Plot
  p <- ggplot() + 
    geom_sf(data = poly_lst[[i]], color="gray20", fill = "gray85", lwd = 0.3) +
    geom_tile(data = df, aes(x = x, y = y, fill = value)) +
    #scale_fill_viridis_d(option = "plasma", direction = -1) +
    scale_fill_manual(values = cols_thres,
                      name = c("Model type")) +
    geom_sf(data = ln_lst[[i]], lwd = 0.2, color = "gray10") +
    #geom_sf(data = occ_recs[[i]], shape=21, size=1, fill= "black", color = "white") +
    mytheme  +
    lgd_theme_lst[[i]]  +
    theme(legend.title = element_text(angle = 0, size = 9, face = "bold"),
          legend.text = element_text(angle = 0, size = 9),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill = "white", color = "white"),
          legend.key =  element_rect(fill = "white"))
  
  assign(out_plot_lst3[i], p)
  
}

# Arrange and save climate suitability and potential distribution plots 
# for figures in main manuscript
all_eur_plots <- plot_grid(Eurasia_CLIMEX.ir.p, Eurasia_CLIMEX.p, 
                           Eurasia_Corr.p, Eurasia_potDistro.p,
                           ncol = 2,
                           labels = c("(a) CLIMEX model with irrigation", 
                                      "(b) CLIMEX model without irrigation",
                                      "(c) Correlative model (ensemble)",
                                      "(d) Potential distribution"),
                           label_size = 12, hjust = 0 , vjust = 1)
ggsave(all_eur_plots, file= here("Final_figures", "CLIMEX_v_Corr_Eurasia.png"),
       width = 8, height = 6.7, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "CLIMEX_v_Corr_Eurasia.png"))

all_conus_plots <- plot_grid(CONUS_CLIMEX.ir.p, CONUS_CLIMEX.p, 
                           CONUS_Corr.p, CONUS_potDistro.p,
                           ncol = 2,
                           labels = c("(a) CLIMEX model with irrigation", 
                                      "(b) CLIMEX model without irrigation",
                                      "(c) Correlative model (ensemble)",
                                      "(d) Potential distribution"),
                           label_size = 12, hjust = 0 , vjust = 1.1)
ggsave(all_conus_plots, file= here("Final_figures", "CLIMEX_v_Corr_CONUS.png"),
       width = 8, height = 5.5, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "CLIMEX_v_Corr_CONUS.png"))

# Plots: ensemble model vs. presence predictions for each algorithm ----
Eurasia_Corr.p2 = Eurasia_Corr.p + 
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_cex = 0.6
  )

CONUS_Corr.p2 = CONUS_Corr.p + 
  ggspatial::annotation_scale(
    location = "br",
    bar_cols = c("grey60", "white"),
    text_cex = 0.6
  )

# Arrange and save plots for supporting info
ens_presence_plots <- plot_grid(Eurasia_Corr.p2, Eurasia_presence.p, 
                          CONUS_Corr.p2, CONUS_presence.p,  
                             ncol = 2,
                             labels = c("(a)", "(b)", "(c)", "(d)"),
                             label_size = 12, hjust = 0 , vjust = 1.1)
ggsave(ens_presence_plots, file= here("Final_figures", "Ensemble_v_AlgsPres.png"),
       width = 8, height = 6, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "CLIMEX_v_Corr_CONUS.png"))


# Plots: climate suitability models for 6 correlative algorithms ----
alg_fls <- all_fls[grep("Algorithm", all_fls)]
alg_fls <- alg_fls[-grep("MAX_TSS", alg_fls)]

# Format rasters and convert to data frames
alg_outs.eur <- Rasts_to_df2(map(alg_fls, .f = raster), ext_eur, prj_eur)
alg_outs.conus <- Rasts_to_df2(map(alg_fls, .f = raster), ext_conus, prj_conus)

# Inputs
types <- c("BRT", "GAM", "GAU", "MXS", "RDF", "SVM")
alg_out_lst <- list(alg_outs.eur, alg_outs.conus)
poly_lst <- list(eur_cntry_p, conus_states_p)
ln_lst <- list(eur_cntry_l, conus_states_l)
lgd_theme_lst <- list(eur_lgd, conus_lgd)
out_plot_lst <- c("eur_plots", "conus_plots")

# Plots
for (i in 1:2) {
  
  alg_outs <- alg_out_lst[[i]]
  names(alg_outs) <- types
  poly <- poly_lst[[i]]
  ln <- ln_lst[[i]]
  lgd_theme <- lgd_theme_lst[[i]]
  
  plots_lst <- map(types, function(type) {
    
    df <- alg_outs[names(alg_outs) == type] %>%
      do.call(rbind.data.frame, .) %>% 
      filter(!(value < 0.1))
    
    # Format results for plotting data in categories (bin probabilities)
    df2 <- Bin_pres(df)
    df2$value_bin <- factor(df2$value_bin,
                            levels = unique(df2$value_bin[order(df2$value)]))
    # Plot
    p <- ggplot() + 
      geom_sf(data = poly, color="gray20",  fill = "gray85", lwd = 0.3) +
      geom_tile(data = df2, 
                aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols, drop = FALSE,
                        na.value = "transparent") +
      geom_sf(data = ln, lwd = 0.2, color = "gray10") +
      mytheme +
      theme(legend.title = element_text(angle = 90, size = 8, face = "bold")) +
      lgd_theme_lst[[i]] +
      guides(fill = guide_legend(title.position = "left", title.hjust = 0.5,
                                 title = "Prob. of occurrence"))
    
  })
  
  assign(out_plot_lst[i], plots_lst)
  
}

# Add north arrows to BRT and Ensemble plots for each region
eur_plots2 <- map(list(eur_plots[[1]], eur_plots[[6]]), function(p) {
  p2 <- p + 
    ggspatial::annotation_north_arrow(
      location =  "tr",
      height = unit(0.75, "cm"),
      width = unit(0.75, "cm"),
      pad_x = unit(2, "cm")
    ) +
    
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("grey60", "white"),
      pad_x = unit(1, "cm"),
      pad_y = unit(0.5, "cm")
    ) 
  
})

conus_plots2 <- map(list(conus_plots[[1]], conus_plots[[6]]), function(p) {
  
  p2 <- p + ggspatial::annotation_scale(
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
  
})

# Arrange and save plots for the 6 algorithms (supporting info)
labs <- c("(a) BRT", "(b) GAM", "(c) GAU", "(d) MXS", 
          "(e) RDF", "(f) SVM")
All_eur.p <- plot_grid(eur_plots2[[1]], 
                       eur_plots[[2]], eur_plots[[3]], 
                       eur_plots[[4]], eur_plots[[5]], eur_plots[[6]],
                       ncol = 2, nrow = 3,
                       labels = labs,
                       label_size = 12, hjust = 0, vjust = 1)
ggsave(All_eur.p, file= paste0(outdir, "/All_algorithms_Eurasia.png"),
       width = 8, height = 10, units = c('in'), dpi=300)

All_conus.p <- plot_grid(conus_plots2[[1]], conus_plots[[2]], conus_plots[[3]], 
                         conus_plots[[4]], conus_plots[[5]], conus_plots[[6]],
                         ncol = 2, nrow = 3,
                         labels = labs,
                         label_size = 12, hjust = 0, vjust = 1)
ggsave(All_conus.p, file= paste0(outdir, "/All_algorithms_CONUS.png"),
       width = 8.5, height = 7.5, units = c('in'), dpi=300)

# Save to "Final_figures" folder
ggsave(All_eur.p, file = here("Final_figures", "All_algorithms_Eurasia.png"),
       width = 8, height = 10, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "All_algorithms_Eurasia.png"))
ggsave(All_conus.p, file= here("Final_figures", "All_algorithms_CONUS.png"),
       width = 8.5, height = 7.5, units = c('in'), dpi=300)
knitr::plot_crop(here("Final_figures", "All_algorithms_CONUS.png"))

gc()