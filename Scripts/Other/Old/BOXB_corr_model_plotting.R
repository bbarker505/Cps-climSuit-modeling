# Script for plotting SDM (correlative) models for BOXB in Eurasia and CONUS

# Libraries
pkgs <- c("raster", "dplyr", "stringr", "ggplot2",
          "ggthemes","maptools","RColorBrewer","cowplot","sf","ggspatial",
          "here", "openxlsx", "ggalt", "rnaturalearth", "purrr")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("scripts", "BOXB_model_functions.R"))

## Records 
all_recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
sub_recs <- read.table(here("ENMTML", "Locations", 
                            "Cps_noNZ_sites_2021-05-26.txt"), header = TRUE) %>%
  rename("Latitude" = "latitude", "Longitude" = "longitude")

## Output files ---- 
out_dir <- here("ENMTML", "outfiles", "run_06-16-2021_1")

# All files for training/test area (not projection)
all_fls <- list.files(out_dir, 
                      pattern = glob2rx("*calonectria_pseudonaviculata*.tif$*"),
                      recursive = TRUE, full.names = TRUE)
all_fls <- all_fls[-grep("Projection", all_fls)]

# Pattern for threshold rasters
thres <- c("JACCARD|MAX_KAPPA|MAX_TSS|SORENSEN")

# Import by type of output as a raster stack

# Algorithm outputs
alg_outs <- map(all_fls[-grep(paste0(thres, "|GeoEnv|MOP|MXD"), all_fls)], raster)
# Binary outputs - but remove MXD and Ensemble from main figure
thres_pat <- all_fls[grep("MAX_TSS", all_fls)]
thres_pat <- thres_pat[-grep("PCA_SUP|MXD", thres_pat)]
thres_outs <- map(thres_pat, raster)

# Extents 
ext_conus <- c(xmin = -170, xmax = -51, ymin= 25.5, ymax = 52)
ext_eur <- c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)
ext_world <- c(xmin = -180,  xmax = 180, ymin= -59.47275, ymax = 83.6341)

# Map projections
prj_eur <- CRS("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 
               +ellps=intl +units=m +no_defs")
prj_world <- CRS("+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 
                 +datum=WGS84 +units=m +no_defs") # World Robinson projection
prj_conus <- CRS("+init=epsg:5070")

# Project rasters for plotting
alg_outs.eur <- Proj_rasts(alg_outs, ext_eur, prj_eur)
alg_outs.conus <- Proj_rasts(alg_outs, ext_conus, prj_conus)
thres_outs.eur <- Proj_rasts(thres_outs, ext_eur, prj_eur)
thres_outs.conus <- Proj_rasts(thres_outs, ext_conus, prj_conus)

# Background maps (polygons and lines) to use in plots
# The "RegCrop" function crops and sf object for "countries" or "states" 
# (rnaturalearth package)
# World
world_p <- ne_countries(scale = 10, type = "countries", returnclass = "sf") %>%
  filter(!(admin == "Antarctica")) # Multi-polygon
world_p <- st_transform(world_p, prj_world) 
world_l <- st_cast(world_p, "MULTILINESTRING") # Create line feature

# Eurasia countries
eur_cntry_feats <- RegionCrop(type = "countries", ext = ext_eur, prj = prj_eur)
eur_cntry_p <- eur_cntry_feats[[1]] # Polygon feature
eur_cntry_l <- eur_cntry_feats[[2]] # Line features

# North America and U.S. states and Canadian provinces
conus_states_feats <- RegionCrop(type = "states", ext = ext_conus, prj = prj_conus)
conus_states_p <- conus_states_feats[[1]] # Polygon feature
conus_states_l <- conus_states_feats[[2]] # Line features

# Themes for plots
mytheme <- theme(plot.margin = unit(c(0.01,0,0,0),"cm"),
                 panel.grid.major = element_blank(), 
                 panel.grid.minor = element_blank(), 
                 panel.background = element_blank(), panel.border = element_blank(),
                 axis.title.x = element_blank(), axis.title.y = element_blank(), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(), axis.text.y=element_blank(), 
                 legend.margin=margin(0,0,0,-1, unit="cm"),
                 legend.key.width = unit(0.82,"line"), 
                 legend.key.height = unit(0.64,"line"))

# Specifics for legends
eur_lgd <- theme(legend.key.width = unit(0.72,"line"), 
                 legend.key.height = unit(0.55,"line"),
                 legend.title = element_text(size = 8, face = "bold"),
                 legend.position = c(0.15, 0.79))
conus_lgd <- theme(legend.title = element_text(size = 9, face = "bold"),
                   legend.position = c(0.95, 0.37))

# Color scale for model predictions
col_prob <- c("#313695","#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71",
          "#EB8B55", "#C8453D", "#A50026")
col_thres <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(6)

## Suitability plots ----

# Plots showing results of the 6 algorithms (1-6) and ensemble (7)
types <- c("BRT", "GAM", "GAU", "MXS", "RDF", "SVM", "Ensemble")
alg_out_lst <- list(alg_outs.eur, alg_outs.conus)
poly_lst <- list(eur_cntry_p, conus_states_p)
ln_lst <- list(eur_cntry_l, conus_states_l)
lgd_theme_lst <- list(eur_lgd, conus_lgd)
out_plot_lst <- c("eur_plots", "con_plots")

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
      scale_fill_manual(values = col_prob, drop = FALSE,
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
eur_plots2 <- map(list(eur_plots[[1]], eur_plots[[7]]), function(p) {
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

con_plots2 <- map(list(con_plots[[1]], con_plots[[7]]), function(p) {
  
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

# Combine plots for the 6 algorithms (supporting info)
labs <- c("(a) BRT", "(b) GAM", "(c) GAU", "(d) MXS", 
          "(e) RDF", "(f) SVM")
All_eur.p <- plot_grid(eur_plots2[[1]], 
                       eur_plots[[2]], eur_plots[[3]], 
                       eur_plots[[4]], eur_plots[[5]], eur_plots[[6]],
                       ncol = 2, nrow = 3,
                       labels = labs,
                       label_size = 12, hjust = 0, vjust = 1)
ggsave(All_eur.p, file= paste0(out_dir, "/All_Eurasia.png"),
       width = 8, height = 10, units = c('in'), dpi=300)

All_conus.p <- plot_grid(con_plots2[[1]], con_plots[[2]], con_plots[[3]], 
                         con_plots[[4]], con_plots[[5]], con_plots[[6]],
                       ncol = 2, nrow = 3,
                       labels = labs,
                       label_size = 12, hjust = 0, vjust = 1)
ggsave(All_conus.p, file= paste0(out_dir, "/All_CONUS.png"),
       width = 8.5, height = 7.5, units = c('in'), dpi=300)

## Binary map plots ----

# Need adjustments to legend positions
eur_lgd2 <- theme(legend.position = c(0.22, 0.82))
conus_lgd2 <- theme(legend.position = c(0.93, 0.35))
lgd_theme_lst2 <- list(eur_lgd2, conus_lgd2)

# Inputs
thres_out_lst <- list(thres_outs.eur, thres_outs.conus)
out_plot_lst2 <- c("eur_thr.p", "con_thr.p")

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
    mytheme  +
    lgd_theme_lst2[[i]]  +
    theme(legend.title = element_text(angle = 0, size = 9, face = "bold"),
          legend.text = element_text(angle = 0, size = 9),
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          legend.key = element_blank())    
  
  assign(out_plot_lst2[i], p)

}

## Location plots ----

# Convert locality records to sf object and project
recs_both.sf <- full_join(all_recs, sub_recs, 
                          by = c("Longitude", "Latitude")) %>%
  mutate(set = factor(ifelse(is.na(species), "Full", "Subsampled"), 
         levels = c("Full", "Subsampled"))) %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = "WGS84")

# Plot locations
region_lst <- list( c("Europe", "Asia"), c("North America"))
prj_lst <- list(prj_eur, prj_conus)

loc_plots <- map(c(1:2), function(i) {
  
  locs <- recs_both.sf %>% 
    filter(Continent %in% region_lst[[i]]) %>%
    st_transform(., prj_lst[[i]])
  
  locs.p <- ggplot() + 
    geom_sf(data = poly_lst[[i]], color="gray20", fill="gray85", lwd = 0.3) +
    geom_sf(data = ln_lst[[i]], lwd=0.2, color="gray10") +
    geom_sf(data=locs, aes(geometry=geometry, shape=set, fill=set), 
            color="white", size=1.5) +
    scale_fill_manual(values = c("red2","blue2"),
                       name = c("Ocurrence\nrecords")) +
    scale_shape_manual(values=c(24,21),
                       name = c("Ocurrence\nrecords")) +
    mytheme +
    lgd_theme_lst2[[i]] +
    theme(legend.key=element_blank(),
          legend.background=element_blank(),
          legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 9))

})

# Plot for main manuscript - ensemble suitability, binary, and locations
ensemble.p <- plot_grid(loc_plots[[1]], loc_plots[[2]],
                        eur_plots2[[2]], con_plots2[[2]],
                        eur_thr.p, con_thr.p, 
                        ncol = 2, nrow = 3,
                        labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"),
                        label_size = 14, hjust = 0, vjust = 1)
ggsave(ensemble.p, file= paste0(out_dir, "/Ensemble_1.png"),
       width = 9, height = 9, units = c('in'), dpi=300)

# Maybe don't want to include locations in this figure so create a second version
ensemble.p2 <- plot_grid(eur_plots2[[2]], eur_thr.p,
                         con_plots2[[2]], con_thr.p, 
                        ncol = 2, nrow = 2, 
                        rel_heights = c(1, 0.85),
                        labels = c("(a)", "(b)", "(c)", "(d)"),
                        label_size = 15, hjust = -0.25, vjust = 1)
ggsave(ensemble.p2, file= paste0(out_dir, "/Ensemble_2.png"),
       width = 9, height = 6.5, units = c('in'), dpi=300)


# Save desired figures to "Final_figures" folder, and crop off white space
final_plots <- list(All_eur.p, All_conus.p, ensemble.p, ensemble.p2 )
widths <- c(8, 8.5, 9, 9)
heights <- c(10, 7.5, 9, 6.5)
nams <- c("All_Corr_Eurasia.png", "All_Corr_CONUS.png", 
          "Ensemble_Corr_Locs_Eur_CONUS.png", "Ensemble_Corr_Eur_CONUS.png")

for (i in seq_along(final_plots)) {
  ggsave(final_plots[[i]], file= here("Final_figures", nams[i]),
         width = widths[i], height = heights[i], units = c('in'), dpi=300)
  knitr::plot_crop(here("Final_figures", nams[i]))
}

