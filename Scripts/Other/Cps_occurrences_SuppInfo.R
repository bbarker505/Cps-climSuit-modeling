## Script to create Excel file with formatted occurrence records for Calonectria 
## pseudonaviculata, provided as a supporting data file for manuscript reporting
## climate suitability modeling study for the species.
library(tidyverse)
library(here)
library(openxlsx)
library(biogeo)
library(patchwork)

# Load functions
source(here("scripts", "Cps_model_functions.R"))

# Occurrence records - all records and final subset used for correlative models
out_dir <- "run_PCA_09-27-2021"
all_recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx"))
sub_recs <- read.table(
  here("ENMTML", "Outfiles", out_dir, "Occurrences_Cleaned.txt"), 
  header = TRUE) %>%
  rename("Longitude" = "x", "Latitude" = "y") %>%
  mutate("Corr_mod" = 1)

# Convert decimal degrees (DD) to degrees decimal minutes (DDM)
lat_ddm <- dd2dmslat(all_recs$Latitude) %>%
  mutate(min_dd = ymin + ysec/60) %>%
  mutate(lat_ddm = paste0(ydeg, "°", min_dd, "'", NS))

lon_ddm <- dd2dmslong(all_recs$Longitude) %>%
  mutate(min_dd = xmin + xsec/60) %>%
  mutate(lon_ddm = paste0(xdeg, "°", min_dd, "'", EW))

# Join dataset and indicate which were used for correlative models
# Remove accents and other formatting from region names
# Convert DD (degree decimals) to DDM (decimal degree minutes)
all_recs2 <- left_join(all_recs, sub_recs, by = c("Longitude", "Latitude")) %>%
  mutate(Corr_mod = replace_na(Corr_mod, 0),
         Region = iconv(Region, from = 'UTF-8', to = 'ASCII//TRANSLIT'),
         Latitude = lat_ddm$lat_ddm, 
         Longitude = lon_ddm$lon_ddm) %>%
  select(Continent, Country, Region, Site, Latitude, Longitude, Year, Corr_mod, Source) 

write.xlsx(all_recs2, here("Records", "Final_MS_table", "Cps_locations_Final_SuppInfo_Sep2021.xlsx"),
           row.names = FALSE, overwrite = TRUE)

# Convert locations dataset to spatial feature
recs_both.sf <- left_join(all_recs, sub_recs, by = c("Longitude", "Latitude")) %>%
  mutate(Corr_mod = replace_na(Corr_mod, 0)) %>%
  mutate(set = factor(ifelse(Corr_mod == 0, "Full", "Subsampled"),
                      levels = c("Full", "Subsampled"))) %>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs = "WGS84")


# Plot locations used for correlative models ----

# Extents 
ext_conus <- c(xmin = -170, xmax = -51, ymin= 25.5, ymax = 52)
ext_eur <- c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)

# Map projections
prj_eur <- CRS("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 
               +ellps=intl +units=m +no_defs")
prj_conus <- CRS("+init=epsg:5070")

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


eur_lgd <- theme(legend.position = c(0.22, 0.82))
conus_lgd <- theme(legend.position = c(0.93, 0.35))

# Plots
region_lst <- list( c("Europe", "Asia"), c("North America"))
prj_lst <- list(prj_eur, prj_conus)
poly_lst <- list(eur_cntry_p, conus_states_p)
ln_lst <- list(eur_cntry_l, conus_states_l)
lgd_theme_lst <- list(eur_lgd, conus_lgd)

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
    lgd_theme_lst[[i]] +
    theme(legend.key=element_blank(),
          legend.background=element_blank(),
          legend.title = element_text(size = 9, face = "bold"),
          legend.text = element_text(size = 9))
  
})

# Combine and save
loc_plots2 <- loc_plots[[1]] + loc_plots[[2]] +
  plot_layout(ncol = 1)
ggsave(loc_plots2, file= here("Final_figures", "Full_sub_occ_recs.png"),
       width = 6, height = 8, units = c('in'), dpi=300)
