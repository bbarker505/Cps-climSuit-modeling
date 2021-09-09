

# Libraries
pkgs <- c("sp", "rgdal", "raster", "tidyverse", "maptools","RColorBrewer", "knitr",
          "cowplot","sf","spData", "here", "openxlsx", "ggalt", "rnaturalearth")
ld_pkgs <- lapply(pkgs, library, character.only = TRUE) # load them

# Load functions
source(here("Scripts", "BOXB_model_functions.R"))

# Occurrence records
recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) 
coordinates(recs) <- ~Longitude + Latitude
crs(recs) <- CRS("+proj=longlat +datum=WGS84")
recs_sf <- st_as_sf(recs) %>% 
  filter(Continent %in% c("Europe", "Asia")) %>%
  st_transform(., prj_eur)

## Extents, projection definitions, and features ----
# Eurasia
ext_eur <- c(xmin = -12, xmax = 61.9, ymin= 34, ymax = 71.3)
prj_eur <- CRS("+proj=lcc +lat_1=43 +lat_2=62 +lat_0=30 +lon_0=10 +x_0=0 +y_0=0 
               +ellps=intl +units=m +no_defs")
eur_cntry_feats <- RegionCrop(type = "countries", ext = ext_eur, prj = prj_eur)
eur_cntry_p <- eur_cntry_feats[[1]] # Polygon feature
eur_cntry_l <- eur_cntry_feats[[2]] # Line features

# Get all stress exclusion raster outputs for last day of year (9 years)
fls <- list.files(here("DDRP", "Europe", "Europe_2010_2019_72321"), ".tif$",
                  full.names = TRUE)

# Make raster list for layers of last day of year
s <- stack(fls)
rasts <- map(1:length(fls), function(i) {
  s <- stack(fls[[i]])
  s[s==0] <- 1 # Change presence value from 0 to 1
  s[s<0] <- 0 # All stress exclusions changed to 0
  lyr <- s[[13]]
  crs(lyr) <- "+proj=longlat +datum=WGS84"
  return(lyr)
})

# Project rasters, and convert them to data frames
rasts_dfs <- Proj_rasts(rasts, ext_eur, prj_eur)

# Add values aross years - if presence all 9 years then value is 9
df <- Reduce(function(...) 
  merge(..., by = c("x", "y")), rasts_dfs) %>%
  data.frame(.[1], value = rowSums(.[3:11])) %>% 
  filter(!(value == 0)) %>%
  dplyr::select(x, y, value)
df$value <- factor(df$value, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"))

# Plot
cols <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(9)

p <- ggplot() + 
  geom_sf(data = eur_cntry_p, color="gray20", fill = "gray85", lwd = 0.3) +
  geom_tile(data = df, aes(x = x, y = y, fill = value)) +
  #scale_fill_viridis_d(option = "plasma", direction = -1) +
  scale_fill_manual(values = cols,
                    name = c("No. of years\nwith presence")) +
  geom_sf(data = eur_cntry_l, lwd = 0.2, color = "gray10") +
  geom_sf(data=recs_sf, shape=21, size=1.7, fill= "deeppink3", color = "white") +
  theme(plot.margin = unit(c(0.01,0,0,0),"cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_blank(),
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x=element_blank(), axis.text.y=element_blank(),
        legend.text=element_text(size=12, face = "bold"),
        legend.margin=margin(0,0,0,-1, unit="cm"),
        legend.key.width = unit(0.8,"line"), 
        legend.key.height = unit(0.8,"line"),
        legend.title = element_text(size = 13, face = "bold"),
        legend.position = c(0.15, 0.79))
p2 <- p + theme(legend.position = "none")

ggsave(p2, file= here("DDRP", "Europe", "DDRP_Eur_2010_2019_7-23-21_noLeg.png"),
       width = 6, height = 5, units = c('in'), dpi=300)
