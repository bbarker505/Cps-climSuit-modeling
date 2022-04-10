## Functions used for the climate suitability modeling study of Calonectria
## pseudonaviculata, a fungal pathogen causing boxwood blight. This file is 
## required for running scripts in the R project for this study.

# Plots climatic suitability (correlative models) across regions
Alg_suit_plots <- function(region_outs, types, ln_feat, pol_feat, lgd) {
  
  map(types, function(type) {
    
    names(region_outs) <- types
    df <- region_outs[names(region_outs) == type] %>%
      do.call(rbind.data.frame, .)
    
    # Format results for plotting data in categories (bin probabilities)
    df2 <- Bin_pres2(df) %>%
      filter(!is.na(value_bin))
    df2$value_bin <- factor(df2$value_bin,
                           levels = unique(df2$value_bin[order(df2$value)]))
    
    # Plot
    p <- ggplot() + 
      geom_sf(data = pol_feat, color="gray20",  fill = "gray90", lwd = 0.3) +
      geom_raster(data = df2, 
                  aes(x = x, y = y, fill = value_bin)) +
      scale_fill_manual(values = cols, drop = FALSE,
                        na.value = "transparent") +
      geom_sf(data = ln_feat, lwd = 0.2, color = "gray10") +
      #geom_sf(data=recs_eur_sf, shape=21, size=1, fill= "cyan", color = "black") +
      mytheme +
      theme(legend.title = element_text(angle = 90, size = 8.5, face = "bold")) +
      lgd +
      guides(fill = guide_legend(title.position = "left", title.hjust = 0.5,
                                 title = "Prob. of occurrence"))
    
    # Distance scale (in km)
    # if (type == first(types)) {
    #   p <- p + 
    #     ggspatial::annotation_scale(
    #       location = "br",
    #       pad_x = unit(2.7, "cm"),
    #       bar_cols = c("grey60", "white"),
    #       text_cex = 0.5,
    #       height = unit(0.15, "cm")
    #     )  
    # }
    
  })
  
}

# Creates a plot depicting overlap in presence predictions among 4 different
# correlative modeling algorithms
Alg_pres_plot <- function(thres_dfs, ln_feat, pol_feat, recs, region,
                          plot_recs, lgd, size, scale, outfl) {
  
  # Colors
  cols_viridis <- colorRampPalette(c( "#453781FF", "#1F968BFF", "#FDE725FF"))(4)
  
  # Add presence values (=1) across the 4 algorithms to assess agreement
  # Need to show extrapolated areas if its world plots
  if (region == "world") {
    df <- Sum_presence(thres_dfs) %>%
      mutate(x = as.integer(x), y = as.integer(y)) %>%
      full_join(., mop_df, by = c("x", "y")) %>%
      drop_na() %>%
      mutate(value = ifelse(mop == 1, "MOP < 0.9", value)) 
    df$value <- factor(df$value, levels = c("1", "2", "3", "4", "MOP<0.9"))
    
    # Add gray for extrapolated areas 
    cols_viridis <- c(cols_viridis, "gray50")
    ln_wd <- 0.1
    
  } else {
    df <- Sum_presence(thres_dfs)
    ln_wd <- 0.3
  }

  # Plot
  p <- ggplot() + 
    geom_sf(data = pol_feat, color="gray20", fill = "gray90", lwd = ln_wd) +
    geom_raster(data = df, aes(x = x, y = y, fill = value)) +
    #scale_fill_viridis_d(option = "plasma", direction = -1) +
    scale_fill_manual(breaks = as.character(1:4),
                      values = cols_viridis,
                      name = c("Correlative models\nwith presence")) +
    geom_sf(data = ln_feat, lwd = 0.2, color = "gray10") +
    #geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
    mytheme  +
    theme(legend.title = element_text(angle = 0, size = 9, face = "bold"),
          legend.text = element_text(angle = 0, size = 9),
          legend.background = element_rect(fill = "white"),
          legend.box.background = element_rect(fill = "white", color = "white"),
          legend.key =  element_rect(fill = "white")) +
    lgd 
  
  if (scale == "TRUE") {
    ggspatial::annotation_scale(
      location = "br",
      bar_cols = c("grey60", "white"),
      text_cex = 0.6,
      height = unit(0.3, "cm")
    )     
  }
  
  if (plot_recs == 1) {
    p <- p + 
      #geom_sf(data = recs, shape=21, size=0.65, fill= "magenta", color = "black") 
      geom_sf(data = recs, shape=19, size=1.2, color = "white") +
      geom_sf(data = recs, shape=21, size=0.8, fill= "#FF69B4", color = "black") 
  }
  
  # # Save individual plot in output dir
  ggsave(p, filename = here(outdir, paste0(outfl, ".png")),
         width = size, height = size, units = c('in'), device = "png", dpi=300)
  
  return(p)
  
}

# Converting CLIMEX tables to raster functions
Assign_extent <- function(x) {
  ext <-  raster(xmn=min(x$Longitude), xmx=max(x$Longitude), 
                 ymn=min(x$Latitude), ymx=max(x$Latitude), crs="+proj=longlat +datum=WGS84")
  res(ext) <- 0.18 # must change resolution! (or else pixels are huge, and extract gives wrong values)
  return(ext)
}

## Format for plotting data in categories - CLIMEX ecoclimatic index (EI)
Bin_CLMX <-  function(var, df) {
  
  # GI and EI categorical formatting
  if (var == "EI") {

    # Col with bins
    df2 <- Format_EI(df, 7)
    labs <- sort(unique(df2$value_bin))
    brks <- suppressWarnings(as.numeric(str_split_fixed(labs, "-", 2)[,1]))
    
    # Climate stresses categorical formatting
  } else {
    last <- max(df$value, na.rm = TRUE)
    second <- ceiling(last/4)
    third <- ceiling(last/2)
    fourth <- second * 3
    brks <- c(1, second, third, fourth, last)
    labs <- as.character(brks)
    
    # Col with bins
    cuts <- as.integer(labs)
    df2 <- df %>%
      mutate(value_bin = case_when(
        value >= cuts[1] & value <= cuts[2] ~ paste(cuts[1], cuts[2], sep = "-"),
        value > cuts[2] & value <= cuts[3] ~ paste(cuts[2]+1, cuts[3], sep =  "-"),
        value > cuts[3] & value <= cuts[4] ~ paste(cuts[3]+1, cuts[4], sep = "-"),
        value > cuts[4] & value <= cuts[5] ~ paste(cuts[4]+1, cuts[5], sep = "-")))
    
  } 
  
  return(list(brks, labs, df2))
}

# Format for plotting data in categories - correlative modeling algorithsms
Bin_pres <- function(df) {
  df2 <- df %>%
    mutate(value_bin = case_when(value > 0 & value <= 0.1 ~ "0-0.1",
                                 value > 0.1 & value <= 0.2 ~ "0.1-0.2",
                                 value > 0.2 & value <= 0.3 ~ "0.2-0.3",
                                 value > 0.3 & value <= 0.4 ~ "0.3-0.4",
                                 value > 0.4 & value <= 0.5 ~ "0.4-0.5",
                                 value > 0.5 & value <= 0.6 ~ "0.5-0.6",
                                 value > 0.6 & value <= 0.7 ~ "0.6-0.7",
                                 value > 0.7 & value <= 0.8 ~ "0.7-0.8",
                                 value > 0.8 & value <= 0.9 ~ "0.8-0.9",
                                 value > 0.9 ~ "0.9-1.0"))
  df2$value_bin <- factor(df2$value_bin, 
                          levels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4",
                          #levels = c("0.01-0.2", "0.2-0.3", "0.3-0.4",
                          "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"))
  return(df2)
}

Bin_pres2 <- function(df) {
  df2 <- df %>%
    mutate(value_bin = case_when(value < 0.1 ~ "<0.1",
      value >= 0.1 & value <= 0.2 ~ "0.1-0.2",
      value > 0.2 & value <= 0.3 ~ "0.2-0.3",
      value > 0.3 & value <= 0.4 ~ "0.3-0.4",
      value > 0.4 & value <= 0.5 ~ "0.4-0.5",
      value > 0.5 & value <= 0.6 ~ "0.5-0.6",
      value > 0.6 & value <= 0.7 ~ "0.6-0.7",
      value > 0.7 & value <= 0.8 ~ "0.7-0.8",
      value > 0.8 & value <= 0.9 ~ "0.8-0.9",
      value > 0.9 ~ "0.9-1.0"))
  df2$value_bin <- factor(df2$value_bin, 
                          levels = c("<0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4",
                                     "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"))
  return(df2)
}

# For Cut_bins function if data values are < 1
ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)

# Rasterizes CLIMEX output data, applies mask to CONUS, and rescales pixel sizes
CLMX_proc <- function(p, templ){
  ext <- Assign_extent(p)
  clmx <- rasterize(p[, c('Longitude','Latitude')], ext, p[, 1], fun=mean) #Include coords and value columns
  if (names(p)[1] %in% c("CS","HS")){
    clmx <- Rescale(clmx)
  } 
  #bbox <- matrix(st_bbox(templ))
  #x <- raster(xmn=bbox[1,1], xmx=bbox[3,1], ymn=bbox[2,1], ymx=bbox[4,1], res=0.18, crs="+proj=longlat +datum=WGS84")
  #clmx <- rasterize(p[, c('Longitude','Latitude')], x, p[, 1], fun=mean) #Include coords and value columns
  templ <- resample(templ, clmx, method = "bilinear") # put template in same resolution as CLIMEX outs, or mask will not work
  clmx.c <- raster::mask(clmx, templ)  # mask out all areas besides CONUS
  return(clmx.c)
}

# Produce plots of CLIMEX estimates of climate suitability (EI)
CLMX_suit_plots <- function(rast, ext, prj, pol_feat, ln_feat, lgd, recs, km_scl) {
  
  mod_df <- Rasts_to_df1(rast, ext, prj)
  
  # Remove 0 values and bin EI values
  mod_df2 <- mod_df %>%
    #filter(value > 0) %>%
    drop_na %>%
    Bin_CLMX("EI", .) 
  mod_df2 <- mod_df2[[3]]
  
  # Order factor levels
  mod_df2$value_bin <- factor(mod_df2$value_bin,
                              levels = unique(mod_df2$value_bin[order(mod_df2$value)]))
  # Plot
  cols <- c("gray90", colorRampPalette(
    c("#313695", "#4575B4","#ABD9E9","#FEF7B3","#FDD992","#FDBC71", "#EB8B55", "#A50026"))(7))
  
  p <- ggplot() + 
    geom_sf(data = pol_feat, color="gray20",  fill = "gray90", lwd = 0.3) +
    geom_raster(data = mod_df2, 
                aes(x = x, y = y, fill = value_bin)) +
    scale_fill_manual(values = cols, drop = FALSE, na.value = "transparent") +
    geom_sf(data = ln_feat, lwd = 0.2, color = "gray10") +
    #geom_sf(data=recs, shape=21, size=1, fill= "purple", color = "white") +
    mytheme +
    theme(legend.title = element_text(angle = 90, size = 8, hjust = 0.5, face = "bold")) +
    lgd +
    guides(fill = guide_legend(title.position = "left", title.hjust = 0.5,
                               title = "Ecoclimatic index"))
  
  if (km_scl == 1) {
    p <- p + ggspatial::annotation_scale(
      location = "br",
      pad_x = unit(2.7, "cm"),
      bar_cols = c("grey60", "white"),
      text_cex = 0.5,
      height = unit(0.15, "cm")
    )
  }
  return(p)
}

# Convert raster into a data frame for plotting
ConvDF <- function(rast) {
  spdf <- as(rast, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("value", "x", "y")
  return(df)
}

# Maps of overlap between CLIMEX and correlative models of potential distribution
Corr_CLMX_plot <- function(corr_dfs, region, ext, prj, pol_feat, ln_feat, lgd) {
  
  if (!grepl("ens", region)) {
    corr_consens_df <- Sum_presence(corr_dfs) %>%
      mutate(value = ifelse(as.numeric(value) < 4, 0, 1)) 
  } else {
    corr_consens_df <- data.frame(corr_dfs)
  }
  
  # Save the consensus output to validate correlative models for CONUS
  # Create directory for consensus rasters
  if (!file.exists(here(outdir, "Conensus_rasts"))) {
    dir.create(here(outdir, "Conensus_rasts"))
  }
  
  # Convert data frames to rasters and save them for model validation
  #corr_consens_rast <- rasterFromXYZ(corr_consens_df)
  #writeRaster(corr_consens_rast, 
  #            file = here(outdir, "Consensus_rasts", paste0(region, "_consensus.tif")),
  #            format = "GTiff", overwrite = TRUE)

  # Combine with CLIMEX outputs (both with and without irrigation)
  CLMX_df <- Rasts_to_df1(ei_gt10_r, ext, prj)
 # ei_gt10_df <- CLMX_df %>% dplyr::filter(value > 0)
  all_models_list <- list(CLMX_df, corr_consens_df) %>%
    map(~ mutate(.x, across(everything(), as.integer)))
  all_models_df <- purrr::reduce(all_models_list, dplyr::full_join, by = c("x", "y")) %>%
    #mutate(value = rowSums(.[3:last(ncol(.))], na.rm = TRUE)) %>%
    rename("CLIMEX" = "value.x",  "Corr" = "value.y") %>%
    replace(is.na(.), 0) %>%
    filter(!(CLIMEX == 0 & Corr == 0))
  
  if (grepl("europe", region)) {
    all_models_df2 <- mutate(all_models_df,
                             value = factor(case_when(CLIMEX == 1 & Corr == 1 ~ "All models",
                                                      CLIMEX == 1 & Corr == 0 ~ "CLIMEX model",
                                                      CLIMEX == 0 & Corr == 1 ~ "Correlative models"),
                                            levels = c("All models", "CLIMEX model", "Correlative models")))
    cols_pres <- c("All models" = "purple", "CLIMEX model" = "red2",  
                   "Correlative models" = "blue2")
    ln_wd <- 0.3
    
    # Need to consider areas of extrapolation outside the calibration area
  } else {
    mop_df <- Rasts_to_df1(mop, ext, prj) %>%
      drop_na() %>%
      mutate(mop = ifelse(value >= 0.9, 0, 1),
             x = as.integer(x), y = as.integer(y))  %>%
      select(-value)
    
    all_models_df2 <- all_models_df %>%
      left_join(., mop_df, by = c("x", "y")) %>%
      mutate(Corr = ifelse(mop == 1, 2, Corr)) %>%
      replace(is.na(.), 0) %>%
      mutate(value = case_when(CLIMEX == 1 & Corr == 1 ~ "Both",
                               CLIMEX == 1 & Corr == 2 ~ "Both",
                               CLIMEX == 1 & Corr == 0 ~ "CLIMEX",
                               # CLIMEX == 0 & Corr == 1 ~ paste0("Corr. (MOP ", intToUtf8(8805), " 0.9)"),
                               # CLIMEX == 0 & Corr == 2 ~ "Corr. (MOP < 0.9)")) %>%
                              CLIMEX == 0 & Corr == 1 ~ paste0("Correlative (MOP ", intToUtf8(8805), " 0.9)"),
                              CLIMEX == 0 & Corr == 2 ~ "Correlative (MOP < 0.9)")) %>%
      filter(!(is.na(value)))
    all_models_df2$value <- factor(
      all_models_df2$value, levels = c("Both", "CLIMEX", 
                                       paste0("Correlative (MOP ", intToUtf8(8805), " 0.9)"),
                                       "Correlative (MOP < 0.9)"))
    # paste0("Corr. (MOP ", intToUtf8(8805), " 0.9)"), "Corr. (MOP < 0.9)"))
    cols_pres <- c("purple", "red2", "blue2", "gray50")
    ln_wd <- 0.1
  }
  
  # Plot results
  # Note: resampling may cause decimals in x and y coords - these need to 
  # be rounded us "as.integer" above
  p <- ggplot() + 
    geom_sf(data = pol_feat, color="gray20", fill = "gray90", lwd = ln_wd) +
    geom_raster(data = all_models_df2, aes(x = x, y = y, fill = value)) +
    #scale_fill_viridis_d(option = "plasma", direction = -1) +
    scale_fill_manual(values = cols_pres,
                      name = c("Model type")) +
    geom_sf(data = ln_feat, lwd = ln_wd, color = "gray10") +
    #geom_sf(data = occ_recs[[i]], shape=21, size=1, fill= "black", color = "white") +
    mytheme  +
    lgd  +
    theme(legend.title = element_text(angle = 0, size = 9, face = "bold"),
          legend.text = element_text(angle = 0, size = 9),
          legend.background = element_blank(),
          legend.box.background = element_rect(fill = "white", color = "white"),
          legend.key =  element_rect(fill = "white"))
}

# Export rasters for creating map in ArcMap 
CSV_to_rast <- function(infl, output_dir) {
  # Get file and dplyr::select columns of interest
  CLMX_csv <- read.csv(infl) # %>% dplyr::select(Longitude,Latitude,EI)
  
  # Set extent and resolution
  ext <-  raster(xmn=min(CLMX_csv$Longitude), xmx=max(CLMX_csv$Longitude), 
                 ymn=min(CLMX_csv$Latitude), ymx=max(CLMX_csv$Latitude), crs="+proj=longlat +datum=WGS84")
  res(ext) <- 0.1666 # must change resolution! (or else pixels are huge, and extract gives wrong values)
  
  # Function to dplyr::select columns of interest, rasterize and save them
  cols <- c("EI", "CS", "HS", "DS")
  result <- lapply(cols, function(x) {
    result <- CLMX_csv %>% dplyr::select(x, "Longitude", "Latitude")
    result_rast <- rasterize(result[, c('Longitude', 'Latitude')], ext, result[, 1])
    writeRaster(result_rast, file = paste0(output_dir, spp, "_", x), format="GTiff", overwrite=TRUE)
  })
}

# Classify data (= df) so it can be visualized in categories 
# (e.g., 1-10, 11-20, 21-30) 
Cut_bins <- function(df, breaks) {
  df$value_orig <- df$value # Keep old value so can sort factors against it
  # Find max number of digits in the values and cut values into bins, 
  # remove brackets, parentheses and dashes
  # If any non-zero values are less than 1, then need to change dig.lab
  if (any(df$value_orig < 1)) {
    dig.lab <- 2
  } else {
    dig.lab <- nchar(as.character(max(df$value))) 
  }
  
  df2 <- df %>% mutate(value = cut_interval(df$value, n = breaks)) 
  df2$value <- gsub(df2$value, pattern = "[()]|\\[|\\]", 
                    replacement = "", df2$value)

  # Remove any numbers following a decimal - it is unclear how to better 
  # deal with this in the "cut_interval" function (mixing low (below 10)
  # and high (above 10) numbers results in lower numbers having decimals
  # due to the dig.lab input needing to be higher)
  #if (any(df$value_orig < 1 & df$value_orig != 0)) {
  #  df2$bin1 <- ceiling_dec(as.numeric(str_split_fixed(df2$value, ",", 2)[,1]))
  #  df2$bin2 <- ceiling_dec(as.numeric(str_split_fixed(df2$value, ",", 2)[,2]))
  #} else {
    df2$bin1 <- ceiling(as.numeric(str_split_fixed(df2$value, ",", 2)[,1])) 
    df2$bin2 <- ceiling(as.numeric(str_split_fixed(df2$value, ",", 2)[,2])) 
  #}
  
  # Paste those values back together and order the bins according to 
  # the original values to they will plotted in numerical order
  df2$value <- paste(df2$bin1, df2$bin2, sep = "-")
  df2$value <- factor(df2$value, 
                      levels = unique(df2$value[order(df2$value_orig)])) 
  return(df2)
}

# Function to compiled evaluation statistics into a table and export
# as an Excel file
Eval_stats_tbl <- function(result_dir) {
  
  # Evaluation statistics out file
  eval_tbl <- read.table(paste0(result_dir, "/Evaluation_table.txt"), 
                         header = TRUE)
  
  # Get desired statistics and create a new table
  eval_tbl2 <- eval_tbl %>%
    dplyr::select(Algorithm, AUC, Kappa, TSS, Jaccard, Sorensen, Fpb) %>%
    mutate_if(is.numeric, round, digits = 3) %>%
    mutate(type = ifelse(Algorithm %in% c("PCA", "PCS", "WMEA"), 
                         "ensemble", "algorithm")) %>%
    arrange(type) %>%
    dplyr::select(-type) %>%
    mutate(
      Algorithm_nm = case_when(#Algorithm == "BRT" ~ "Boosted Regression Tree",
                               #Algorithm == "GAM" ~ "Generalized Additive Models",
                               Algorithm == "GAU" ~ "Gaussian Process Usage",
                               #Algorithm == "MXD" ~ "Maxent (default)",
                               Algorithm == "MXS" ~ "Maxent (simple)",
                               Algorithm == "RDF" ~ "Random Forests",
                               Algorithm == "SVM" ~ "Support Vector Machine",
                               Algorithm == "WMEA" ~ "Ensemble",
                               Algorithm == "PCA" ~ "Ensemble",
                               Algorithm == "PCS" ~ "Ensemble")) %>%
    dplyr::select(Algorithm_nm, Algorithm, everything()) %>%
    write.xlsx(., here(result_dir, "Evaluation_table.xlsx"),
               overwrite = TRUE)
}

# Bin Ecoclimatic Index (EI) values (CLIMEX)
Format_EI <- function(df, bins) {
  
  if (bins == 4) {
    EI_df <- df %>%
      mutate(value_bin = factor(case_when(
        value > 0 & value <= 10 ~ "1-10",
        value > 10 & value <= 20 ~ "11-20",
        value > 20 & value <= 30 ~ "21-30",
        value > 30 ~ "31-100")))
    
  } else if (bins == 7) {
    EI_df <- df %>% 
      mutate(value_bin = case_when(value < 1 ~ "<1",
                                   value >= 1 & value <= 5 ~ "1-5",
                                   value > 5 & value <= 10 ~ "6-10",
                                   value > 10 & value <= 15 ~ "11-15",
                                   value > 15 & value <= 20 ~ "16-20",
                                   value > 20 & value <= 25 ~ "21-25",
                                   value > 25 & value <= 30 ~ "26-30",
                                   value > 30 ~ "31-100")) 
  } else if (bins == 8) {
    EI_df <- df %>% 
      mutate(value_bin = case_when(value > 0 & value <= 5 ~ "1-5",
                                   value > 5 & value <= 10 ~ "6-10",
                                   value > 10 & value <= 15 ~ "11-15",
                                   value > 15 & value <= 20 ~ "16-20",
                                   value > 20 & value <= 25 ~ "21-25",
                                   value > 25 & value <= 30 ~ "26-30",
                                   value > 30 & value <= 35 ~ "31-35",
                                   value > 35 ~ "36-100")) 
  } else if (bins == 9) {
    EI_df <- df %>% 
      mutate(value_bin = case_when(value > 0 & value <= 5 ~ "1-5",
                                   value > 5 & value <= 10 ~ "6-10",
                                   value > 10 & value <= 15 ~ "11-15",
                                   value > 15 & value <= 20 ~ "16-20",
                                   value > 20 & value <= 25 ~ "21-25",
                                   value > 25 & value <= 30 ~ "26-30",
                                   value > 30 & value <= 35 ~ "31-35",
                                   value > 35 & value <= 40 ~ "36-40",
                                   value > 40 ~ "41-100")) 
  }
  
  return(EI_df)
}

# Function to find UTM zone based on longitude data
# Need to convert dec deg data to UTMs to calculate distance (meters)
long2UTM <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}

# Creates presence plots for individual correlative algorithms and outputs
# the formatted data frame of raster results
Pres_plots_algs <- function(thres, sens_dfs, region, ext, prj) {
  
  map(types, function(t) {
    
    if (thres != "SENSITIVITY") {
      rast <- raster(here(outdir, "Projection", "World", 
                          t, thres, "calonectria_pseudonaviculata.tif"))
      #rast[rast <= 0] <- NA
      thres_df <- Rasts_to_df1(rast, ext, prj) 
    } else {
      thres_df <- sens_dfs[[grep(t, names(sens_dfs))]]
    }
    
    # Plots
    outfl <- paste0("World_pres_", t, "_", thres, ".png")
    
    if (region == "world") {
      thres.p <- ggplot() + 
        geom_sf(data = world_p, color="gray20",  fill = "gray90", lwd = 0.1) +
        geom_raster(data = filter(thres_df, value == 1), aes(x = x, y = y, fill = value)) +
        geom_sf(data = world_l, lwd = 0.1, color = "gray10") + 
        geom_sf(data = na_states_l, lwd = 0.1, color = "gray10") +
        mytheme 
    } else if (region == "europe") {
      thres.p <- ggplot() + 
        geom_sf(data = eur_cntry_p, color="gray20",  fill = "gray90", lwd = 0.1) +
        geom_raster(data = filter(thres_df, value == 1), aes(x = x, y = y, fill = value)) +
        geom_sf(data = eur_cntry_p, lwd = 0.1, color = "gray10") + 
        geom_sf(data = eur_cntry_l, lwd = 0.1, color = "gray10") +
        mytheme 
    } else if (region == "conus") {
      thres.p <- ggplot() + 
        geom_sf(data = conus_states_p, color="gray20",  fill = "gray90", lwd = 0.1) +
        geom_raster(data = filter(thres_df, value == 1), aes(x = x, y = y, fill = value)) +
        geom_sf(data = conus_states_p, lwd = 0.1, color = "gray10") + 
        geom_sf(data = conus_states_l, lwd = 0.1, color = "gray10") +
        mytheme 
    }

    ggsave(thres.p,
         filename = here(outdir, outfl),
        width = 8, height = 4, units = c('in'), device = "png", dpi=300)
    
    # Save output data frame after converting value back to numeric
    thres_df$value <- as.numeric(thres_df$value)
    return(thres_df)
    
  })
}

# Project CLIMEX results by converting them to raster format, and then convert
# back to points for plotting in ggplot
Rasts_to_df1 <- function(rast, ext, prj) {
  crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  rast_crp <- crop(rast, extent(ext))
  rast_prj <- projectRaster(rast_crp, crs = prj, method = "ngb")
  df <-  data.frame(rasterToPoints(rast_prj)) 
  names(df) <- c("x", "y", "value")
  return(df)
}

Rasts_to_df2 <- function(rast_lst, ext, prj) {
  out_lst <- list()
  for (i in 1:length(rast_lst)) {
    rast <- rast_lst[[i]]
    crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    rast_crp <- crop(rast, extent(ext))
    rast_prj <- projectRaster(rast_crp, crs = prj, method = "ngb")
    rast_pts <-  data.frame(rasterToPoints(rast_prj))
    names(rast_pts) <- c("x", "y", "value")
    out_lst[[i]] <- rast_pts
  }
  return(out_lst)
  #assign(rast_na_out[i], rast_pts)
}

# Rasterize a data frame
Rasterize <- function(x, ext, col) {
  rasterize(x[, c('Longitude', 'Latitude')], ext, x[, col])
}

# Crop world world or country polygon (from "rnaturalearth" package plus 
# "rnaturalearthhires" (download from GitHub: "ropensci/rnaturalearthhires)
# Also do set of separate maps just for CONUS and southern Canada
RegionCrop <- function(type, ext, prj) {
  
  # For working with countries feature
  if (type == "countries") {
    countries <- ne_countries(scale = 10, type = "countries", returnclass = "sf") 
    p <- countries %>%
      st_crop(., ext) %>%
      st_transform(., prj)
    
    # For working with states feature
  } else if (type == "states") {
    states <- ne_states(returnclass = "sf")
    p <- states %>%
      st_crop(., ext) %>%
      st_transform(., prj)
  }
  
  # Make a line features in addition to the polygon
  l <- st_cast(p, "MULTILINESTRING" ) # Create line feature
  
  return(list(p, l))
}

# Rescaling function
#Rescale from 0 to 1000 (cellStats does 0 to 1)
Rescale <- function(x){
  x2 <- (x-cellStats(x,"min"))/(cellStats(x,"max")-cellStats(x,"min")) * 1000 
} 

# Add presence values (=1) across the 4 algorithms to assess agreement
Sum_presence <- function(thres_dfs) {
  n_cols <- 2 + length(thres_dfs) 
  df <- purrr::reduce(thres_dfs, dplyr::left_join, by = c("x", "y")) %>%
    mutate(value = rowSums(.[3:n_cols], na.rm = TRUE)) %>%
    filter(!(value == 0)) %>%
    dplyr::select(x, y, value)
  df$value <- factor(df$value)
  return(df)
}

# Function to compile percent contribution/variable importance outputs from
# ENMTML (across algorithms) and export the table as an Excel file
Var_contib_tbl <- function(result_dir) {
  
  # Contribution files across algorithms
  var_contrib_fls <- list.files(
    path = here(result_dir, "Algorithm"),
    pattern = "VariableImportance.txt", 
    full.names = TRUE, recursive = TRUE)
  
  var_contribs <- map(var_contrib_fls, function(x) {
    
    # Rename columns (variable contribution columns names
    # differ across algorithms) and reformat table
    contribs <- read.table(x, row.names = NULL) 
    names(contribs)[length(names(contribs))]<-"Contribution" 
    
    contribs2 <- contribs %>%
      slice(tail(row_number(), length(unique(.$Variables)))) %>%
      mutate(Percent_contribution_or_importance = round(100 * Contribution, 1)) %>%
      dplyr::select(-row.names, -Sp, -Contribution) 
  })
  
  all_contribs <- bind_rows(var_contribs) %>%
    spread(key = Algorithm, value = Percent_contribution_or_importance) %>%
    rowwise() %>%
    mutate(mean = round(mean(c(BRT, GAU, MXS, RDF)), 1),
           min= min(BRT, GAU, MXS, RDF),
           max = max(BRT, GAU, MXS, RDF)) %>%
    # mutate(mean = round(mean(c(BRT, GAM, GAU, MXS, RDF, SVM)), 1),
    #        min= min(BRT, GAM, GAU, MXS, RDF, SVM),
    #        max = max(BRT, GAM, GAU, MXS, RDF, SVM)) %>%
    write.xlsx(., here(result_dir, "Variable_contributions.xlsx"),
               overwrite = TRUE)

}

# Generate occurrence data file for ENMTML
WriteTable <- function(x, nam) {
  
  # Create locations folder if it doesn't already exist
  if (!file.exists(here("ENMTML", "Locations"))) {
    dir.create(here("ENMTML", "Locations"))
  }
  
  x2 <- x %>% 
    dplyr::select(longitude, latitude)  %>%
    mutate(species = "calonectria_pseudonaviculata")
  write.table(x2, here("ENMTML", "Locations", 
                       paste0(nam, "_",  format(Sys.Date(), "%m-%d-%Y"),".txt")), 
              sep = "\t", quote = FALSE, row.names = FALSE) 
}
