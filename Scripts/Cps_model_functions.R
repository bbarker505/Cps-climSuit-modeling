## Functions used for the climate suitability modeling study of Calonectria
## pseudonaviculata, a fungal pathogen causing boxwood blight. This file is 
## required for running scripts in the R project for this study.

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
  if (var %in% c("EI", "EI.ir")) {

    # Col with bins
    df2 <- Format_EI(df, 9)
    labs <- sort(unique(df2$value_bin))
    brks <- as.numeric(str_split_fixed(labs, "-", 2)[,1])
    
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
    mutate(value_bin = case_when(#value > 0 & value <= 0.1 ~ "0-0.1",
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
                          #levels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4",
                          levels = c("0.1-0.2", "0.2-0.3", "0.3-0.4",
                          "0.4-0.5", "0.5-0.6", "0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1.0"))
  return(df2)
}

Bin_pres2 <- function(df) {
  df2 <- df %>%
    mutate(value_bin = case_when(value > 0 & value <= 0.1 ~ "0-0.1",
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
                          levels = c("0-0.1", "0.1-0.2", "0.2-0.3", "0.3-0.4",
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


# Convert raster into a data frame for plotting
ConvDF <- function(rast) {
  spdf <- as(rast, "SpatialPixelsDataFrame")
  df <- as.data.frame(spdf)
  colnames(df) <- c("value", "x", "y")
  return(df)
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
      Algorithm_nm = case_when(Algorithm == "BRT" ~ "Boosted Regression Tree",
                               Algorithm == "GAM" ~ "Generalized Additive Models",
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
      mutate(value_bin = case_when(value > 0 & value <= 5 ~ "1-5",
                                   value > 5 & value <= 10 ~ "6-10",
                                   value > 10 & value <= 15 ~ "11-15",
                                   value > 15 & value <= 20 ~ "16-20",
                                   value > 20 & value <= 25 ~ "21-25",
                                   value > 25 & value <= 30 ~ "26-30",
                                   value > 30 ~ "31-100")) 
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

# Project CLIMEX results by converting them to raster format, and then convert
# back to points for plotting in ggplot
Rasts_to_df1 <- function(rast, ext, prj) {
  #crs(rast) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
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
    mutate(mean = round(mean(c(BRT, GAM, GAU, MXS, RDF, SVM)), 1),
           min= min(BRT, GAM, GAU, MXS, RDF, SVM),
           max = max(BRT, GAM, GAU, MXS, RDF, SVM)) %>%
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
    dplyr::select("longitude" = Longitude, "latitude" = Latitude)  %>%
    mutate(species = "calonectria_pseudonaviculata")
  write.table(x2, here("ENMTML", "Locations", 
                       paste0(nam, "_",  format(Sys.Date(), "%m-%d-%Y"),".txt")), 
              sep = "\t", quote = FALSE, row.names = FALSE) 
}
