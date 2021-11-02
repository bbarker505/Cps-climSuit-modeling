## Script to run ENMTML program to produce correlative climate suitability
## models for Calonectria pseudonaviculata. The ENMTML run that used a data set
## comprised of six principal components was presented in Barker et al. 20XX.

# Libraries
library(ENMTML)
library(tidyverse)
library(here)
library(sf)
library(openxlsx)

# Notes
# Example of changing a function within ENMTML
# my_maxnet2 <- ENMTML:::maxnet2 # Make edits to this
# assignInNamespace("maxnet2", my_maxnet2, envir = as.environment("package:ENMTML"))

## Setup ----

# Load functions
source(here("Scripts", "Cps_model_functions.R"))

# Occurrence data
occ_file <- here("ENMTML", "Locations", "Cps_noNZ_sites_09-27-2021.txt")
#occ_file <- here("ENMTML", "Locations", "Cps_EurAsia_only_sites_10-27-2021.txt")

# There appears to be a bug in the program that prevents it from using multiple
# threshold types on the ensemble model so need to do separate runs for each
#thresholds <- c("LPT", "MAX_TSS", "JACCARD")
thresholds <- c("MAX_TSS")

# Output directories
#today <- format(Sys.Date(), "%m-%d-%Y")
#out_sub <- here("ENMTML", "Outfiles", paste0("run_r75_", today))
#out_PCA <- here("ENMTML", "Outfiles", paste0("run_PCA_", today))
  
## Run ENMTML ----

# Run 1: Uses a subset of bioclimatic variables
#result_dir <- here("ENMTML", "Outfiles", 
#                   paste0("run_r75_", format(Sys.Date(), "%m-%d-%Y")))
ENMTML_subset <- function(occ_file, out_sub) {
  
  # Climate data
  pred_dir <- here("ENMTML", "Sub_vars", "Predictors")
  proj_dir <- here("ENMTML", "Sub_vars", "Projection")
  
  for (thres in thresholds) {
    ENMTML(pred_dir = pred_dir, 
           proj_dir = proj_dir, 
           result_dir = out_sub, 
           occ_file = occ_file,
           sp = "species",
           x = "longitude", 
           y = "latitude", 
           thin_occ = c(method='CELLSIZE'),
           colin_var = NULL,
           imp_var = TRUE,
           #sp_accessible_area = c(method = "BUFFER", type = "2", width = "300"),
           pseudoabs_method = c(method="GEO_ENV_KM_CONST", width = "400"),
           pres_abs_ratio = 1,
           part = c(method = "BOOT", replicates = "50", proportion = "0.7"),
           algorithm = c("MXS", "BRT", "RDF", "GAM", "GAU", "SVM"),
           thr = c(type = thres),
           msdm = NULL,
           save_final = TRUE,
           ensemble = c(method = c("W_MEAN", "PCA", "PCA_SUP"), metric="Fpb"),
           extrapolation = TRUE,
           cores = 4)
    
  }
    # Create and export evaluation statistic and variable contribution tables
    Eval_stats_tbl(out_sub)
    Var_contib_tbl(out_sub)
  
}

# Run 2: Uses PCA transformed bioclimatic variables 

ENMTML_PCA <- function(occ_file, out_PCA) {
  
  # Climate data
  #pred_dir <- here("ENMTML", "All_vars", "Predictors")
  pred_dir <- here("ENMTML", "All_vars", "Predictors", "EUR_NA")
  proj_dir <- here("ENMTML", "All_vars", "Projection")
  
  for (thres in thresholds) {
    ENMTML(pred_dir = pred_dir, 
           proj_dir = proj_dir, 
           result_dir = out_PCA, 
           occ_file = occ_file,
           sp = "species",
           x = "longitude", 
           y = "latitude", 
           thin_occ = c(method='CELLSIZE'),
           colin_var = c(method = "PCA"),
           imp_var = TRUE,
           pseudoabs_method = c(method="GEO_ENV_KM_CONST", width = "400"),
           pres_abs_ratio = 1,
           part = c(method = "BOOT", replicates = "5", proportion = "0.7"),
           algorithm = c("MXS", "BRT", "RDF", "GAM", "GAU", "SVM"),
           thr =  c(type = thres),
           msdm = NULL,
           save_final = TRUE,
           ensemble = c(method = c("W_MEAN", "PCA", "PCA_SUP"), metric="Fpb"),
           extrapolation = TRUE,
           cores = 4)
  }
  
  # Create and export evaluation statistic and variable contribution tables
  Eval_stats_tbl(out_PCA)
  Var_contib_tbl(out_PCA)
}

# Run functions
#ENMTML_PCA(occ_file = occ_file, out_PCA)

# Create directory for final figures to use in manuscript
if (!file.exists(here("Final_figures"))) {
  dir.create(here("Final_figures"))
}

#rm(list = setdiff(ls(), "dat"))