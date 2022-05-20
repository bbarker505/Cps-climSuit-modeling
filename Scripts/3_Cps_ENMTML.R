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
occ_file <- here("ENMTML", "Locations", "Cps_EurAsia_only_sites_03-28-2022.txt")

# There appears to be a bug in the program that prevents it from using multiple
# threshold types on the ensemble model so need to do separate runs for each
thresholds <- c("MAX_TSS", "MAX_KAPPA", "JACCARD", "SORENSEN")
thresholds <- c("MAX_TSS")
predictors <- "EUR_CliMond"

## Run ENMTML ----

# Run 1: Uses a subset of bioclimatic variables
today <- format(Sys.Date(), "%m-%d-%Y")
#outdir <- here("ENMTML", "Outfiles", paste0("run_r75_", today))
#dir.create(outdir)

ENMTML_subset <- function(occ_file, outdir) {
  
  # Climate data
  pred_dir <- here("ENMTML", "Sub_vars", "Predictors", predictors)
  proj_dir <- here("ENMTML", "Sub_vars", "Projection")
  
  for (thres in thresholds) {
    ENMTML(pred_dir = pred_dir, 
           proj_dir = proj_dir, 
           result_dir = outdir, 
           occ_file = occ_file,
           sp = "species",
           x = "longitude", 
           y = "latitude", 
           colin_var = NULL,
           imp_var = TRUE,
           # Restrict background for model fitting to w/in 400 km buffer around points
           sp_accessible_area = c(method = "BUFFER", type = "2", width = "400"),
           # Random allocation of pseudo-absences from the background
           pseudoabs_method = c(method = "RND"),
           #pseudoabs_method = c(method="GEO_ENV_KM_CONST", width = "400"),
           pres_abs_ratio = 1,
           #part = c(method = "BOOT", replicates = "20", proportion = "0.7"),
           # Partition background data using the "Block" method
           part = c(method = "KFOLD", folds = '5'),
           algorithm = c("BRT", "GAU", "MXS", "RDF"),
           thr = thres,
           msdm = NULL,
           save_final = TRUE,
           ensemble = c(method = c("W_MEAN", "PCA", "PCA_SUP"), metric="Fpb"),
           extrapolation = TRUE,
           cores = 4)
    
  }
    # Create and export evaluation statistic and variable contribution tables
    Eval_stats_tbl(outdir)
    Var_contib_tbl(outdir)
  
}

# Run function
#ENMTML_subset(occ_file = occ_file, outdir)

# Run 2: Uses PCA transformed bioclimatic variables 
outdir <- here("ENMTML", "Outfiles", paste0("run_PCA_4algs_", today))
dir.create(outdir)

ENMTML_PCA <- function(occ_file, outdir) {
  
  # Climate data
  #pred_dir <- here("ENMTML", "All_vars", "Predictors")
  pred_dir <- here("ENMTML", "All_vars", "Predictors", predictors)
  proj_dir <- here("ENMTML", "All_vars", "Projection")
  
  for (thres in thresholds) {
    ENMTML(pred_dir = pred_dir, 
           proj_dir = proj_dir, 
           result_dir = outdir, 
           occ_file = occ_file,
           sp = "species",
           x = "longitude", 
           y = "latitude", 
           #thin_occ = c(method='CELLSIZE'),
           colin_var = c(method = "PCA"), # Produce PC variables
           imp_var = TRUE,
           # Restrict background for model fitting to w/in 400 km buffer around points
           sp_accessible_area = c(method = "BUFFER", type = "2", width = "400"),
           # Random allocation of pseudo-absences from the background
           pseudoabs_method = c(method = "RND"),
           #pseudoabs_method = c(method="GEO_ENV_KM_CONST", width = "400"),
           pres_abs_ratio = 1,
           #part = c(method = "BOOT", replicates = "10", proportion = "0.7"),
           # Partition background data using the "Block" method
           #part = c(method = "BLOCK"),
           part = c(method = "KFOLD", folds = '5'),
           #algorithm = c("MXS", "MXD"),
           algorithm = c("BRT", "GAU", "MXS", "RDF"),
           thr = c(thres),
           #thr = c(type=c(thres), sens=0.5),
           msdm = NULL,
           save_final = TRUE,
           ensemble = c(method = c("W_MEAN", "PCA", "PCA_SUP"), metric="Fpb"),
           extrapolation = TRUE,
           cores = 4)
  }
    
  # Create and export evaluation statistic and variable contribution tables
  Eval_stats_tbl(outdir)
  Var_contib_tbl(outdir)
}

# Run function
ENMTML_PCA(occ_file = occ_file, outdir)

# Create directory for final figures to use in manuscript
if (!file.exists(here("Final_figures"))) {
  dir.create(here("Final_figures"))
}

#rm(list = setdiff(ls(), "dat"))