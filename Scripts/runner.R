# Run all scripts for R project for Calonectria pseudonaviculata climate
# suitability modeling study (Barker et al.)
library(here)
here()

# Min and max weekly temps at occurrence records
source("Scripts/1_Cps_extract_tmin_tmax.R")

# CLIMEX model validation
source("Scripts/2_Cps_CLIMEX_model_validation.R")

# Correlative modeling analyses
source("Scripts/3_Cps_corr_model_preprocessing.R") # Preproccesing 

# Run ENMTML on subset of variables and PCA-transformed variables
source("Scripts/4_Cps_ENMTML.R") 
dat <- format(Sys.Date(), "%m-%d-%Y")
occ_file <- here("ENMTML", "Locations", paste0("Cps_noNZ_sites_", dat, ".txt"))
#out_sub <- here("ENMTML", "Outfiles", paste0("run_r75_", dat))
#ENMTML_subset(occ_file, out_sub) # Run ENMTML with data subset
out_PCA <- here("ENMTML", "Outfiles", paste0("run_PCA_", dat))
ENMTML_PCA(occ_file, out_PCA) # Run ENMTML with all data (PCA transformed)

# Define output folder to use for the rest of the post-processing analyses and 
# plots for correlative models (currently models based on PCA-transformed data)

# Post- correlative modeling analyses
source("Scripts/5_Cps_PCA_biplot.R") # PCA tables and biplot
source("Scripts/6_Cps_corr_model_occurrences_summary.R") # Summarize occurrences

# Plot all models outputs (CLIMEX and correlative)
# Europe, western Asia, and CONUS
source("Scripts/7_Cps_region_CLIMEX_climStress_plots.R") # CLIMEX climate stress
source("Scripts/8_Cps_region_climSuit_model_plots.R") # Suitability & presence 

# World
source("Scripts/9_Cps_world_model_plots.R") # Suitability, presence, CLIMEX stress

# Plot comparing rainfall and temperature at 8 localities in Europe, North America
source("Scripts/10_Cps_8location_plots.R")

rm(list = ls())