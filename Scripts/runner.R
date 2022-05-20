# Run all scripts for R project for Calonectria pseudonaviculata climate
# suitability modeling study (Barker et al.)
library(here)
here()

# Min and max weekly temps at occurrence records
source("Scripts/1_Cps_extract_tmin_tmax.R")

# Pre-processing (sub-sample occurrence records, process climate data)
source("Scripts/2_Cps_corr_model_preprocessing.R") 

# Run ENMTML on subset of variables and PCA-transformed variables
# Change "outdir" name within this file
source("Scripts/3_Cps_ENMTML.R") 

# Post- correlative modeling analyses
source("Scripts/4_Cps_PCA_biplot.R") # PCA tables and biplot
source("Scripts/5_Cps_corr_model_occurrences_summary.R") # Summarize occurrences

# Plot all models outputs (CLIMEX and correlative)
# Europe, western Asia, and CONUS
source("Scripts/6_Cps_CLIMEX_stress_plots.R") # CLIMEX climate stress
source("Scripts/7_Cps_climSuit_model_plots.R") # Suitability & presence 

# Model validation analyses
source("Scripts/8_Cps_model_validation.R")

# Plot comparing rainfall and temperature at 8 localities in Europe, North America
source("Scripts/9_Cps_8location_plots.R")

rm(list = ls())