# Climate suitability modeling study of Calonectria pseudonaviculata

Files in this repository replicate results presented in the climate suitability 
study of *Calonectria pseudonaviculata*, a fungal pathogen that causes boxwood blight. The repository contents include one R project, 10 R scripts, raw and
processed CliMond data used for modeling (27 variables), the full and 
subsampled occurrence record datasets used for modeling, and the CLIMEX 
outfiles. 

All correlative modeling anlayses are conducted within the 
project; however, the process-based CLIMEX model must be run within a 
stand-alone software program using parameters presented in the manuscript. 
The resulting maps and tables associated with correlative models may be slightly 
different than those presented in the manuscript due to random sampling of 
locations used for correlative models (both prior to and during modeling).

Subsetted occurrence records and formatted climate data are provided in this
repository; however, running the project will re-do the entire process
of creating these files using the full occurrence records dataset
(~/Records/Cps_locations_updated_Nov2021) and raw climate data (files in 
~/CliMond_raw). 

## Installation

Simply clone the repository and follow the instructions below.

## Usage

Open the R project "Cps_climSuit_modeling.Rproj" and run scripts in numerical order (number in first character of file name). The scripts will conduct correlative modeling anlayses and creates tables and maps of model outputs produced by CLIMEX and correlatve modeling anlyses. 

The "Cps_model_functions.R" script has functions that are used by most 
of the 9 auxillary scripts. 

All files must remain in the exact same locations as the repository.
Do not delete the full occurrence dataset or CliMond data.

Output files produced by ENMTML will be saved to a folder in "~/ENMTML/Outfiles/" 
that has the date that the model was run. The directory name of this output folder needs to be entered at the top of all subsequent R scripts.

All figures (maps) of model outputs will be saved to a folder named "~/Final_figures"

Please read the comments within each R script for further clarity on the process.

## Packages
The below R packages are used in the project. Please note that the ENMTML
source code was slightly edited (see manuscript for details). This slightly edited version of 
ENMTML is also available via my GitHub account.

`cowplot`  
`ENMTML` (https://github.com/bbarker505/ENMTML)  
`ggalt`  
`ggrepel`  
`gtools`  
`here`  
`knitr`  
`maptools`  
`openxlsx`  
`raster`  
`rnaturalearth`  
`RStoolbox`  
`sf`  
`sp`  
`spatialEco`  
`tidyverse`  

## History
9/9/2021: Created repository  
11/2/2021: Last updates before first manuscript submission    
4/15/2022: Completed manuscript revisions; major updates to repository  
