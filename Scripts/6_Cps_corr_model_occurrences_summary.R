## Script to summarize records used to fit and validate correlative 
## climate suitability models for Calonectria pseudonaviculata. 
library(tidyverse)
library(here)

# Occurrence records - all records and final subset used for correlative models
outdir <- here("ENMTML", "Outfiles", "run_PCA_09-27-2021")
all_recs <- read.xlsx(here("Records", "Cps_locations_updated_Apr2021.xlsx")) %>%
  filter(!(Country == "New Zealand")) # NZ occurrences not used in models
sub_recs <- read.table(
  here(outdir, "Occurrences_Cleaned.txt"), 
  header = TRUE) %>%
  rename("Longitude" = "x", "Latitude" = "y")

# Summarize all records (Table 1 for manuscript?)
all_recs_sumt <- all_recs %>%
  select(Continent, Country, Region) %>%
  group_by(Continent, Country) %>%
  tally()

# Create an indicator of whether record is present in full data set
recs_jn <- select(all_recs, Latitude, Longitude, Site) %>%
  left_join(sub_recs %>% transmute(Latitude, Longitude, Corr_Model = 1)) %>%
  replace_na(list(Corr_Model = 0))

# Create a table of recs to use in supporting information that has this info 
# along with other relevant record information.
all_recs2 <- all_recs %>%
  left_join(., recs_jn) %>% 
  distinct(Latitude, Longitude, Site, .keep_all = TRUE) %>%
  select(Latitude, Longitude, Continent, Country, 
         Region, Site, Year, Type, Corr_Model) %>%
  rename("Source_Type" = "Type")

# Summarize for each region
recs_summary <- all_recs2 %>%
  group_by(Continent, Corr_Model) %>%
  tally() %>%
  mutate(Region = ifelse(Continent %in% c("Asia", "Europe"), 
                         "Eurasia", Continent)) %>%
  group_by(Continent) %>%
  mutate(Total = sum(n))

write.csv(recs_summary, here(outdir, "Occurrences_Summary.csv"), 
          row.names = FALSE)

rm(list = setdiff(ls(), "outdir"))
