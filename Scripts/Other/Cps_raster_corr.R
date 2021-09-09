library(ENMtools)
library(raster)
library(here)
library(dplyr)

# Calculate correlations between CliMond vars (bio1-19, 28-35) for model 
# calibration area used for Calonectria pseudonaviculata (Eurasia and CONUS)
vars <- stack(list.files(pattern = ".asc$", path = here("ENMTML", "Predictors"), 
                         full.names = TRUE))
corrs <- raster.cor.matrix(vars, method = "pearson")

write.csv(corrs, here("ENMTML", "Correlation_vars", "CorrMatrix_predictors.csv"), 
          row.names = FALSE)

# Analysis
corrs <- read.csv(here("ENMTML", "Correlation_vars", "CorrMatrix_predictors.csv"))

# Convert to dataframe
df <- corrs %>% 
  as.data.frame %>% 
  #tibble::rownames_to_column() %>% 
  mutate(var1 = as.numeric(str_sub(names(.), 4))) %>%
  tidyr::pivot_longer(-var1) %>%
  mutate(var2 = as.numeric(str_sub(name, 4)),
         value = round(value, 2))  %>%
  filter(value < 0.999) %>%
  arrange(var1, var2) %>%
  select(var1, var2, value, -name)
  

# Remove variables that we know are correlated to more important predictors
df2 <- df %>% 
  filter(var1 %in% c(5, 6, 7, 15, 33)) %>%
  filter(var2 %in% c(5, 6, 7, 15, 33)) %>%
  #filter(!duplicated(paste0(pmax(var1, var2), pmin(var1, var2)))) %>%# keep unique combos
  spread(., var1, value) %>%
  #mutate(val = as.integer(substring(var2, 4))) %>%
  #arrange(val) %>%
  mutate(variable = paste0("bio", var2)) %>%
  rename(bio15 = `15`, bio5 = `5`, bio6 = `6`, bio7 = `7`) %>%
  select(variable, bio5, bio6, bio7, bio15)

# Save results
write.csv(df2, here("ENMTML", "Correlation_vars", "Predictors_Lt75r2.csv"),
          row.names = FALSE)
