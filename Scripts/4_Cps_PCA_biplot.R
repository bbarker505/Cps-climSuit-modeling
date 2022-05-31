## Script to create a biplot of PCA loadings of 27 bioclimatic variables used in
## the climate suitability modeling study for Calonectria pseudonavicaulta. 
## Note that this can be done simply by using outputs of PCA done within ENMTML. 
## Scores cannot be plotted with loadings because there are thousands of scores.
library(RStoolbox)
library(raster)
library(tidyverse)
library(here)
library(ggrepel)
library(gtools)

# Environmental data to use
climdat <- "EUR_CliMond"
#outdir <- here("ENMTML", "Outfiles", "run_PCA_4algs_kfold_prev1_03-29-2022")

# Create directory to save results - can't be in the climate data folder
if (!file.exists(outdir)) {
  dir.create(outdir)
}

# Conduct PCA of 27 bioclimatic variables
s <- stack(list.files(here("ENMTML", "All_vars", "Predictors", climdat), 
                      pattern = ".asc", full.names = TRUE))
s_pca <- RStoolbox::rasterPCA(s, spca = TRUE)

scores <- data.frame(PC1 = matrix(s_pca$map$PC1),
                     PC2 = matrix(s_pca$map$PC2)) %>%
  drop_na()

# Extract loadings and importance info from model and save tables
pca_mod <- s_pca$model

# Loadings ----

# Variable names to tack onto loadings table
var_names <- read.table(here("ENMTML", "All_vars", "variable_names.txt"), sep = ",")

loadings <- varimax(pca_mod$loadings[, 1:6])$loadings
loadings <- data.frame(matrix(as.numeric(loadings), 
                           attributes(loadings)$dim, 
                           dimnames=attributes(loadings)$dimnames)) 
loadings <- rownames_to_column(round(loadings, 3) ) 
names(loadings) <- c("Variable", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6")
loadings <- arrange(loadings, order(mixedorder(Variable))) %>%
  mutate(Name = var_names$V2) %>%
  dplyr::select(Variable, Name, everything())

# Save table of formatted loadings
write.xlsx(loadings, here(outdir, "Loadings.xlsx"), overwrite = TRUE)

# Importance (proportion and cumulative variation) ----
prop_var <- data.frame(
  cum_prop = round(
    100 *cumsum(pca_mod$sdev^2 / sum(pca_mod$sdev^2))[1:6] , 1)
  ) %>%
  mutate(prop = cum_prop - lag(cum_prop, default = first(cum_prop)),
         axis = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6")) %>%
  dplyr::select(axis, prop, cum_prop)
prop_var[1,2] <- prop_var[1,3] # Fix cell value 
write.xlsx(prop_var, here(outdir, "Proportion_variance_PC1-6.xlsx"), overwrite = TRUE)

# Reformat importance table to match format of loadings table
prop_var2 <-  data.frame(t(as.matrix(prop_var)))
prop_var2 <- rownames_to_column(prop_var2) %>%
  filter(rowname %in% c("prop", "cum_prop")) %>%
  dplyr::select(-rowname) %>%
  mutate(Variable = NA, Name = NA)
names(prop_var2) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "Variable", "Name")
prop_var2 <- data.frame(lapply(prop_var2[1:6], function(x) 
  as.numeric(as.character(x)))
  )

# Merge tables and save
both <- bind_rows(loadings, prop_var2) %>%
  mutate(Variable = paste0(str_to_sentence(Name), " (", Variable, ")")) %>%
  dplyr::select(-Name) 
both[28,1] <- "Proportion explained by each PC (%)"
both[29,1] <- "Accumulated proportion explained by the PCs (%)"
write.xlsx(both, here(outdir, "Loadings_Proportions.xlsx"), overwrite = TRUE)

# Make the biplot (loadings only)
pc1_var <- prop_var[1,2]
pc2_var <- prop_var[2,2]
loadings$Variable <- sub("bio", "b", loadings$Variable) # Shorten label

biplot.p <- ggplot()+
  #geom_point(data=scores, aes(x=PC1, y=PC2)) +
  geom_segment(data=loadings, aes(x=0, y=0, xend=PC1, yend=PC2),
               arrow=arrow(length=unit(0.2,"cm")), color = "blue")+
  geom_text_repel(data=loadings, aes(x=PC1, y=PC2, label=Variable), size=3,
                  nudge_x = 0.01, nudge_y = 0.01) +
  #geom_text(data=loadings, aes(x=PC1, y=PC2, label=variable), size=3,
  #          position = position_dodge(width = 0.1),
  #          vjust = -0.5) +
  scale_colour_discrete("Variety")+
  scale_x_continuous(paste0("Principal Component 1 (", pc1_var, "%)")) +
  scale_y_continuous(paste0("Principal Component 2 (", pc2_var, "%)")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=12, hjust = 0.7),
        axis.title=element_text(size=13,face="bold")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# Save biplot
ggsave(biplot.p, filename = here("Final_figures", "PCA_Biplot.png"),
       width = 6, height = 6, units = c('in'), dpi=300)
ggsave(biplot.p, filename = here(outdir, "PCA_Biplot.png"),
       width = 6, height = 6, units = c('in'), dpi=300)