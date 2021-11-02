library(tidyverse)
library(patchwork)
library(cowplot)

# Produces lines plots comparing monthly temperatures and rainfall across 8
# localities in Europe and North America, based on 1981-2010 norms from the 
# World Meteorlogical Organization (https://climatedata-catalogue.wmo.int/)
clim <- read.csv(here("Climate_pt_analysis", "Climate_8pts_1981-2010norms.csv"))
clim <- clim %>% 
  mutate(Month = factor(str_sub(Month, 1, 3),
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

med <- clim %>% 
  filter(Type == "Mediterranean")
atl <- clim %>% 
  filter(Type == "Atlantic") %>% 
  filter(!(Site == "New Orleans LA"))

# Colors
cols <- c("Cannes FR" = "darkorange3", "Naples IT" = "orange",
          "Seattle WA" = "dodgerblue4", "Portland OR" = "dodgerblue")
cols2 <- c("Brussels BE" = "darkorange3", "Bordeaux FR" = "orange",
           "Virginia Beach VA" = "dodgerblue", "Atlanta GA" = "dodgerblue4")

p1.med <- ggplot(med, aes(x=Month, group=Site, color=Site)) + 
  geom_line(aes(y=MeanC), size=0.5) + 
  geom_line(aes(y=Precipitation/6), size=0.5, linetype="dashed") +
  scale_color_manual(values = cols) +
  scale_x_discrete(expand = c(0,0)) + 
  xlab(NULL) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, 30), expand = c(0,0),
                     name = "Temperature (°C)", 
                     sec.axis = sec_axis(~ . * 6, breaks = scales::pretty_breaks(n=10),
                                         name = "Precipitation (mm)")) +
  theme_light() +
  theme(legend.position = "top", 
        plot.margin = unit(c(t=0.1, r=0.1, b=0.25, l=0.1), "cm"),
        legend.title = element_text(size=10, face="bold"),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)),
        axis.title.y.right = element_text(margin = margin(t=0, r=0, b=0, l=10)))

# p2.med <- ggplot(med, aes(x=Month, y=Precipitation, group=Site, color=Site)) + 
#   geom_line(size=0.5) +
#   scale_color_manual(values = cols) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n=10), 
#                      limits = c(0, 180), expand = c(0,0)) +
#   scale_x_discrete(expand = c(0,0)) +  xlab(NULL) +
#   theme_light()

#p3.med <- p1.med + p2.med + plot_layout(nrow = 1)

p1.atl <- ggplot(atl, aes(x=Month, group=Site, color=Site)) + 
  geom_line(aes(y=MeanC), size=0.5) + 
  geom_line(aes(y=Precipitation/6), size=0.5, linetype="dashed") +
  scale_color_manual(values = cols2) +
  scale_x_discrete(expand = c(0,0)) + 
  xlab(NULL) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5), 
                     limits = c(0, 30), expand = c(0,0),
                     name = "Temperature (°C)",
                     sec.axis = sec_axis(~ . * 6, breaks = scales::pretty_breaks(n=10),
                                         name = "Precipitation (mm)")) +
  theme_light() +
  theme(legend.position = "top", 
        plot.margin = unit(c(t=0.1, r=0.1, b=0.1, l=0.1), "cm"),
        legend.title = element_text(size=10, face="bold"),
        axis.title.y = element_text(margin = margin(t=0, r=10, b=0, l=0)),
        axis.title.y.right = element_text(margin = margin(t=0, r=0, b=0, l=10)))

# p2.atl <- ggplot(atl, aes(x=Month, y=Precipitation, group=Site, color=Site)) + 
#   geom_line(size=0.5) +
#   scale_color_manual(values = cols2) +
#   scale_y_continuous(breaks = scales::pretty_breaks(n=10), 
#                      limits = c(0, 180), expand = c(0,0)) +
#   scale_x_discrete(expand = c(0,0)) +  xlab(NULL) +
#   theme_light()

#p3.atl <- p1.atl + p2.atl + plot_layout(nrow = 1)

p.final <- p1.med + p1.atl + plot_layout(ncol=1)
ggsave(p.final, file= here("Final_figures", "Locations_comparison.png"),
       width = 5.5, height = 8.5, units = c('in'), dpi=300)
