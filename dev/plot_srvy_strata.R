# load libraries
library(sf)
library(ggplot2)
library(tidyverse)
library(akgfmaps)
library(geosphere)
library(janitor)
library(gridExtra)
library(grid)
# remotes::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)

# plot GOA NMFS management areas
nmfs_areas <- akgfmaps::get_nmfs_areas(set.crs = "EPSG:4326")
goa_layers <- akgfmaps::get_base_layers(select.region = "goa", set.crs = "EPSG:3338")
goa_layers_hist <- akgfmaps::get_base_layers(select.region = "goa", design.year = 1984, set.crs = "EPSG:3338")

# get 610 and 620 survey strata for plotting
goa_curr <- goa_layers$survey.strata
goa_hist <- goa_layers_hist$survey.strata



ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620, 630, 640, 650)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_curr, aes(fill = as.factor(STRATUM)), alpha = 0.7, color = "black", size = 0.2) +
  geom_sf(data = goa_layers$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -130),
           ylim = c(50, 62),
           crs = "+proj=longlat +datum=WGS84") +
  scico::scale_fill_scico_d(palette = "roma") +
    labs(title = "2025 design survey strata",
         x = "Longitude",
         y = "Latitude",
         fill = 'Strata') +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14))

ggsave(here::here('plots', 'GOA_Survey_Map_2025.png'), width = 10, height = 10, units = "in", dpi = 300)



ggplot() +
  geom_sf(data = nmfs_areas %>% filter(REP_AREA %in% c(610, 620, 630, 640, 650)), alpha = 0, color = "black", size = 0.1) +
  geom_sf(data = goa_hist, aes(fill = as.factor(STRATUM)), alpha = 0.7, color = "black", size = 0.2) +
  geom_sf(data = goa_layers$akland, fill = "#2c3e50", color = "white") +
  coord_sf(xlim = c(-170, -130),
           ylim = c(50, 62),
           crs = "+proj=longlat +datum=WGS84") +
  scico::scale_fill_scico_d(palette = "roma") +
  labs(title = "1984 design survey strata",
        x = "Longitude",
        y = "Latitude",
        fill = 'Strata') +
  theme(plot.title = element_text(size = 20, face = "bold"),
                axis.title = element_text(size = 14),
                axis.text = element_text(size = 12),
                legend.text = element_text(size = 12),
                legend.title = element_text(size = 14))

ggsave(here::here('plots', 'GOA_Survey_Map_1984.png'), width = 10, height = 10, units = "in", dpi = 300)
