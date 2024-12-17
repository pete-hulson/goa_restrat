# script to compute goa survey indices under restratification

# load/source libraries/functions for testing ----
library(tidyverse)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)


# query data ----

## species codes ----
species_t3 = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
species_t5 = c(310, 400, 406, 410, 420, 425, 435, 440, 445, 450, 455, 460, 471, 472, 475, 477, 480, 483, 485, 490, 495, 10170, 10210, 10220, 10250, 10270, 10285, 30020, 30100, 30430, 30475, 30535, 30560, 30576)

species = c(species_t3, species_t5)

data <- query_data(species)

# import new stratum shapefile ----
strata_2025 <- terra::vect(x = here::here("data","goa_strata_2025.gpkg"))

# reclassify haul station locations ----

## Create spatial object with historical stations ----
haul_locs_lat <- terra::vect(x = data.frame(data$haul),
                             geom = c("long_mid", "lat_mid"),
                             crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## Project to same projection as the new strata ----
haul_locs_aea <- terra::project(x = haul_locs_lat,
                                terra::crs(x = strata_2025))

## Reclassify each station to the new strata. Takes a while... ----
haul_locs_new <- terra::extract(x = strata_2025, y = haul_locs_aea)

## Attach HAULJOINs ----
haul_locs_2025$HAULJOIN[haul_locs_2025$id.y] <- haul_locs_aea$HAULJOIN[haul_locs_2025$id.y]

haul_locs_aea$hauljoin[haul_locs_new$id.y]


haul_locs_new$hauljoin[haul_locs_new$id.y] <- haul_locs_aea$hauljoin[haul_locs_new$id.y]


unique(haul_locs_aea$hauljoin[haul_locs_new$id.y])


data$haul %>% 
  tidytable::summarise(n = n(), .by = c(long_mid, lat_mid)) %>% 
  tidytable::filter(n > 1)


haul_locs_new %>% 
  tidytable::summarise(n = n(), .by = id.y) %>% 
  tidytable::filter(n > 1)

haul_locs_new %>% 
  tidytable::filter(id.y == 2569)

haul_locs_new %>% 
  tidytable::filter(id.y == 8724)

haul_locs_new %>% 
  tidytable::filter(id.y == 10440)



haul_locs_aea[2569,]





length(haul_locs_aea$hauljoin)

length(haul_locs_new$id.y)

max(haul_locs_new$id.y)


length(haul_locs_aea$hauljoin[haul_locs_new$id.y])

