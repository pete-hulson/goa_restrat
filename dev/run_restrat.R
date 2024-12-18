# script to compute goa survey indices under restratification

# load/source libraries/functions for testing ----
library(tidyverse)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# set parameters for running stuff ----
# query data?
run_query = FALSE
# run terra::extract function from shape file?
run_terra = FALSE



# query data ----

## species codes ----
species_t3 = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
species_t5 = c(310, 400, 406, 410, 420, 425, 435, 440, 445, 450, 455, 460, 471, 472, 475, 477, 480, 483, 485, 490, 495, 10170, 10210, 10220, 10250, 10270, 10285, 30020, 30100, 30430, 30475, 30535, 30560, 30576)

species = c(species_t3, species_t5)

if(isTRUE(run_query)){
  data <- query_data(species)
} else{data <- readRDS(here::here('data', 'data.rds'))}


# reclassify haul station locations ----

if(isTRUE(run_terra)){
  ## import new stratum shapefile ----
  strata_2025 <- terra::vect(x = here::here("data","goa_strata_2025.gpkg"))
  
  ## Create spatial object with historical stations ----
  haul_locs_lat <- terra::vect(x = data.frame(data$haul),
                               geom = c("long_st", "lat_st"),
                               crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  ## Project to same projection as the new strata ----
  haul_locs_aea <- terra::project(x = haul_locs_lat,
                                  terra::crs(x = strata_2025))
  
  ## Reclassify each station to the new strata. Takes a while... ----
  haul_locs_new <- terra::extract(x = strata_2025, y = haul_locs_aea)
  
  ## Attach HAULJOINs ----
  haul_locs_new$hauljoin[haul_locs_new$id.y] <- haul_locs_aea$hauljoin[haul_locs_new$id.y]
  
  ## set up new haul data ----
  data$haul %>% 
    tidytable::rename(stratum_og = stratum) %>% 
    tidytable::left_join(haul_locs_new %>% 
                           dplyr::rename_all(tolower) %>% 
                           tidytable::select(stratum_new = stratum,
                                             hauljoin,
                                             area_km2_new = area_km2)) %>% 
    tidytable::drop_na() -> new_haul
  
  vroom::vroom_write(new_haul, here::here('data', 'new_haul.csv'), delim = ',')
} else{new_haul <- vroom::vroom(here::here('data', 'new_haul.csv'))}
# compute abundance indices (biom & numbers) ----



data$cpue %>% 
  tidytable::left_join(data$strata %>% 
                         tidytable::select(survey, stratum, area, area_id)) %>% 
  tidytable::mutate(subreg = case_when(area_id == 803 ~ 'cgoa',
                                       area_id == 804 ~ 'egoa',
                                       area_id == 805 ~ 'wgoa')) %>% 
  tidytable::rename(stratum_og = stratum,
                    area_og = area) %>% 
  tidytable::select(-area_id) %>% 
  tidytable::left_join(new_haul) -> cpue_full

## compute og indices ----
cpue_full %>% 
  # compute mean & var in cpue by stratum
  tidytable::summarise(mean_num = mean(numcpue, na.rm = TRUE),
                       var_num = stats::var(numcpue, na.rm = TRUE) / length(stats::na.omit(numcpue)),
                       mean_wt = mean(wtcpue, na.rm = TRUE),
                       var_wt = stats::var(wtcpue, na.rm = TRUE) / length(stats::na.omit(wtcpue)),
                       .by = c(year, species_code, stratum_og, area_og, subreg)) %>% 
  # compute design-based area-weighted index & var by stratum
  tidytable::mutate(biomass_mt = area_og * mean_wt * 0.001,
                    biomass_var = area_og ^ 2 * var_wt * 1e-6,
                    population_count = area_og * mean_num,
                    population_var = area_og ^ 2 * var_num) -> og_indx_st

# summarise design-based area-weighted index & var by goa subregion & total
og_indx_st %>% 
  tidytable::summarise(biomass_mt = sum(biomass_mt, na.rm = TRUE),
                       biomass_var = sum(biomass_var, na.rm = TRUE),
                       population_count = sum(population_count, na.rm = TRUE),
                       population_var = sum(population_var, na.rm = TRUE),
                       .by = c(year, species_code, subreg)) %>% 
  tidytable::bind_rows(og_indx_st %>% 
                         tidytable::summarise(biomass_mt = sum(biomass_mt, na.rm = TRUE),
                                              biomass_var = sum(biomass_var, na.rm = TRUE),
                                              population_count = sum(population_count, na.rm = TRUE),
                                              population_var = sum(population_var, na.rm = TRUE),
                                              .by = c(year, species_code)) %>% 
                         tidytable::mutate(subreg = 'goa')) %>% 
  tidytable::rename(region = subreg) -> og_indx_reg

## compare with gap indices
og_indx_st %>% 
  tidytable::summarise(biomass_mt = sum(biomass_mt, na.rm = TRUE),
                       biomass_var = sum(biomass_var, na.rm = TRUE),
                       population_count = sum(population_count, na.rm = TRUE),
                       population_var = sum(population_var, na.rm = TRUE),
                       .by = c(year, species_code)) %>% 
  tidytable::left_join(data$index %>% 
                         tidytable::rename(biomass_mt_gap = biomass_mt,
                                           biomass_var_gap = biomass_var,
                                           population_count_gap = population_count,
                                           population_var_gap = population_var) %>% 
                         tidytable::select(-area_id)) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(diff_biom = case_when(biomass_mt_gap > 0 ~ (biomass_mt_gap - biomass_mt) / biomass_mt_gap,
                                          .default = 0),
                    diff_biom_var = case_when(biomass_var_gap > 0 ~ (biomass_var_gap - biomass_var) / biomass_var_gap,
                                              .default = 0),
                    diff_popn = case_when(population_count_gap > 0 ~ (population_count_gap - population_count) / population_count_gap,
                                          .default = 0),
                    diff_popn_var = case_when(population_var_gap > 0 ~ (population_var_gap - population_var) / population_var_gap,
                                              .default = 0)) %>% 
  tidytable::summarise(diff_biom = sum(diff_biom),
                       diff_biom_var = sum(diff_biom_var),
                       diff_popn = sum(diff_popn),
                       diff_popn_var = sum(diff_popn_var),
                       .by = c(species_code)) %>% 
  print(n = 100)

## compute new indices ----
cpue_full %>% 
  # compute mean & var in cpue by stratum
  tidytable::summarise(mean_num = mean(numcpue, na.rm = TRUE),
                       var_num = stats::var(numcpue, na.rm = TRUE) / length(stats::na.omit(numcpue)),
                       mean_wt = mean(wtcpue, na.rm = TRUE),
                       var_wt = stats::var(wtcpue, na.rm = TRUE) / length(stats::na.omit(wtcpue)),
                       .by = c(year, species_code, stratum_new, area_km2_new, subreg)) %>% 
  tidytable::drop_na() %>% 
  # compute design-based area-weighted index & var by stratum
  tidytable::mutate(biomass_mt = area_km2_new * mean_wt * 0.001,
                    biomass_var = area_km2_new ^ 2 * var_wt * 1e-6,
                    population_count = area_km2_new * mean_num,
                    population_var = area_km2_new ^ 2 * var_num) -> new_indx_st

  # summarise design-based area-weighted index & var by goa subregion & total
new_indx_st %>% 
  tidytable::summarise(biomass_mt = sum(biomass_mt, na.rm = TRUE),
                       biomass_var = sum(biomass_var, na.rm = TRUE),
                       population_count = sum(population_count, na.rm = TRUE),
                       population_var = sum(population_var, na.rm = TRUE),
                       .by = c(year, species_code, subreg)) %>% 
  tidytable::bind_rows(new_indx_st %>% 
                         tidytable::summarise(biomass_mt = sum(biomass_mt, na.rm = TRUE),
                                              biomass_var = sum(biomass_var, na.rm = TRUE),
                                              population_count = sum(population_count, na.rm = TRUE),
                                              population_var = sum(population_var, na.rm = TRUE),
                                              .by = c(year, species_code)) %>% 
                         tidytable::mutate(subreg = 'goa')) %>% 
  tidytable::rename(region = subreg) -> new_indx_reg


# compare restratified with og indices ----

species_test = 21720

og_indx_reg %>% 
  tidytable::filter(species_code %in% species_test) %>% 
  tidytable::left_join(new_indx_reg %>% 
                         tidytable::filter(species_code %in% species_test) %>% 
                         tidytable::select(year, region, species_code, biom_new = biomass_mt, popn_new = population_count)) %>% 
  tidytable::summarise(biomass_mt = sum(biomass_mt),
                       biomass_var = sum(biomass_var),
                       population_count = sum(population_count),
                       population_var = sum(population_var),
                       biom_new = sum(biom_new),
                       popn_new = sum(popn_new),
                       .by = c(year, region)) %>% 
  tidytable::mutate(diff_biom = (biom_new - biomass_mt) / biomass_mt,
                    diff_popn = (popn_new - population_count) / population_count,
                    biom_test = case_when(biom_new > biomass_mt + 1.96 * sqrt(biomass_var) ~ 1,
                                          biom_new < biomass_mt - 1.96 * sqrt(biomass_var) ~ 1,
                                          .default = 0),
                    popn_test = case_when(popn_new > population_count + 1.96 * sqrt(population_var) ~ 1,
                                          popn_new < population_count - 1.96 * sqrt(population_var) ~ 1,
                                          .default = 0)) %>% 
  tidytable::select(-biomass_var, -population_var) -> db

# timeseries plot
db %>% 
  tidytable::select(year, region, old = biomass_mt, new = biom_new) %>% 
  tidytable::pivot_longer(., cols = c(old, new)) %>% 
  ggplot(aes(x = year, y = value, colour = name)) +
  geom_line() +
  geom_point() +
  facet_wrap(~region, ncol = 1, scales = 'free_y')


db %>% 
  tidytable::select(year, region, diff_biom, biom_test) %>% 
  ggplot(aes(x = year, y = diff_biom)) +
  geom_bar(stat = "identity") +
  facet_wrap(~region, ncol = 1, scales = 'free_y')
