# Script to run simulation testing of effect of dropping stations

# load libraries
library(tidyverse)
library(tictoc)
library(survey)
library(akgfmaps)
library(parallel)
library(foreach)
library(doParallel)

# source functions
source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# query data ----

# tier 3 species codes
species_t3 = c(10110, 10130, 10180, 21720, 21740, 30060, 30420, 10261, 10262, 10200)
species_rebs = c(30050, 30051, 30052)
species_dusky = c(30150, 30152)

# tier 5 species codes
species_t5 = c(30020, 30576)
species_orox = c(30100, 30430, 30475, 30535, 30560)
species_swf = c(10285, 10270, 10170, 10250, 10220, 10210)

# put 'em together
species = c(species_t3, species_t5, species_rebs, species_dusky, species_orox, species_swf)

# define species types
flats = c(10110, 10130, 10180, 10285, 10270, 10170, 10250, 10220, 10210, 10261, 10262, 10200)
rox = c(30060, 30420, 30050, 30051, 30052, 30150, 30152, 30020, 30576, 30100, 30430, 30475, 30535, 30560)
gad = c(21720, 21740)

# get data (if desired, run query)
if(!dir.exists(here::here('data'))){
  dir.create(here::here('data'), recursive = TRUE)
}
if(!file.exists(here::here('data', 'data.rds'))){
  data <- query_data(species)
} else{data <- readRDS(here::here('data', 'data.rds'))}

# Import GOA strata
goa_strata_2025 <- akgfmaps::get_base_layers(select.region = "goa", design.year = 2025, set.crs = "EPSG:4326")$survey.strata

# reassign stations pre-2025 to the new 2025 strata
goa_stations_hist <- data$haul %>% 
  tidytable::filter(year <= 2023) %>% 
  tidytable::mutate(lat = (latitude_dd_start + latitude_dd_end) / 2,
                    lon = (longitude_dd_start + longitude_dd_end) / 2) %>%
  tidytable::select(c(hauljoin, year, lat, lon)) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
  sf::st_intersection(y = goa_strata_2025[, c("STRATUM")])

# define iterations
iters = 2
iters_vec <- set_names(1:iters, 1:iters)

# run simulation  ----

# define test vector of the total number of stations
tests <- seq(200, 500, by = 50)
names(tests) <- tests

# get the number of available cores
num_cores <- parallel::detectCores()
if(num_cores > length(tests)) num_cores = length(tests)

# set the number of cores to be used for parallel computing
doParallel::registerDoParallel(cores = num_cores)

# run tests in parallel
tictoc::tic() # Start timer
foreach::foreach(test = tests, 
                 .packages = c("tidyverse", "survey"),
                 .export = c("restratify")) %dopar% {
  res <- purrr::map_df(
    iters_vec, 
    ~sim_db(
      data, 
      hauls = data$cpue %>% 
        tidytable::distinct(year, hauljoin) %>% 
        tidytable::arrange(year), 
      test = test,
      goa_stations_hist),
  .id = 'iteration')
  
  # write out results
  saveRDS(res, here::here('output', paste0('subsamp_', test, '.rds')))
}
sim_time <- tictoc::toc(quiet = TRUE) # End timer

# compile all results and estimate original values
res <- purrr::map_df(tests,
                     ~readRDS(here::here('output', paste0('subsamp_', .x, '.rds'))),
                     .id = 'subtest') %>% 
  tidytable::left_join(get_index_db(data) %>% 
                         tidytable::drop_na() %>%
                         tidytable::summarise(biomass_mt_og = sum(biomass_mt),
                                              biomass_var_og = sum(biomass_var),
                                              population_count_og = sum(population_count),
                                              population_var_og = sum(population_var),
                                              .by = c(year, species_code, area_id, subreg)) %>%
                         tidytable::mutate(est_type = case_when(year < 2025 ~ "Historical",
                                                                year == 2025 ~ "2025")) %>%
                         tidytable::bind_rows(get_index_db(data) %>%
                                                tidytable::drop_na() %>%
                                                tidytable::summarise(biomass_mt_og = sum(biomass_mt),
                                                                     biomass_var_og = sum(biomass_var),
                                                                     population_count_og = sum(population_count),
                                                                     population_var_og = sum(population_var),
                                                                     .by = c(year, species_code)) %>%
                                                tidytable::mutate(est_type = case_when(year < 2025 ~ "Historical",
                                                                                       year == 2025 ~ "2025"),
                                                                  area_id = 99903, subreg = "GOA")) %>%
                         tidytable::bind_rows(get_index_ps(data, goa_stations_hist) %>%
                                                tidytable::filter(year < 2025) %>%
                                                tidytable::drop_na() %>%
                                                tidytable::rename(biomass_mt_og = biomass_mt,
                                                                  biomass_var_og = biomass_var,
                                                                  population_count_og = population_count,
                                                                  population_var_og = population_var)  %>%
                                                tidytable::mutate(est_type = "Post-stratified")))


# write out results
saveRDS(res, here::here('output', 'subsamp_res.rds'))

# write out total time to run simulation
paste("Run time", round((as.numeric(strsplit(sim_time$callback_msg, split = " ")[[1]][1]) / iters) * 500 / 60 / 60, digits = 1), "hours for 500 iterations")



