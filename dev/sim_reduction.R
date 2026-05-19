# Script to run simulation testing of effect of dropping stations

# load libraries
library(tidyverse)
library(tictoc)

# source functions
source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# query data ----
# query data? - if first time running, or if data.rds file not in data folder, you will need to change this to TRUE
run_query = FALSE

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

# run query
if(isTRUE(run_query)){
  data <- query_data(species)
} else{data <- readRDS(here::here('data', 'data.rds'))}


# define iterations
iters = 1000
iters_vec <- set_names(1:iters, 1:iters)

# run simulation  ----
# define test vector (you can either use the total number of stations, or the proportion of stations to reduce)
test <- seq(200, 500, by = 50)
names(test) <- test

tictoc::tic() # Start timer
res <- purrr::map_df(iters_vec, ~purrr::map_df(test, ~sim_db(data, 
                                                             hauls = data$cpue %>% 
                                                               tidytable::distinct(year, hauljoin) %>% 
                                                               tidytable::arrange(year), 
                                                             test = .x), .id = 'subtest'),
                     .id = 'iteration',
                     .progress = list(type = "iterator", 
                                      format = "Resampling {cli::pb_bar} {cli::pb_percent}",
                                      clear = TRUE)) %>% 
  tidytable::left_join(get_index_db(data) %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarise(biomass_mt_og = sum(biomass_mt),
                                              biomass_var_og = sum(biomass_var),
                                              population_count_og = sum(population_count),
                                              population_var_og = sum(population_var),
                                              .by = c(year, species_code)))
sim_time <- tictoc::toc(quiet = TRUE) # End timer

paste("Run time", round((as.numeric(strsplit(sim_time$callback_msg, split = " ")[[1]][1]) / iters) * 500 / 60 / 60, digits = 1), "hours")

# write out results
saveRDS(res, here::here('output', 'subsamp_res.rds'))
