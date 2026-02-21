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
iters = 500
iters_vec <- set_names(1:iters, 1:iters)

# run simulation  ----
# define test vector (you can either use the total number of stations, or the proportion of stations to reduce)
test <- c(0.1, 0.25, 0.5)
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


(as.numeric(strsplit(sim_time$callback_msg, split = " ")[[1]][1]) / iters) * 500 / 60


# get results ready for plotting
res %>%  
  # take out complexes
  tidytable::filter(!(species_code %in% c(species_rebs, species_dusky, species_orox, species_swf)),
                    species_code %in% c(species_t3, species_t5)) %>% 
  # add re-bs complex
  tidytable::bind_rows(get_cmplx_index(res, 
                                       species_rebs)) %>% 
  # add duskys
  tidytable::bind_rows(get_cmplx_index(res, 
                                       species_dusky)) %>% 
  # add other rockfish
  tidytable::bind_rows(get_cmplx_index(res, 
                                       species_orox)) %>% 
  # add shallow water flats
  tidytable::bind_rows(get_cmplx_index(res, 
                                       species_swf)) %>% 
  # calculate percent differences
  tidytable::mutate(perc_diff_biom = (biomass_mt - biomass_mt_og) / biomass_mt_og,
                    perc_diff_biom_var = (biomass_var - biomass_var_og) / biomass_var_og,
                    perc_diff_num = (population_count - population_count_og) / population_count_og,
                    perc_diff_num_var = (population_var - population_var_og) / population_var_og) %>% 
  tidytable::select(iteration, subtest, year, species_code, perc_diff_biom, perc_diff_biom_var, perc_diff_num, perc_diff_num_var) %>% 
  tidytable::drop_na() %>% 
  # add species type
  tidytable::mutate(species_type = case_when(species_code %in% flats ~ 'flatfish',
                                             species_code %in% rox ~ 'rockfish',
                                             species_code %in% gad ~ 'gadid'),
                    subtest = case_when(subtest == test[1] & as.numeric(subtest) > 1 ~ paste(test[1], 'stations'),
                                        subtest == test[2] & as.numeric(subtest) > 1 ~ paste(test[2], 'stations'),
                                        subtest == test[3] & as.numeric(subtest) > 1 ~ paste(test[3], 'stations'),
                                        subtest == test[1] & as.numeric(subtest) < 1 ~ paste0(100 * as.numeric(test[1]), '% station reduction'),
                                        subtest == test[2] & as.numeric(subtest) < 1 ~ paste0(100 * as.numeric(test[2]), '% station reduction'),
                                        subtest == test[3] & as.numeric(subtest) < 1 ~ paste0(100 * as.numeric(test[3]), '% station reduction'))) -> res_dat
# get mean and median
res_dat %>% 
  tidytable::summarise(mu_biom = mean(perc_diff_biom),
                       mu_biom_var = mean(perc_diff_biom_var),
                       mu_num = mean(perc_diff_num),
                       mu_num_var = mean(perc_diff_num_var),
                       med_biom = median(perc_diff_biom),
                       med_biom_var = median(perc_diff_biom_var),
                       med_num = median(perc_diff_num),
                       med_num_var = median(perc_diff_num_var),
                       .by = c(species_type, subtest)) -> res_stats
  
# plot
ggplot(res_dat, aes(x = perc_diff_biom, fill = species_type)) +
  geom_vline(xintercept = 0, linewidth = 1, linetype = 'dashed') +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.77) +
  facet_grid(subtest ~ species_type, 
             scales = 'free_y') +
  # geom_vline(data = res_stats, aes(xintercept = mu_biom), color = 'green', linewidth = 0.75) +
  # geom_vline(data = res_stats, aes(xintercept = med_biom), color = 'orange', linewidth = 0.75) +
  scale_x_continuous(labels = scales::label_percent()) +
  theme_bw(base_size = 14) +
  scico::scale_fill_scico_d(palette = 'roma') +
  labs(x = "% difference from original value", y = 'Density') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none',
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# save
ggsave(filename = "sim_res.png",
       path = here::here('plots'),
       width = 6.5,
       height = 6.5,
       units = "in")
  

  
