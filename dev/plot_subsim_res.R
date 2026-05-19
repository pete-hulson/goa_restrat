# plot sub-sampling simulation results
# load libraries
library(tidyverse)

# source functions
source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# get results
readRDS(here::here('output', 'subsamp_res.rds')) -> res

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

## plot distribution of percent difference
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
                    perc_diff_num_var = (population_var - population_var_og) / population_var_og,
                  pos_biom = case_when(perc_diff_biom > 0 ~ 1,
                  .default = 0)) %>% 
  tidytable::select(iteration, subtest, year, species_code, perc_diff_biom, perc_diff_biom_var, perc_diff_num, perc_diff_num_var, pos_biom) %>% 
  tidytable::drop_na() %>% 
  # add species type
tidytable::mutate(species_type = case_when(species_code %in% flats ~ 'flatfish',
species_code %in% rox ~ 'rockfish',
species_code %in% gad ~ 'gadid'),
subtest = forcats::fct_rev(subtest)) -> res_dat

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
                       prob_pos = round(sum(pos_biom) / n(), digits = 2),
                       .by = c(species_type, subtest)) %>%
  tidytable::mutate(prob_neg = 1 - prob_pos,
  prob_pos = scales::percent(prob_pos, accuracy = 1),
  prob_neg = scales::percent(prob_neg, accuracy = 1)) -> res_stats
  
# plot
ggplot(res_dat, aes(x = perc_diff_biom, fill = species_type)) +
  geom_vline(data = res_stats, aes(xintercept = med_biom), color = 'dark green', linewidth = 0.75) +
  geom_vline(xintercept = 0, linewidth = 1, linetype = 'dashed') +
  geom_histogram(aes(y = after_stat(density)), bins = 50, alpha = 0.77) +
  facet_grid(subtest ~ species_type, 
             scales = 'free_y') +
  # geom_text(data = res_stats, aes(x = Inf, y = Inf, label = paste0("P(>0) = ", prob_pos)), 
  #           color = 'black', size = 2.5, hjust = 1.2, vjust = 1.2, inherit.aes = FALSE) +
  # geom_text(data = res_stats, aes(x = -Inf, y = Inf, label = paste0("P(<0) = ", prob_neg)), 
  #           color = 'black', size = 2.5, hjust = -0.2, vjust = 1.2, inherit.aes = FALSE) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(-2, 2)) +
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
  

  
## plot cv in biomass
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
  # calculate cv in biomass
  tidytable::mutate(cv_biom = sqrt(biomass_var) / biomass_mt,
                    cv_biom_og = sqrt(biomass_var_og) / biomass_mt_og) %>% 
  tidytable::select(iteration, subtest, year, species_code, cv_biom, cv_biom_og) %>%
  tidytable::drop_na() %>% 
  # add species type
  tidytable::mutate(species_type = case_when(species_code %in% flats ~ 'flatfish',
                    species_code %in% rox ~ 'rockfish',
                    species_code %in% gad ~ 'gadid')) -> res_dat_cv

res_dat_cv %>% 
  tidytable::summarise(med_cv_biom_og = median(cv_biom_og),
.by = c(species_type)) -> res_stats_cv

ggplot(res_dat_cv, aes(x = subtest, y = cv_biom, fill = species_type)) +
  geom_boxplot() +
  geom_hline(data = res_stats_cv, aes(yintercept = med_cv_biom_og), color = 'dark green', linewidth = 0.75) +
  facet_wrap(~species_type) +
  scico::scale_fill_scico_d(palette = 'roma') +
  labs(x = "Number of survey stations", y = "CV in biomass") +
  theme_bw(base_size = 14) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = 'none')

# save
ggsave(filename = "sim_cv_res.png",
       path = here::here('plots'),
       width = 8,
       height = 5,
       units = "in")
