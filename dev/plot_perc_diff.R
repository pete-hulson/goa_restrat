# Script to plot percent changes for GOA stocks

##Install/Import libraries
library(tidyverse)
library(scales)

# read in trawl sruvey indices
index <- vroom::vroom(here::here('output', 'all_ests.csv'))


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


# start playing with data
index %>% 
  dplyr::rename_all(tolower) %>% 
  # subset to og indices
  tidytable::filter(est_type == 'ORIG',
                    region == 'GOA',
                    species_code %in% species) %>% 
  # take out complexes
  tidytable::filter(!(species_code %in% c(species_rebs, species_dusky, species_orox, species_swf))) %>% 
  # filter columns
  tidytable::select(year, species_code, biom = biomass_mt, num = population_count) %>% 
  # add common names
  tidytable::left_join(vroom::vroom(here::here('data', 'species_names.csv')) %>% 
                         tidytable::select(-species_name)) %>% 
  # add species types
  tidytable::mutate(species_type = case_when(species_code %in% flats ~ 'flatfish',
                                             species_code %in% rox ~ 'rockfish',
                                             species_code %in% gad ~ 'gadid')) %>% 
  tidytable::select(-species_code) %>% 
  # add re-bs complex
  tidytable::bind_rows(index %>% 
                         dplyr::rename_all(tolower) %>% 
                         # subset to og indices
                         tidytable::filter(est_type == 'ORIG',
                                           region == 'GOA',
                                           species_code %in% species_rebs) %>% 
                         tidytable::summarise(biom = sum(biomass_mt),
                                              num = sum(population_count),
                                              .by = c(year)) %>% 
                         tidytable::mutate(common_name = 'RE-BS complex',
                                           species_type = 'rockfish')) %>% 
  # add duskys
  tidytable::bind_rows(index %>% 
                         dplyr::rename_all(tolower) %>% 
                         # subset to og indices
                         tidytable::filter(est_type == 'ORIG',
                                           region == 'GOA',
                                           species_code %in% species_dusky) %>% 
                         tidytable::summarise(biom = sum(biomass_mt),
                                              num = sum(population_count),
                                              .by = c(year)) %>% 
                         tidytable::mutate(common_name = 'dusky rockfish',
                                           species_type = 'rockfish')) %>% 
  # add other rockfish
  tidytable::bind_rows(index %>% 
                         dplyr::rename_all(tolower) %>% 
                         # subset to og indices
                         tidytable::filter(est_type == 'ORIG',
                                           region == 'GOA',
                                           species_code %in% species_orox) %>% 
                         tidytable::summarise(biom = sum(biomass_mt),
                                              num = sum(population_count),
                                              .by = c(year)) %>% 
                         tidytable::mutate(common_name = 'Other rockfish complex',
                                           species_type = 'rockfish')) %>% 
  # add shallow water flats
  tidytable::bind_rows(index %>% 
                         dplyr::rename_all(tolower) %>% 
                         # subset to og indices
                         tidytable::filter(est_type == 'ORIG',
                                           region == 'GOA',
                                           species_code %in% species_swf) %>% 
                         tidytable::summarise(biom = sum(biomass_mt),
                                              num = sum(population_count),
                                              .by = c(year)) %>% 
                         tidytable::mutate(common_name = 'Shallow water flatfish complex',
                                           species_type = 'flatfish')) -> stock_index
  


# calculate percent differences from previous year
stock_index %>% 
  tidytable::mutate(prev_biom = lag(biom),
                    prev_num = lag(num),
                    .by = c(common_name)) %>% 
  tidytable::mutate(Biomass = (biom - prev_biom) / prev_biom,
                    Numbers = (num - prev_num) / prev_num) %>% 
  tidytable::select(year, common_name, species_type, Biomass, Numbers) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(type = '% difference from previous survey') %>% 
  tidytable::bind_rows(# calculate percent differences time-series mean
    stock_index %>% 
      tidytable::mutate(prev_biom = lag(biom),
                        prev_num = lag(num),
                        .by = c(common_name)) %>% 
      tidytable::mutate(Biomass = (biom - mean(biom)) / mean(biom),
                        Numbers = (num - mean(num)) / mean(num),
                        .by = c(common_name)) %>% 
      tidytable::select(year, common_name, species_type, Biomass, Numbers) %>% 
      tidytable::drop_na() %>% 
      tidytable::mutate(type = '% difference from survey mean')) -> plot_dat
  

# plot results
ggplot(plot_dat %>% filter(species_type != 'gadid'), aes(x = factor(year), y = Biomass, fill = species_type)) +
  geom_boxplot(outlier.shape = NA, width = 0.5, alpha = 0.5) +
  geom_point(position = position_dodge(width = 0.5),
             size = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~type, 
             ncol = 1,
             scales = 'free_y') +
  scale_y_continuous(labels = scales::label_percent()) +
  theme_bw(base_size = 14) +
  scico::scale_fill_scico_d(palette = 'roma') +
  labs(x = "Year", y = "% difference in Biomass", fill = "Stock type") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = 'top')


# save
ggsave(filename = "perc_diff.png",
       path = here::here('plots'),
       width = 6.5,
       height = 5,
       units = "in")
