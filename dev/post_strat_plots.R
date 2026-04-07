library(tidyverse)
library(cowplot)

all_ests <- vroom::vroom(here::here('output', 'all_ests.csv'))
sp_names <- vroom::vroom(here::here('data', 'species_names.csv'))

# tier 3 species codes
species_t3 = c(10110, 10130, 10180, 21720, 21740, 30060, 30420, 10261, 10262, 10200)
species_rebs = c(30050, 30051, 30052)
species_dusky = c(30150, 30152)

# tier 5 species codes
species_t5 = c(310, 420, 440, 30020, 30576)
species_orox = c(30100, 30430, 30475, 30535, 30560)
species_swf = c(10285, 10270, 10170, 10250, 10220, 10210)
species_skate = c(406, 410, 425, 435, 445, 450, 455, 460, 471, 472, 475, 477, 480, 483, 485, 490, 495)

# put 'em together
species = c(species_t3, species_t5, species_rebs, species_dusky, species_orox, species_swf, species_skate)

# plot regional and subregional indices for stocks with single species code
all_ests %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(species_code %in% species) %>% 
  tidytable::left_join(sp_names) -> plot_dat

for(i in 1:length(c(species_t3, species_t5))){
  
  # get plot data together
  ispp <- c(species_t3, species_t5)[i]
  dat <- plot_dat %>% 
    tidytable::filter(region == 'GOA' & species_code == ispp)
  dat_subreg <- plot_dat %>% 
    tidytable::filter(region != 'GOA' & species_code == ispp) %>% 
    tidytable::mutate(region = factor(region, levels = c("Western GOA", "Central GOA", "Eastern GOA")))
  stock_name <- paste0(first(dat$species_name), " (",first(dat$common_name), ")")
  comm_name <- first(dat$common_name)
  
  # biomass index
  biomass <- ggplot(data = dat, 
    aes(x = year, 
        y = biomass_mt * 1e-3, 
        colour = est_type, shape = est_type)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = biomass_lci * 1e-3, 
                        ymax = biomass_hci * 1e-3), 
                    linewidth = 0.75, size = 0.5) +
    theme_bw(base_size = 14) + 
    labs(x = "", y = "Survey biomass (thousand mt)", 
         colour = "Index type", shape = "Index type", title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  # numbers index
  num <- ggplot(data = dat, 
    aes(x = year, 
        y = population_count * 1e-6, 
        colour = est_type, shape = est_type)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = population_lci * 1e-6, 
                        ymax = population_hci * 1e-6), 
                    linewidth = 0.75, size = 0.5) +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey numbers (millions)", 
         colour = "Index type", 
         shape = "Index type") +
    scico::scale_color_scico_d(palette = 'roma')

  # save plots
  ggsave(filename = here::here('survey_comp_app', 'www', 'stocks',
                               paste0(gsub(x = comm_name, 
                                           pattern = " ", 
                                           replacement = "_"), 
                                      '_region.png')),
         plot =   cowplot::plot_grid(biomass, num, ncol = 1),
         width = 11, height = 7, units = "in")
  
  # biomass by subregion
  biomass_subreg <- ggplot(data = dat_subreg, 
    aes(x = year, 
        y = biomass_mt / 1000, 
        colour = est_type, 
        shape = est_type)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = biomass_lci / 1000, 
                        ymax = biomass_hci  / 1000), 
                    linewidth = 0.75, 
                    size = 0.5) +
    facet_wrap(facets = ~region, 
               ncol = 1, 
               scales = 'free_y') +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey biomass (1000s mt)", 
         colour = "Index type", 
         shape = "Index type", 
         title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  # save plot
  ggsave(filename = here::here('survey_comp_app', 'www', 'stocks',
                               paste0(gsub(x = comm_name, 
                                           pattern = " ", 
                                           replacement = "_"), 
                                      '_subreg_biom.png')),
         plot = biomass_subreg,
         width = 11, height = 7, units = "in")
  
}

# plot for stock complexes
complex_species <- list(species_rebs,
                        species_dusky,
                        species_orox,
                        species_swf,
                        species_skate)

complex_names <- c('Rougheye-Blackspotted rockfish',
                   'Dusky rockfish',
                   'Other rockfish',
                   'Shallow-water flatfish',
                   'Other skates')

for(i in 1:length(complex_species)){
  
  # get plot data together
  ispp <- complex_species[[i]]
  dat <-   plot_dat %>% 
    tidytable::filter(region == 'GOA' & species_code %in% ispp) %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                         population_count = sum(population_count),
                         popn_var = sum(((population_hci - population_count) / 1.96)^2),
                         .by = c(year, est_type, region)) %>% 
    tidytable::mutate(biomass_hci = biomass_mt + 1.96 * sqrt(biomass_var),
                      biomass_lci = biomass_mt - 1.96 * sqrt(biomass_var),
                      population_hci = population_count + 1.96 * sqrt(popn_var),
                      population_lci = population_count - 1.96 * sqrt(popn_var))
  dat_subreg <- plot_dat %>% 
    tidytable::filter(region != 'GOA' & species_code %in% ispp) %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                         population_count = sum(population_count),
                         popn_var = sum(((population_hci - population_count) / 1.96)^2),
                         .by = c(year, est_type, region)) %>% 
    tidytable::mutate(biomass_hci = biomass_mt + 1.96 * sqrt(biomass_var),
                      biomass_lci = biomass_mt - 1.96 * sqrt(biomass_var),
                      population_hci = population_count + 1.96 * sqrt(popn_var),
                      population_lci = population_count - 1.96 * sqrt(popn_var)) %>% 
    tidytable::mutate(region = factor(region, levels = c("Western GOA", "Central GOA", "Eastern GOA")))
  
  complex_name <- complex_names[i]
  
  # biomass index
  biomass <- ggplot(data = dat, 
                    aes(x = year, 
                        y = biomass_mt * 1e-3, 
                        colour = est_type, shape = est_type)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = biomass_lci * 1e-3, 
                        ymax = biomass_hci * 1e-3), 
                    linewidth = 0.75, size = 0.5) +
    theme_bw(base_size = 14) + 
    labs(x = "", y = "Survey biomass (thousand mt)", 
         colour = "Index type", shape = "Index type", title = complex_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  # numbers index
  num <- ggplot(data = dat, 
                aes(x = year, 
                    y = population_count * 1e-6, 
                    colour = est_type, shape = est_type)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = population_lci * 1e-6, 
                        ymax = population_hci * 1e-6), 
                    linewidth = 0.75, size = 0.5) +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey numbers (millions)", 
         colour = "Index type", 
         shape = "Index type") +
    scico::scale_color_scico_d(palette = 'roma')
  
  # save plots
  ggsave(filename = here::here('survey_comp_app', 'www', 'stocks',
                               paste0(gsub(x = complex_name, 
                                           pattern = " ", 
                                           replacement = "_"), 
                                      '_region.png')),
         plot =   cowplot::plot_grid(biomass, num, ncol = 1),
         width = 11, height = 7, units = "in")
  
  # biomass by subregion
  biomass_subreg <- ggplot(data = dat_subreg, 
                           aes(x = year, 
                               y = biomass_mt / 1000, 
                               colour = est_type, 
                               shape = est_type)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = biomass_lci / 1000, 
                        ymax = biomass_hci  / 1000), 
                    linewidth = 0.75, 
                    size = 0.5) +
    facet_wrap(facets = ~region, 
               ncol = 1, 
               scales = 'free_y') +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey biomass (1000s mt)", 
         colour = "Index type", 
         shape = "Index type", 
         title = complex_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  # save plot
  ggsave(filename = here::here('survey_comp_app', 'www', 'stocks',
                               paste0(gsub(x = complex_name, 
                                           pattern = " ", 
                                           replacement = "_"), 
                                      '_subreg_biom.png')),
         plot = biomass_subreg,
         width = 11, height = 7, units = "in")
}

