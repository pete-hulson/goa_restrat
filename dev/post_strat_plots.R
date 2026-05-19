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

# get data ready for plotting
all_ests %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(species_code %in% species, year < 2025) %>% 
  tidytable::mutate(year = case_when(est_type == 'PS' ~ year - 0.25, .default = year)) %>%
  tidytable::pivot_wider(names_from = est_type, values_from = c(biomass_mt, biomass_lci, biomass_hci, population_count, population_lci, population_hci)) %>%
  tidytable::drop_na() -> .data

.data %>% 
  tidytable::select(year, species_code, area_id, region, ends_with("ORIG")) %>% 
  tidytable::mutate(est_type = 'ORIG') %>%
  tidytable::rename_with(~gsub(x = ., pattern = "_ORIG", replacement = "")) %>% 
  tidytable::bind_rows(.data %>% 
    tidytable::select(year, species_code, area_id, region, ends_with("PS")) %>% 
      tidytable::mutate(est_type = 'PS',
                        year = year + 0.25) %>%
      tidytable::rename_with(~gsub(x = ., pattern = "_PS", replacement = ""))) %>% 
  tidytable::bind_rows(all_ests %>% 
      dplyr::rename_all(tolower) %>% 
      tidytable::filter(species_code %in% species & year == 2025)) %>%
  tidytable::filter(biomass_mt > 0 & population_count > 0) %>%
tidytable::left_join(sp_names) -> plot_dat

# plot regional and subregional indices for stocks with single species code
for(i in 1:length(c(species_t3, species_t5))){

  # get plot data together
  ispp <- c(species_t3, species_t5)[i]
  dat <- plot_dat %>% 
    tidytable::filter(region == 'GOA' & species_code == ispp & biomass_mt > 0 & population_count > 0)
  dat_subreg <- plot_dat %>% 
    tidytable::filter(region != 'GOA' & species_code == ispp & biomass_mt > 0 & population_count > 0) %>% 
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


# create plot comparing og to post-stratified estimates

# set up data by tier and sotck/complex

plot_dat %>% 
  tidytable::filter(species_code %in% species_t3 & region == "GOA") %>% 
  tidytable::mutate(tier = "Tier 3") %>% 
  tidytable::select(year, est_type, biomass_mt, Stock = common_name, tier) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_rebs & region == "GOA") %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                          .by = c(year, est_type)) %>%
    tidytable::mutate(Stock = "Rougheye/Blacspotted complex", tier = "Tier 3")) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_dusky & region == "GOA") %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                     .by = c(year, est_type)) %>%
    tidytable::mutate(Stock = "Dusky rockfish", tier = "Tier 3")) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_t5 & region == "GOA") %>% 
    tidytable::mutate(tier = "Tier 4/5") %>% 
    tidytable::select(year, est_type, biomass_mt, Stock = common_name, tier)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_orox & region == "GOA") %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         .by = c(year, est_type)) %>%
    tidytable::mutate(Stock = "Other rockfish complex", tier = "Tier 4/5")) %>%
  # tidytable::bind_rows(plot_dat %>% 
  #   tidytable::filter(species_code %in% species_swf & region == "GOA") %>% 
  #   tidytable::summarise(biomass_mt = sum(biomass_mt),
  #                        .by = c(year, est_type)) %>%
  #   tidytable::mutate(Stock = "Shallow-water flatfish complex", tier = "Tier 4/5")) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_skate & region == "GOA") %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         .by = c(year, est_type)) %>%
    tidytable::mutate(Stock = "Other skate complex", tier = "Tier 4/5"))  %>% 
  tidytable::mutate(year = case_when(est_type == 'PS' ~ year - 0.25, .default = year),
                    biomass_mt = biomass_mt / 1000) %>% 
  tidytable::pivot_wider(names_from = est_type, values_from = biomass_mt) %>% 
  tidytable::left_join(# get lci
plot_dat %>% 
  tidytable::filter(species_code %in% species_t3 & region == "GOA") %>% 
  tidytable::mutate(tier = "Tier 3") %>% 
  tidytable::select(year, est_type, biomass_lci, Stock = common_name, tier) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_rebs & region == "GOA") %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                        biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                        .by = c(year, est_type)) %>%
    tidytable::mutate(biomass_lci = biomass_mt - 1.96 * sqrt(biomass_var), Stock = "Rougheye/Blacspotted complex", tier = "Tier 3") %>% 
    tidytable::select(-biomass_mt, -biomass_var)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_dusky & region == "GOA") %>% 
      tidytable::summarise(biomass_mt = sum(biomass_mt),
                          biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                          .by = c(year, est_type)) %>%
      tidytable::mutate(biomass_lci = biomass_mt - 1.96 * sqrt(biomass_var), Stock = "Dusky rockfish", tier = "Tier 3") %>% 
      tidytable::select(-biomass_mt, -biomass_var)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_t5 & region == "GOA") %>% 
    tidytable::mutate(tier = "Tier 4/5") %>% 
    tidytable::select(year, est_type, biomass_lci, Stock = common_name, tier)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_orox & region == "GOA") %>% 
      tidytable::summarise(biomass_mt = sum(biomass_mt),
                          biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                          .by = c(year, est_type)) %>%
      tidytable::mutate(biomass_lci = biomass_mt - 1.96 * sqrt(biomass_var), Stock = "Other rockfish complex", tier = "Tier 4/5") %>% 
      tidytable::select(-biomass_mt, -biomass_var)) %>%
  # tidytable::bind_rows(plot_dat %>% 
  #   tidytable::filter(species_code %in% species_swf & region == "GOA") %>% 
  # tidytable::summarise(biomass_mt = sum(biomass_mt),
  #                     biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
  #                     .by = c(year, est_type)) %>%
  # tidytable::mutate(biomass_lci = biomass_mt - 1.96 * sqrt(biomass_var), Stock = "Shallow-water flatfish complex", tier = "Tier 4/5") %>% 
  # tidytable::select(-biomass_mt, -biomass_var)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_skate & region == "GOA") %>% 
      tidytable::summarise(biomass_mt = sum(biomass_mt),
                          biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                          .by = c(year, est_type)) %>%
      tidytable::mutate(biomass_lci = biomass_mt - 1.96 * sqrt(biomass_var), Stock = "Other skate complex", tier = "Tier 4/5") %>% 
      tidytable::select(-biomass_mt, -biomass_var))  %>% 
  tidytable::mutate(year = case_when(est_type == 'PS' ~ year - 0.25, .default = year),
biomass_lci = biomass_lci / 1000) %>% 
  tidytable::pivot_wider(names_from = est_type, values_from = biomass_lci) %>% 
  tidytable::rename(ORIG_lci = ORIG, PS_lci = PS)) %>% 
  tidytable::left_join(# get uci
plot_dat %>% 
  tidytable::filter(species_code %in% species_t3 & region == "GOA") %>% 
  tidytable::mutate(tier = "Tier 3") %>% 
  tidytable::select(year, est_type, biomass_hci, Stock = common_name, tier) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_rebs & region == "GOA") %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                        biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                        .by = c(year, est_type)) %>%
    tidytable::mutate(biomass_hci = biomass_mt + 1.96 * sqrt(biomass_var), Stock = "Rougheye/Blacspotted complex", tier = "Tier 3") %>% 
    tidytable::select(-biomass_mt, -biomass_var)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_dusky & region == "GOA") %>% 
      tidytable::summarise(biomass_mt = sum(biomass_mt),
                          biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                          .by = c(year, est_type)) %>%
      tidytable::mutate(biomass_hci = biomass_mt + 1.96 * sqrt(biomass_var), Stock = "Dusky rockfish", tier = "Tier 3") %>% 
      tidytable::select(-biomass_mt, -biomass_var)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_t5 & region == "GOA") %>% 
    tidytable::mutate(tier = "Tier 4/5") %>% 
    tidytable::select(year, est_type, biomass_hci, Stock = common_name, tier)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_orox & region == "GOA") %>% 
      tidytable::summarise(biomass_mt = sum(biomass_mt),
                          biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                          .by = c(year, est_type)) %>%
      tidytable::mutate(biomass_hci = biomass_mt + 1.96 * sqrt(biomass_var), Stock = "Other rockfish complex", tier = "Tier 4/5") %>% 
      tidytable::select(-biomass_mt, -biomass_var)) %>%
  # tidytable::bind_rows(plot_dat %>% 
  #   tidytable::filter(species_code %in% species_swf & region == "GOA") %>% 
  # tidytable::summarise(biomass_mt = sum(biomass_mt),
  #                     biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
  #                     .by = c(year, est_type)) %>%
  # tidytable::mutate(biomass_hci = biomass_mt - 1.96 * sqrt(biomass_var), Stock = "Shallow-water flatfish complex", tier = "Tier 4/5") %>% 
  # tidytable::select(-biomass_mt, -biomass_var)) %>% 
  tidytable::bind_rows(plot_dat %>% 
    tidytable::filter(species_code %in% species_skate & region == "GOA") %>% 
      tidytable::summarise(biomass_mt = sum(biomass_mt),
                          biomass_var = sum(((biomass_hci - biomass_mt) / 1.96)^2),
                          .by = c(year, est_type)) %>%
      tidytable::mutate(biomass_hci = biomass_mt + 1.96 * sqrt(biomass_var), Stock = "Other skate complex", tier = "Tier 4/5") %>% 
      tidytable::select(-biomass_mt, -biomass_var))  %>% 
  tidytable::mutate(year = case_when(est_type == 'PS' ~ year - 0.25, .default = year),
biomass_hci = biomass_hci / 1000) %>% 
  tidytable::pivot_wider(names_from = est_type, values_from = biomass_hci) %>% 
  tidytable::rename(ORIG_uci = ORIG, PS_uci = PS)) -> comp_dat

ggplot(comp_dat, aes(x = ORIG, y = PS, color = Stock)) +
  geom_point() +
  geom_errorbar(aes(ymin = PS_lci, ymax = PS_uci), width = 0.2, alpha = 0.3) +
  geom_errorbarh(aes(xmin = ORIG_lci, xmax = ORIG_uci), height = 0.2, alpha = 0.3) +
  facet_wrap(~tier, scales = "free", ncol = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
  theme_bw(base_size = 14) +
  labs(x = "Historical bottom trawl survey biomass (thousand mt)", y = "Post-stratified biomass (thousand mt)") +
  scico::scale_color_scico_d(palette = 'roma')

ggsave(filename = "biom_comp.png",
       path = here::here('plots'),
       width = 8,
       height = 8,
       units = "in")

