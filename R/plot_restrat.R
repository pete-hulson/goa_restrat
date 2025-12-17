#' Plot restratification results
#' 
#' @description
#' Function that plots time-series of orginal indices (with 95% CIs) and restratified indices (for both biomass and numbers)
#' and percent difference between original and restratified. Stores plots in 'plots' folder
#' 
#' @param index list of computed indices, obtained by running the get_index() function (default = NULL)
#' @param species species code for which plots will be computed (can also be a vector for stock complexes, default = NULL)
#' @param stock_name descriptive name for stock, will be included with title and also in plot filename that saved (default = NULL)
#'
#' @return plots with time-series and percent difference
#' 
#' @export
#'
plot_restrat <- function(index = NULL,
                         species = NULL,
                         stock_name = NULL) {

  # start plots folder
  if(!dir.exists(here::here('plots'))){
    dir.create(here::here('plots'))
  }
  
  # get data prepped ----
  index$new_indx_reg %>% 
    tidytable::filter(species_code %in% species) %>% 
    tidytable::rename(biom_new = biomass_mt, biom_var_new = biomass_var, popn_new = population_count, popn_var_new = population_var) %>%
    tidytable::left_join(index$og_indx_reg %>% 
                           tidytable::filter(species_code %in% species) %>% 
                           tidytable::rename(biom_og = biomass_mt, biom_var_og = biomass_var, popn_og = population_count, popn_var_og = population_var)) %>%
    tidytable::left_join(index$ht_indx_reg %>% 
                           tidytable::filter(species_code %in% species) %>% 
                           tidytable::rename(biom_ht = biomass_mt, popn_ht = population_count)) %>% 
    tidytable::mutate(biom_var_ht = NA,
                      popn_var_ht = NA,
                      # cause i just like west to the left/top and east to the right/bottom
                      region = factor(region, levels = c("Western GOA", "Central GOA", "Eastern GOA", "GOA"))) -> db
  
  # subarea biomass time-series plot ----
  biomass_dat <- db %>% 
    tidytable::mutate(og_uci = case_when(!is.na(biom_var_og) ~ biom_og + 1.96 * sqrt(biom_var_og),
                                         .default = 0),
                      new_uci = case_when(!is.na(biom_var_new) ~ biom_new + 1.96 * sqrt(biom_var_new),
                                          .default = 0),
                      og_lci = case_when(!is.na(biom_var_og) ~ biom_og - 1.96 * sqrt(biom_var_og),
                                         .default = 0),
                      new_lci = case_when(!is.na(biom_var_new) ~ biom_new - 1.96 * sqrt(biom_var_new),
                                          .default = 0),) %>% 
    tidytable::select(year, region, Original = biom_og, Restratified = biom_new, HT = biom_ht, og_uci, og_lci, new_uci, new_lci) %>% 
    tidytable::pivot_longer(., cols = c(Original, Restratified, HT)) %>% 
    tidytable::mutate(uci = case_when(name == 'Original' & og_uci > 0 ~ og_uci,
                                      name == 'Original' & og_uci == 0 ~ value,
                                      name == 'Restratified' & new_uci > 0 ~ new_uci,
                                      name == 'Restratified' & new_uci == 0 ~ value,
                                      name == 'HT' ~ value),
                      lci = case_when(name == 'Original' & og_lci > 0 ~ og_lci,
                                      name == 'Original' & og_lci == 0 ~ value,
                                      name == 'Restratified' & new_lci > 0 ~ new_lci,
                                      name == 'Restratified' & new_lci == 0 ~ value,
                                      name == 'HT' ~ value),
                      name = case_when(name == 'HT' ~ 'H-T',
                                       name == 'Restratified' ~ 'Naive',
                                       .default = name)) %>% 
    tidytable::select(-c(og_uci, og_lci, new_uci, new_lci))

  biomass_dat <- biomass_dat %>% 
    filter(year != 2025) %>% 
    tidytable::bind_rows(biomass_dat %>% 
                           filter(year == 2025,
                                  name == 'Naive') %>% 
                           tidytable::mutate(name = 'Restratified')) %>% 
    tidytable::mutate(name = factor(name, levels = c("Original", "H-T", "Naive", "Restratified")))
  
  biomass <- ggplot(data = biomass_dat %>% filter(region != 'GOA'), aes(x = year, y = value / 1000, colour = name, shape = name)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = lci / 1000, ymax = uci / 1000), linewidth = 0.75, size = 0.5) +
    facet_wrap(~region, ncol = 1, scales = 'free_y') +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey biomass (1000s mt)", colour = "Index type", shape = "Index type", title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  ggsave(filename = here::here('plots', paste0(stock_name, '_subreg_biom.png')),
         plot = biomass,
         width = 11,
         height = 7,
         units = "in")
  
  # numbers time-series plot ----
  numbers_dat <- db %>% 
    tidytable::mutate(og_uci = case_when(!is.na(popn_var_og) ~ popn_og + 1.96 * sqrt(popn_var_og),
                                         .default = 0),
                      new_uci = case_when(!is.na(popn_var_new) ~ popn_new + 1.96 * sqrt(popn_var_new),
                                          .default = 0),
                      og_lci = case_when(!is.na(popn_var_og) ~ popn_og - 1.96 * sqrt(popn_var_og),
                                         .default = 0),
                      new_lci = case_when(!is.na(popn_var_new) ~ popn_new - 1.96 * sqrt(popn_var_new),
                                          .default = 0),) %>% 
    tidytable::select(year, region, Original = popn_og, Restratified = popn_new, HT = popn_ht, og_uci, og_lci, new_uci, new_lci) %>% 
    tidytable::pivot_longer(., cols = c(Original, Restratified, HT)) %>% 
    tidytable::mutate(uci = case_when(name == 'Original' & og_uci > 0 ~ og_uci,
                                      name == 'Original' & og_uci == 0 ~ value,
                                      name == 'Restratified' & new_uci > 0 ~ new_uci,
                                      name == 'Restratified' & new_uci == 0 ~ value,
                                      name == 'HT' ~ value),
                      lci = case_when(name == 'Original' & og_lci > 0 ~ og_lci,
                                      name == 'Original' & og_lci == 0 ~ value,
                                      name == 'Restratified' & new_lci > 0 ~ new_lci,
                                      name == 'Restratified' & new_lci == 0 ~ value,
                                      name == 'HT' ~ value),
                      name = case_when(name == 'HT' ~ 'H-T',
                                       name == 'Restratified' ~ 'Naive',
                                       .default = name)) %>% 
    tidytable::select(-c(og_uci, og_lci, new_uci, new_lci))

  numbers_dat <- numbers_dat %>% 
    filter(year != 2025) %>% 
    tidytable::bind_rows(numbers_dat %>% 
                           filter(year == 2025,
                                  name == 'Naive') %>% 
                           tidytable::mutate(name = 'Restratified')) %>% 
    tidytable::mutate(name = factor(name, levels = c("Original", "H-T", "Naive", "Restratified")))

  numbers <- ggplot(data = numbers_dat %>% filter(region != 'GOA'), aes(x = year, y = value / 1000000, colour = name, shape = name)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = lci / 1000000, ymax = uci / 1000000), linewidth = 0.75, size = 0.5) +
    facet_wrap(~region, ncol = 1, scales = 'free_y') +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey numbers (millions)", colour = "Index type", shape = "Index type", title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  ggsave(filename = here::here('plots', paste0(stock_name, '_subreg_num.png')),
         plot = numbers,
         width = 11,
         height = 7,
         units = "in")
  
  
  # biomass & numbers time-series plot ----
  biomass_dat %>% 
    filter(region == 'GOA') %>% 
    tidytable::mutate(index = 'Biomass (1000s mt)',
                      value = value / 1000,
                      uci = uci / 1000,
                      lci = lci / 1000) %>% 
    tidytable::bind_rows(numbers_dat %>% 
                           filter(region == 'GOA') %>% 
                           tidytable::mutate(index = 'Numbers (millions)',
                                             value = value / 1000000,
                                             uci = uci / 1000000,
                                             lci = lci / 1000000)) -> num_biom_dat
  
  num_biom <- ggplot(data = num_biom_dat, aes(x = year, y = value, colour = name, shape = name)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = lci, ymax = uci), linewidth = 0.75, size = 0.5) +
    facet_wrap(~index, ncol = 1, scales = 'free_y') +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey index", colour = "Index type", shape = "Index type", title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')

  ggsave(filename = here::here('plots', paste0(stock_name, '_num_biom_ts.png')),
         plot = num_biom,
         width = 11,
         height = 7,
         units = "in")


}