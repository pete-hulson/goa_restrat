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
  index$og_indx_reg %>% 
    tidytable::filter(species_code %in% species) %>% 
    tidytable::left_join(index$new_indx_reg %>% 
                           tidytable::filter(species_code %in% species) %>% 
                           tidytable::select(year, region, species_code, biom_new = biomass_mt, popn_new = population_count)) %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         biomass_var = sum(biomass_var),
                         population_count = sum(population_count),
                         population_var = sum(population_var),
                         biom_new = sum(biom_new),
                         popn_new = sum(popn_new),
                         .by = c(year, region)) %>% 
    tidytable::mutate(diff_biom = 100 * (biom_new - biomass_mt) / biomass_mt,
                      diff_popn = 100 * (popn_new - population_count) / population_count,
                      biom_test = case_when(biom_new > biomass_mt + 1.96 * sqrt(biomass_var) ~ 1,
                                            biom_new < biomass_mt - 1.96 * sqrt(biomass_var) ~ 1,
                                            .default = 0),
                      popn_test = case_when(popn_new > population_count + 1.96 * sqrt(population_var) ~ 1,
                                            popn_new < population_count - 1.96 * sqrt(population_var) ~ 1,
                                            .default = 0),
                      # cause i just like west to the left/top and east to the right/bottom
                      region = factor(region, levels = c("Western GOA", "Central GOA", "Eastern GOA", "GOA"))) -> db
  
  # subarea biomass time-series plot ----
  biomass_dat <- db %>% 
    tidytable::mutate(og_uci = case_when(year <= 2023 ~ biomass_mt + 1.96 * sqrt(biomass_var),
                                         .default = 0),
                      new_uci = case_when(year >= 2025 ~ biomass_mt + 1.96 * sqrt(biomass_var),
                                          .default = 0),
                      og_lci = case_when(year <= 2023 ~ case_when(biomass_mt - 1.96 * sqrt(biomass_var) > 0 ~ biomass_mt - 1.96 * sqrt(biomass_var),
                                                                  biomass_mt - 1.96 * sqrt(biomass_var) < 0 ~ 0.1),
                                         .default = 0),
                      new_lci = case_when(year >= 2025 ~ case_when(biomass_mt - 1.96 * sqrt(biomass_var) > 0 ~ biomass_mt - 1.96 * sqrt(biomass_var),
                                                                   biomass_mt - 1.96 * sqrt(biomass_var) < 0 ~ 0.1),
                                          .default = 0)) %>% 
    tidytable::select(year, region, Original = biomass_mt, Restratified = biom_new, og_uci, og_lci, new_uci, new_lci) %>% 
    tidytable::pivot_longer(., cols = c(Original, Restratified)) %>% 
    tidytable::mutate(uci = case_when(name == 'Original' & og_uci > 0 ~ og_uci,
                                      name == 'Original' & og_uci == 0 ~ value,
                                      name == 'Restratified' & new_uci > 0 ~ new_uci,
                                      name == 'Restratified' & new_uci == 0 ~ value),
                      lci = case_when(name == 'Original' & og_lci > 0 ~ og_lci,
                                      name == 'Original' & og_lci == 0 ~ value,
                                      name == 'Restratified' & new_lci > 0 ~ new_lci,
                                      name == 'Restratified' & new_lci == 0 ~ value))
  
  biomass_dat %>% 
    tidytable::filter(year < 2025) %>% 
    # do 2025 swap
    tidytable::bind_rows(biomass_dat %>% 
                           tidytable::filter(year == 2025 & name == 'Original') %>% 
                           tidytable::mutate(value = NA) %>% 
                           tidytable::bind_rows(biomass_dat %>% 
                                                  tidytable::filter(year == 2025 & name == 'Restratified') %>% 
                                                  tidytable::select(-value) %>% 
                                                  tidytable::left_join(biomass_dat %>% 
                                                                         tidytable::filter(year == 2025 & name == 'Original') %>% 
                                                                         tidytable::select(year, region, value)))) %>% 
    tidytable::drop_na() %>% 
    tidytable::select(-c(og_uci, og_lci, new_uci, new_lci)) -> biomass_dat
  
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
    tidytable::mutate(og_uci = case_when(year <= 2023 ~ population_count + 1.96 * sqrt(population_var),
                                         .default = 0),
                      new_uci = case_when(year >= 2025 ~ population_count + 1.96 * sqrt(population_var),
                                          .default = 0),
                      og_lci = case_when(year <= 2023 ~ case_when(population_count - 1.96 * sqrt(population_var) > 0 ~ population_count - 1.96 * sqrt(population_var),
                                                                  population_count - 1.96 * sqrt(population_var) < 0 ~ 0.1),
                                         .default = 0),
                      new_lci = case_when(year >= 2025 ~ case_when(population_count - 1.96 * sqrt(population_var) > 0 ~ population_count - 1.96 * sqrt(population_var),
                                                                   population_count - 1.96 * sqrt(population_var) < 0 ~ 0.1),
                                          .default = 0)) %>% 
    tidytable::select(year, region, Original = population_count, Restratified = popn_new, og_uci, og_lci, new_uci, new_lci) %>% 
    tidytable::pivot_longer(., cols = c(Original, Restratified)) %>% 
    tidytable::mutate(uci = case_when(name == 'Original' & og_uci > 0 ~ og_uci,
                                      name == 'Original' & og_uci == 0 ~ value,
                                      name == 'Restratified' & new_uci > 0 ~ new_uci,
                                      name == 'Restratified' & new_uci == 0 ~ value),
                      lci = case_when(name == 'Original' & og_lci > 0 ~ og_lci,
                                      name == 'Original' & og_lci == 0 ~ value,
                                      name == 'Restratified' & new_lci > 0 ~ new_lci,
                                      name == 'Restratified' & new_lci == 0 ~ value))
  
  numbers_dat %>% 
    tidytable::filter(year < 2025) %>% 
    # do 2025 swap
    tidytable::bind_rows(numbers_dat %>% 
                           tidytable::filter(year == 2025 & name == 'Original') %>% 
                           tidytable::mutate(value = NA) %>% 
                           tidytable::bind_rows(numbers_dat %>% 
                                                  tidytable::filter(year == 2025 & name == 'Restratified') %>% 
                                                  tidytable::select(-value) %>% 
                                                  tidytable::left_join(numbers_dat %>% 
                                                                         tidytable::filter(year == 2025 & name == 'Original') %>% 
                                                                         tidytable::select(year, region, value)))) %>% 
    tidytable::drop_na() %>% 
    tidytable::select(-c(og_uci, og_lci, new_uci, new_lci)) -> numbers_dat
    
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
  
  # # % difference in biomass plot
  # db %>% 
  #   tidytable::select(year, region, diff_biom, biom_test) %>% 
  #   tidytable::mutate(perc_diff = scales::percent(diff_biom / 100, accuracy = 1, trim = FALSE),
  #                     pjust = case_when(diff_biom > 0 ~ -0.5,
  #                                       diff_biom < 0 ~ 1),
  #                     biom_test = case_when(biom_test == 0 ~ "plain",
  #                                           biom_test == 1 ~ "bold")) -> plot_dat
  # 
  # biomass_diff <- ggplot(plot_dat, aes(x = year, y = diff_biom, fill = region)) +
  #   geom_bar(stat = "identity") +
  #   geom_text(aes(label = perc_diff, vjust = pjust, fontface = biom_test)) +
  #   facet_wrap(~region, ncol = 1, scales = 'free_y') +
  #   ylim(floor(1.2 * min(plot_dat$diff_biom)), ceiling(1.2 * max(plot_dat$diff_biom))) +
  #   geom_abline(intercept = 0, slope = 0, color = "black") +
  #   scico::scale_fill_scico_d(palette = 'roma') +
  #   theme_bw(base_size = 14) + 
  #   labs(x = "Year", y = "Percent difference in biomass after restratification", fill = "Region", title = stock_name)
  # 
  # ggsave(filename = here::here('plots', paste0(stock_name, '_biom_diff.png')),
  #        plot = biomass_diff,
  #        width = 11,
  #        height = 7,
  #        units = "in")
  # 
  # # % difference in numbers plot
  # db %>% 
  #   tidytable::select(year, region, diff_popn, popn_test) %>% 
  #   tidytable::mutate(perc_diff = scales::percent(diff_popn / 100, accuracy = 1, trim = FALSE),
  #                     pjust = case_when(diff_popn > 0 ~ -0.5,
  #                                       diff_popn < 0 ~ 1),
  #                     biom_test = case_when(popn_test == 0 ~ "plain",
  #                                           popn_test == 1 ~ "bold")) -> plot_dat
  # 
  # numbers_diff <- ggplot(plot_dat, aes(x = year, y = diff_popn, fill = region)) +
  #   geom_bar(stat = "identity") +
  #   geom_text(aes(label = perc_diff, vjust = pjust, fontface = biom_test)) +
  #   facet_wrap(~region, ncol = 1, scales = 'free_y') +
  #   ylim(floor(1.2 * min(plot_dat$diff_popn)), ceiling(1.2 * max(plot_dat$diff_popn))) +
  #   geom_abline(intercept = 0, slope = 0, color = "black") +
  #   scico::scale_fill_scico_d(palette = 'roma') +
  #   theme_bw(base_size = 14) + 
  #   labs(x = "Year", y = "Percent difference in numbers after restratification", fill = "Region", title = stock_name)
  # 
  # ggsave(filename = here::here('plots', paste0(stock_name, '_num_diff.png')),
  #        plot = numbers_diff)
  
  # return plot output
  # list(biomass = biomass,
  #      numbers = numbers,
  #      num_biom = num_biom)

}