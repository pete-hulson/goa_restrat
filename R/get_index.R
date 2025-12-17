#' Compute survey indices
#' 
#' @description
#' Function that computes original and restratified GOA survey indices (in biomass and numbers).
#' 
#' @param data list of data provided by the query_data() function (default = NULL)
#' @param new_haul dataset of haul locations that include new strata, obtained by running the restrat() function (default = NULL)
#'
#' @return a list that contains original survey indices at the strata (og_indx_st) and subregion (og_indx_reg) scale
#' and restratified indices at the strata (new_indx_st) and subregion (new_indx_reg) scale.
#' 
#' @export
#'
get_index <- function(data = NULL,
                      new_haul = NULL) {
  
  # prep survey data ----
  cpue_full <- data$cpue %>% 
    tidytable::filter(year != 2025) %>% 
    tidytable::select(year, survey, hauljoin, species_code, numcpue, wtcpue) %>% 
    tidytable::left_join(new_haul) %>% 
    tidytable::mutate(subreg_og = case_when(area_id_og == 803 ~ 'Central GOA',
                                            area_id_og == 804 ~ 'Eastern GOA',
                                            area_id_og == 805 ~ 'Western GOA'),
                      subreg_new = case_when(area_id_new == 803 ~ 'Central GOA',
                                             area_id_new == 804 ~ 'Eastern GOA',
                                             area_id_new == 805 ~ 'Western GOA'))
  
  # add in 2025 data
  cpue_full <- cpue_full %>% 
    tidytable::bind_rows(data$cpue %>% 
                           tidytable::filter(year == 2025) %>% 
                           tidytable::select(year, survey, stratum_new = stratum, hauljoin, species_code, numcpue, wtcpue) %>% 
                           tidytable::left_join(new_haul %>% 
                                                  tidytable::distinct(stratum_new, area_km2_new, area_id_new)) %>% 
                           tidytable::mutate(stratum_og = NA,
                                             area_km2_og = NA,
                                             subreg_og = NA,
                                             n_grid_og = NA,
                                             pinc = NA,
                                             subreg_new = case_when(area_id_new == 803 ~ 'Central GOA',
                                                                    area_id_new == 804 ~ 'Eastern GOA',
                                                                    area_id_new == 805 ~ 'Western GOA')))
  
  # compute og indices ----
  cpue_full %>% 
    tidytable::filter(year != 2025) %>% 
    # compute mean & var in weight cpue by stratum
    tidytable::summarise(mean_wt = mean(wtcpue, na.rm = TRUE),
                         var_wt = stats::var(wtcpue, na.rm = TRUE) / length(stats::na.omit(wtcpue)),
                         .by = c(year, species_code, stratum_og, area_km2_og, subreg_og)) %>% 
    tidytable::left_join(cpue_full %>% 
                           tidytable::filter(year != 2025,
                                             numcpue >= 0) %>% 
                           # compute mean & var in num cpue by stratum
                           tidytable::summarise(mean_num = mean(numcpue, na.rm = TRUE),
                                                var_num = stats::var(numcpue, na.rm = TRUE) / length(stats::na.omit(numcpue)),
                                                .by = c(year, species_code, stratum_og, area_km2_og, subreg_og))) %>% 
    # compute design-based area-weighted index & var by stratum
    tidytable::mutate(biomass_mt = area_km2_og * mean_wt * 0.001,
                      biomass_var = area_km2_og ^ 2 * var_wt * 1e-6,
                      population_count = area_km2_og * mean_num,
                      population_var = area_km2_og ^ 2 * var_num) %>% 
    tidytable::rename(subreg = subreg_og) -> og_indx_st
  
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
                           tidytable::mutate(subreg = 'GOA')) %>% 
    tidytable::rename(region = subreg) -> og_indx_reg
  
  # compute new indices ----
  new_indx_st <- cpue_full %>% 
    tidytable::filter(year != 2025) %>% 
    # compute mean & var in weight cpue by stratum
    tidytable::summarise(mean_wt = mean(wtcpue, na.rm = TRUE),
                         var_wt = NA,
                         .by = c(year, species_code, stratum_new, area_km2_new, subreg_new)) %>% 
    tidytable::left_join(cpue_full %>% 
                           tidytable::filter(year != 2025,
                                             numcpue >= 0) %>% 
                           # compute mean & var in weight cpue by stratum
                           tidytable::summarise(mean_num = mean(numcpue, na.rm = TRUE),
                                                var_num = NA,
                                                .by = c(year, species_code, stratum_new, area_km2_new, subreg_new))) %>% 
    tidytable::drop_na(c(stratum_new, mean_num)) %>% 
    # compute design-based area-weighted index & var by stratum
    tidytable::mutate(biomass_mt = area_km2_new * mean_wt * 0.001,
                      biomass_var = NA,
                      population_count = area_km2_new * mean_num,
                      population_var = NA) %>% 
    tidytable::rename(subreg = subreg_new) %>% 
    tidytable::bind_rows(# add 2025
      cpue_full %>% 
        tidytable::filter(year == 2025) %>% 
        # compute mean & var in cpue by stratum
        tidytable::summarise(mean_wt = mean(wtcpue, na.rm = TRUE),
                             var_wt = stats::var(wtcpue, na.rm = TRUE) / length(stats::na.omit(wtcpue)),
                             mean_num = mean(numcpue, na.rm = TRUE),
                             var_num = stats::var(numcpue, na.rm = TRUE) / length(stats::na.omit(numcpue)),
                             .by = c(year, species_code, stratum_new, area_km2_new, subreg_new)) %>% 
        # compute design-based area-weighted index & var by stratum
        tidytable::mutate(biomass_mt = area_km2_new * mean_wt * 0.001,
                          biomass_var = area_km2_new ^ 2 * var_wt * 1e-6,
                          population_count = area_km2_new * mean_num,
                          population_var = area_km2_new ^ 2 * var_num) %>% 
        tidytable::rename(subreg = subreg_new))
  
  # summarise design-based area-weighted index by goa subregion & total
  new_indx_reg <- new_indx_st %>% 
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
                           tidytable::mutate(subreg = 'GOA')) %>% 
    tidytable::rename(region = subreg) %>% 
    tidytable::mutate(biomass_var = case_when(biomass_var > 0 ~ biomass_var,
                                              .default = NA),
                      population_var = case_when(population_var > 0 ~ population_var,
                                                 .default = NA))
  
  # compute indices based on horvitz-thompson estimator ----
  cpue_full %>% 
    tidytable::filter(year != 2025,
                      !is.na(stratum_new)) %>% 
    # compute number of grid cells and number of grids sampled
    tidytable::mutate(n_grid_new = round(area_km2_new / 25, digits = 0)) %>% 
    tidytable::mutate(n_hl = .N, .by = c(year, species_code, stratum_new)) %>% 
    # filter to > 1 grid sampled within a stratum to compute h-t estimator
    filter(n_hl > 1) %>% 
    tidytable::summarise(num_dens = suppressWarnings(samplingbook::htestimate(y = numcpue,
                                                                              N = unique(n_grid_new),
                                                                              pk = pinc,
                                                                              method = 'hh')$mean),
                         wt_dens = suppressWarnings(samplingbook::htestimate(y = wtcpue,
                                                                             N = unique(n_grid_new),
                                                                             pk = pinc,
                                                                             method = 'hh')$mean),
                         .by = c(year, species_code, stratum_new, area_km2_new, subreg_new)) %>% 
    tidytable::bind_rows(cpue_full %>% 
                           tidytable::filter(year != 2025,
                                             !is.na(stratum_new)) %>% 
                           tidytable::mutate(n_hl = .N, .by = c(year, species_code, stratum_new, subreg_new)) %>% 
                           filter(n_hl == 1) %>% 
                           tidytable::select(year, species_code, stratum_new, area_km2_new, subreg_new, num_dens = numcpue, wt_dens = wtcpue)) %>% 
    tidytable::mutate(population_count = area_km2_new * num_dens,
                      biomass_mt = area_km2_new * wt_dens * 0.001) %>% 
    tidytable::rename(subreg = subreg_new) -> ht_indx_st
  
  ht_indx_st %>% 
    tidytable::summarise(population_count = sum(population_count),
                         biomass_mt = sum(biomass_mt),
                         .by = c(year, species_code, subreg)) %>% 
    tidytable::bind_rows(ht_indx_st %>% 
                           tidytable::summarise(biomass_mt = sum(biomass_mt, na.rm = TRUE),
                                                population_count = sum(population_count, na.rm = TRUE),
                                                .by = c(year, species_code)) %>% 
                           tidytable::mutate(subreg = 'GOA')) %>% 
    tidytable::rename(region = subreg) -> ht_indx_reg
  
  # output return ----
  list(og_indx_st = og_indx_st, 
       og_indx_reg = og_indx_reg,
       new_indx_st = new_indx_st,
       new_indx_reg = new_indx_reg,
       ht_indx_st = ht_indx_st,
       ht_indx_reg = ht_indx_reg)
  
}