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
  data$cpue %>% 
    tidytable::left_join(data$strata %>% 
                           tidytable::select(survey, stratum, area, area_id)) %>% 
    tidytable::mutate(subreg = case_when(area_id == 803 ~ 'Central GOA',
                                         area_id == 804 ~ 'Eastern GOA',
                                         area_id == 805 ~ 'Western GOA')) %>% 
    tidytable::rename(stratum_og = stratum,
                      area_og = area) %>% 
    tidytable::select(-area_id) %>% 
    tidytable::left_join(new_haul) -> cpue_full
  
  # compute og indices ----
  cpue_full %>% 
    # compute mean & var in cpue by stratum
    tidytable::summarise(mean_num = mean(numcpue, na.rm = TRUE),
                         var_num = stats::var(numcpue, na.rm = TRUE) / length(stats::na.omit(numcpue)),
                         mean_wt = mean(wtcpue, na.rm = TRUE),
                         var_wt = stats::var(wtcpue, na.rm = TRUE) / length(stats::na.omit(wtcpue)),
                         .by = c(year, species_code, stratum_og, area_og, subreg)) %>% 
    # compute design-based area-weighted index & var by stratum
    tidytable::mutate(biomass_mt = area_og * mean_wt * 0.001,
                      biomass_var = area_og ^ 2 * var_wt * 1e-6,
                      population_count = area_og * mean_num,
                      population_var = area_og ^ 2 * var_num) -> og_indx_st
  
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
  cpue_full %>% 
    # compute mean & var in cpue by stratum
    tidytable::summarise(mean_num = mean(numcpue, na.rm = TRUE),
                         var_num = stats::var(numcpue, na.rm = TRUE) / length(stats::na.omit(numcpue)),
                         mean_wt = mean(wtcpue, na.rm = TRUE),
                         var_wt = stats::var(wtcpue, na.rm = TRUE) / length(stats::na.omit(wtcpue)),
                         .by = c(year, species_code, stratum_new, area_km2_new, subreg)) %>% 
    tidytable::drop_na() %>% 
    # compute design-based area-weighted index & var by stratum
    tidytable::mutate(biomass_mt = area_km2_new * mean_wt * 0.001,
                      biomass_var = area_km2_new ^ 2 * var_wt * 1e-6,
                      population_count = area_km2_new * mean_num,
                      population_var = area_km2_new ^ 2 * var_num) -> new_indx_st
  
  # summarise design-based area-weighted index & var by goa subregion & total
  new_indx_st %>% 
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
    tidytable::rename(region = subreg) -> new_indx_reg
  
  # output return ----
  list(og_indx_st = og_indx_st, 
       og_indx_reg = og_indx_reg,
       new_indx_st = new_indx_st,
       new_indx_reg = new_indx_reg)
  
  
  
}