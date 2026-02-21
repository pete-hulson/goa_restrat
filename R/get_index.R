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

#' Compute survey indices
#' 
#' @description
#' Function that computes design-based GOA survey indices (in biomass and numbers).
#' 
#' @param data list of data provided by the query_data() function (default = NULL)
#'
#' @return a list that contains survey indices at the strata scale.
#' 
#' @export
#'
get_index_db <- function(data = NULL) {
  
  # compute design-based indices ----
  data$cpue %>% 
    # compute mean & var in cpue by stratum
    tidytable::summarise(mean_wt = mean(wtcpue, na.rm = TRUE),
                         var_wt = stats::var(wtcpue, na.rm = TRUE) / length(stats::na.omit(wtcpue)),
                         mean_num = mean(numcpue, na.rm = TRUE),
                         var_num = stats::var(numcpue, na.rm = TRUE) / length(stats::na.omit(numcpue)),
                         .by = c(year, species_code, stratum)) %>% 
    # join strata info
    tidytable::left_join(data$strata %>% 
                           tidytable::filter(design_year != 1984)) %>% 
    # compute design-based area-weighted index & var by stratum
    tidytable::mutate(biomass_mt = area * mean_wt * 0.001,
                      biomass_var = area ^ 2 * var_wt * 1e-6,
                      population_count = area * mean_num,
                      population_var = area ^ 2 * var_num) %>% 
    # rename subregions
    tidytable::mutate(subreg = case_when(area_id == 803 ~ 'Central GOA',
                                         area_id == 804 ~ 'Eastern GOA',
                                         area_id == 805 ~ 'Western GOA')) %>% 
    # select columns
    tidytable::select(year, species_code, stratum, area_id, subreg, biomass_mt, biomass_var, population_count, population_var) -> db_index

  # output return ----
  db_index
  
}

#' Simulate Design-Based Index via Strategic Subsampling
#'
#' @description
#' This function performs a subsampling simulation to estimate the impact of 
#' reduced sampling effort on survey indices. It supports two modes of 
#' reduction: proportional (percentage-based) or fixed (count-based).
#'
#' @param data A \code{list} containing at least:
#'   \itemize{
#'     \item \code{cpue}: A \code{tidytable} of Catch Per Unit Effort data.
#'     \item \code{strata}: A \code{tidytable} of stratum area definitions.
#'   }
#' @param hauls A \code{tidytable} or \code{data.frame} of station metadata used 
#'   specifically for the fixed-count sampling mode.
#' @param test A \code{numeric} value determining the sampling mode:
#'   \itemize{
#'     \item \code{test < 1}: Interpreted as a proportion. The function retains 
#'     \code{(1 - test)} of stations within each year/stratum group.
#'     \item \code{test >= 1}: Interpreted as a fixed count (\code{n}). The 
#'     function retains exactly \code{test} stations per year.
#'   }
#'
#' @details
#' The function utilizes \code{tidytable::slice_sample} for high-performance 
#' stratified sampling. In proportional mode, sampling is grouped by both 
#' \code{year} and \code{stratum}. In fixed mode, sampling is grouped by 
#' \code{year}. After subsampling, the design-based index is recalculated via 
#' \code{get_index_db()} and aggregated.
#' 
#' @return A \code{tidytable} summarized by \code{year} and \code{species_code} 
#' containing summed biomass, population counts, and their respective variances.
#' 
#' @export
sim_db <- function(data, 
                   hauls,
                   test){
  
  # reduce number of stations proportionally within a strata
  if(test < 1){
    subcpue <- data$cpue %>%
      tidytable::distinct(year, stratum, hauljoin) %>%
      tidytable::slice_sample(prop = 1 - test, by = c(year, stratum)) %>%
      tidytable::left_join(data$cpue)
    sub_data <- list(cpue = subcpue,
                     strata = data$strata)
  } else{
    # reduce number of stations overall based on total number of stations
    slice_sample(hauls,
                 n = n,
                 by = year) %>% 
      tidytable::left_join(data$cpue) -> subcpue
    
    sub_data <- list(cpue = subcpue,
                     strata = data$strata)
  }
  
  # get new index
  sub_index <- get_index_db(sub_data) %>% 
    tidytable::drop_na() %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         biomass_var = sum(biomass_var),
                         population_count = sum(population_count),
                         population_var = sum(population_var),
                         .by = c(year, species_code))
  
  # return
  sub_index
}

#' Calculate Aggregated Species Complex Index
#'
#' @description
#' This function filters a dataset for a specific set of species and aggregates 
#' their biomass and population metrics into a single "complex" index. It is 
#' designed to work with simulation outputs containing multiple iterations 
#' and station sub-samplings.
#'
#' @param data A \code{tidytable} or \code{data.frame} containing simulation results. 
#'   Must include columns: \code{species_code}, \code{biomass_mt}, \code{biomass_var}, 
#'   \code{population_count}, \code{population_var}, \code{iteration}, 
#'   \code{n_stations}, and \code{year} (plus \code{_og} versions of metrics).
#' @param species A \code{numeric} or \code{integer} vector of species codes to be 
#'   included in the complex.
#'
#' @details
#' The function sums both the estimated and "original" (\code{_og}) biomass and 
#' population statistics. The aggregation is performed using \code{tidytable}'s 
#' fast grouping syntax across iterations, station counts, and years. 
#' 
#' \bold{Note:} After aggregation, the \code{species_code} is assigned the 
#' value of the first element in the \code{species} input vector to act as a 
#' placeholder for the complex.
#'
#' @return A \code{tidytable} with one row per \code{iteration}, \code{n_stations}, 
#'   and \code{year}, containing the summed totals for all metrics.
#' 
#' @export
get_cmplx_index <- function(data, species){
  data %>% 
    tidytable::filter(species_code %in% species)  %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         biomass_var = sum(biomass_var),
                         population_count = sum(population_count),
                         population_var = sum(population_var),
                         biomass_mt_og = sum(biomass_mt_og),
                         biomass_var_og = sum(biomass_var_og),
                         population_count_og = sum(population_count_og),
                         population_var_og = sum(population_var_og),
                         .by = c(iteration, subtest, year)) %>% 
    tidytable::mutate(species_code = species[1])
}


