#' Compare computed indices with gap
#' 
#' @description
#' Function that compares between internally computed indices with those produced by the gap program and posted to akfin.
#' This flags when differences in computed indices and gap indices are, on average across the surveys, greater than 0.1%.
#' 
#' @param data list of data provided by the query_data() function (default = NULL)
#' @param index list of computed indices, obtained by running the get_index() function (default = NULL)
#'
#' @return a message stating whether computed indices match, or not
#' 
#' @export
#'
compare_index <- function(data = NULL,
                          index = NULL) {
  
  # compare with gap indices
  index$og_indx_st %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt, na.rm = TRUE),
                         biomass_var = sum(biomass_var, na.rm = TRUE),
                         population_count = sum(population_count, na.rm = TRUE),
                         population_var = sum(population_var, na.rm = TRUE),
                         .by = c(year, species_code)) %>% 
    tidytable::left_join(data$index %>% 
                           tidytable::rename(biomass_mt_gap = biomass_mt,
                                             biomass_var_gap = biomass_var,
                                             population_count_gap = population_count,
                                             population_var_gap = population_var) %>% 
                           tidytable::select(-area_id)) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(diff_biom = case_when(biomass_mt_gap > 0 ~ (biomass_mt_gap - biomass_mt) / biomass_mt_gap,
                                            .default = 0),
                      diff_biom_var = case_when(biomass_var_gap > 0 ~ (biomass_var_gap - biomass_var) / biomass_var_gap,
                                                .default = 0),
                      diff_popn = case_when(population_count_gap > 0 ~ (population_count_gap - population_count) / population_count_gap,
                                            .default = 0),
                      diff_popn_var = case_when(population_var_gap > 0 ~ (population_var_gap - population_var) / population_var_gap,
                                                .default = 0)) %>% 
    tidytable::summarise(diff_biom = mean(diff_biom),
                         diff_biom_var = mean(diff_biom_var),
                         diff_popn = mean(diff_popn),
                         diff_popn_var = mean(diff_popn_var),
                         .by = c(species_code)) -> compare
  
  # flag if average computed differences > 0.1%
  compare %>% 
    tidytable::filter(abs(diff_biom) > 0.01) -> biom_flag
  compare %>% 
    tidytable::filter(abs(diff_popn) > 0.01) -> popn_flag
  
  # print message
  if(length(biom_flag$diff_biom) == 0 & length(popn_flag$diff_popn) == 0){
    cat(crayon::green$bold("Good to go"), "\u2714", "\n")
  }else{
    cat("\u274c", crayon::red$bold("STOP: check out computed indices for species"), crayon::italic(popn_flag$species_code), "\n")
  }
  
}