#' Restratify haul locations
#' 
#' @description
#' Function that places historical hauls into the new GOA survey strata. Prior to running this function you will need to 
#' run the query_data() function to obtain the list of necessary data (i.e., hauls) as a function input to restratify the historical hauls.
#' Note: the meat of this script was obtained from Zack Oyafuso in the GAP program
#' 
#' @param data list of data provided by the query_data() function (default = NULL)
#' @param run_terra boolean, if TRUE then runs the terra::extract() function to restratify hauls, 
#' if FALSE, reads in previously run data that is stored in the 'data' folder (default = FALSE). Note: you will need to run this at least once
#'
#' @return a dataset that contains haul information with the original strata as well as the new strata
#' 
#' @export
#'
restrat <- function(data = NULL,
                    run_terra = FALSE) {
  
  if(!dir.exists(here::here('data'))){
    dir.create(here::here('data'))
  }
  
  # reclassify haul station locations for surveys prior to restratification ----
  
  if(isTRUE(run_terra)){
    ## import new stratum shapefile ----
    strata_2025 <- terra::vect(x = here::here("data","goa_strata_2025.gpkg"))
    
    ## Create spatial object with historical stations ----
    haul_locs_lat <- terra::vect(x = data.frame(data$haul),
                                 geom = c("long_st", "lat_st"),
                                 crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    ## Project to same projection as the new strata ----
    haul_locs_aea <- terra::project(x = haul_locs_lat,
                                    terra::crs(x = strata_2025))
    
    ## Reclassify each station to the new strata. Takes a while... ----
    haul_locs_new <- terra::extract(x = strata_2025, y = haul_locs_aea)
    
    ## Attach HAULJOINs ----
    haul_locs_new$hauljoin[haul_locs_new$id.y] <- haul_locs_aea$hauljoin[haul_locs_new$id.y]
    
    ## set up new haul data ----
    new_haul <- data$haul %>% 
      tidytable::filter(year <= 2023) %>% 
      tidytable::rename(stratum_og = stratum) %>% 
      tidytable::left_join(data$strata %>% 
                             filter(design_year == 2024) %>%
                             tidytable::select(survey, stratum_og = stratum, area_km2_og = area)) %>% 
      tidytable::left_join(haul_locs_new %>% 
                             dplyr::rename_all(tolower) %>% 
                             tidytable::select(stratum_new = stratum,
                                               hauljoin,
                                               area_km2_new = area_km2)) %>% 
      tidytable::left_join(data$strata %>% 
                             tidytable::filter(design_year == 2025) %>% 
                             tidytable::select(stratum_new = stratum, area_id_new = area_id)) %>% 
      tidytable::left_join(data$strata %>% 
                             tidytable::filter(design_year == 2024) %>% 
                             tidytable::select(stratum_og = stratum, area_id_og = area_id)) %>% 
      tidytable::select(year, survey, hauljoin, stratum_og, area_km2_og, stratum_new, area_km2_new, area_id_og, area_id_new)

    ## compute inclusion probabilities of og hauls
    new_haul <- new_haul %>% 
      tidytable::left_join(new_haul %>% 
                             tidytable::select(year, survey, hauljoin, stratum_og, area_km2_og) %>% 
                             tidytable::mutate(n_grid_og = round(area_km2_og / 25, digits = 0)) %>% 
                             tidytable::mutate(n_hauls_strat = .N, .by = c(year, survey, stratum_og)) %>% 
                             tidytable::mutate(pinc = n_hauls_strat / n_grid_og) %>% 
                             tidytable::select(year, survey, hauljoin, n_grid_og, pinc))
    
    vroom::vroom_write(new_haul, here::here('data', 'new_haul.csv'), delim = ',')
  } else{new_haul <- vroom::vroom(here::here('data', 'new_haul.csv'), 
                                  progress = FALSE, 
                                  show_col_types = FALSE)}
  
  new_haul
  
}