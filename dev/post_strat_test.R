##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Recalculation of total abundance and biomass of major FMP Gulf of Alaska
##  species after post-stratifying historical stations into new 2025 strata 
##
##  Use the "survey" package to calculate post-stratification weights and
##  post-stratified total abundances and biomass at both regional and Western/
##  Central/Eastern GOA management levels.
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Install/Import libraries
library(gapindex) #devtools::install_github("afsc-gap-products/gapindex")
library(akgfmaps) #devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
library(survey) # install.packages("survey)
library(cowplot)
library(ggplot2)
library(dplyr)
library(purrr)

# source needed fcns
source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Import Data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Import historical (DESIGN_YEAR 2024) and new (DESIGN_YEAR 2025) GOA strata
goa_strata_2025 <- akgfmaps::get_base_layers(select.region = "goa", design.year = 2025, set.crs = "EPSG:4326")$survey.strata
goa_strata_2024 <- akgfmaps::get_base_layers(select.region = "goa", design.year = 2024, set.crs = "EPSG:4326")$survey.strata

## Import species data
spp_codes <- #c(21720, 21740, 30060, 10110, 30420)
  c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 
    30051, 30052, 30150, 30152, 10261, 10262, 10200, 310, 406, 410, 
    420, 425, 435, 440, 445, 450, 455, 460, 471, 472, 475, 477, 480, 
    483, 485, 490, 495, 21230, 10170, 10210, 10220, 10250, 10270, 10285, 
    30020, 30100, 30430, 30475, 30535, 30560, 30576)

# run survey data query
if(!file.exists(here::here('data', 'data.rds'))){
  data <- query_data(spp_codes)
} else{data <- readRDS(here::here('data', 'data.rds'))}

# reassign stations pre-2025 to the new 2025 strata
goa_stations_hist <- data$haul %>% 
  tidytable::filter(year <= 2023) %>% 
  tidytable::mutate(lat = (latitude_dd_start + latitude_dd_end) / 2,
                lon = (longitude_dd_start + longitude_dd_end) / 2) %>%
  tidytable::select(-c(survey_definition_id, latitude_dd_start, latitude_dd_end, longitude_dd_start, longitude_dd_end)) %>% 
  sf::st_as_sf(coords = c("lon", "lat"),
              crs = "EPSG:4326") %>% 
  sf::st_intersection(y = goa_strata_2025[, c("STRATUM")]) %>% 
  tidytable::rename(STRATUM_NEW = STRATUM)

# get original estimates
original_est <- get_index_db(data)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Perform post-stratification ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

new_strata_areas <- data.frame(STRATUM_NEW = data$strata[design_year == 2025]$stratum,
  Freq = data$strata[design_year == 2025]$area)

years <- sort(unique(data$cpue[year != 2025]$year))

#testing
ispp_test = 10200

run_grid <- tidytable::expand_grid(ispp = ispp_test, iyear = years)

test <- purrr::pmap_dfr(run_grid, ~restratify(data, goa_stations_hist, original_est, ispp = ..1, iyear = ..2))

ggplot(test, aes(x = year, y = biomass_mt, color = est_type)) +
  geom_line() +
  geom_point()

restratify <- function(data, goa_stations_hist, original_est, ispp, iyear){

  new_strata_areas <- data.frame(STRATUM_NEW = data$strata[design_year == 2025]$stratum,
    Freq = data$strata[design_year == 2025]$area)
  
  cod <- data$cpue %>% 
      tidytable::filter(species_code == ispp, year == iyear, !is.na(numcpue)) %>% 
      tidytable::select(hauljoin, year, species_code, stratum, wtcpue, numcpue)
    
  if (nrow(cod) == 0) return()
  
  cod2 <- cod %>% 
      tidytable::left_join(goa_stations_hist[, c("hauljoin", "STRATUM_NEW")], by = "hauljoin") %>% 
      tidytable::left_join(data$strata[design_year == 1984, c("stratum", "area")], by = "stratum") %>% 
      tidytable::left_join(new_strata_areas, by = "STRATUM_NEW") %>% 
      tidytable::drop_na()

  if (sum(cod2$wtcpue) == 0) return()
  
  ## Impute stations where stratum effort is 1
  singleton_new_strata <- which(table(cod2$STRATUM_NEW) == 1) %>% names() %>% as.numeric()
  singleton_old_strata <- which(table(cod2$stratum) == 1) %>% names() %>% as.numeric()

  cod2 <- cod2 %>% 
    tidytable::bind_rows(cod2 %>% 
      tidytable::filter(STRATUM_NEW %in% singleton_new_strata | stratum %in% singleton_old_strata))

  # define survey design object with original stratification
  orig_design <- survey::svydesign(
    id = ~1,
    strata = ~stratum,
    data = cod2,
    fpc = ~area)
    
  # define post-stratified survey design object with new stratification and new stratum areas as population sizes
  post_design <- survey::postStratify(
    design = orig_design,
    strata = ~STRATUM_NEW,
    population = new_strata_areas,
    partial = TRUE)
  
  # Calculate post-stratified total biomass estimates with variances
  survey::svyby(formula = ~wtcpue,          
    by = ~STRATUM_NEW,        
    design = post_design,     
    FUN = svytotal) %>% 
      tidytable::mutate(
        stratum = STRATUM_NEW,
        biomass_mt = wtcpue * 0.001,
        biomass_var = se^2 * 1e-6) %>% 
    tidytable::select(stratum, biomass_mt, biomass_var) %>% 
     tidytable::summarise(
      biomass_mt = sum(biomass_mt, na.rm = TRUE),
      biomass_var = sum(biomass_var, na.rm = TRUE)) %>% 
    tidytable::mutate(est_type = "PS",
              year = iyear,
              species_code = ispp) %>% 
    tidytable::bind_rows(
      survey::svyby(formula = ~wtcpue,          
        by = ~stratum,        
        design = orig_design,     
        FUN = svytotal) %>% 
          tidytable::mutate(
            stratum = stratum,
            biomass_mt = wtcpue * 0.001,
            biomass_var = se^2 * 1e-6) %>% 
              tidytable::select(stratum, biomass_mt, biomass_var) %>% 
                tidytable::summarise(
                 biomass_mt = sum(biomass_mt, na.rm = TRUE),
                 biomass_var = sum(biomass_var, na.rm = TRUE)) %>% 
                   tidytable::mutate(est_type = "ORIG",
                             year = iyear,
                             species_code = ispp)) %>%
    tidytable::bind_rows(
      original_est %>% 
        tidytable::filter(year == iyear & species_code == ispp) %>% 
        tidytable::summarise(
          biomass_mt = sum(biomass_mt, na.rm = TRUE),
          biomass_var = sum(biomass_var, na.rm = TRUE)) %>% 
        tidytable::mutate(est_type = "ORIG_DB",
                        year = iyear,
                        species_code = ispp))
}









restrat_df <- data.frame()

for (ispp in spp_codes) {
  for (iyear in years) {
    cod <- data$cpue %>% 
      tidytable::filter(species_code == ispp, year == iyear, !is.na(numcpue)) %>% 
      tidytable::select(hauljoin, year, species_code, stratum, wtcpue, numcpue)
    if (nrow(cod) == 0) next
    cod2 <- cod %>% 
      tidytable::left_join(goa_stations_hist[, c("hauljoin", "STRATUM_NEW")], by = "hauljoin") %>% 
      tidytable::left_join(data$strata[design_year == 1984, c("stratum", "area")], by = "stratum") %>% 
      tidytable::left_join(new_strata_areas, by = "STRATUM_NEW")

    ## Impute stations where stratum effort is 1
    singleton_new_strata <- which(table(cod2$STRATUM_NEW) == 1) %>% names() %>% as.numeric()
    singleton_old_strata <- which(table(cod2$stratum) == 1) %>% names() %>% as.numeric()
    cod2 <- cod2 %>% 
      tidytable::bind_rows(cod2 %>% 
        tidytable::filter(STRATUM_NEW %in% singleton_new_strata | stratum %in% singleton_old_strata))

    orig_design <- survey::svydesign(
      id = ~1,
      strata = ~stratum,
      data = cod2,
      fpc = ~area)
    
    post_design <- survey::postStratify(
      design = orig_design,
      strata = ~STRATUM_NEW,
      population = new_strata_areas,
      partial = TRUE)
    
    
    
    
    
     survey::svyby(formula = ~wtcpue,          
      by = ~STRATUM_NEW,        
      design = post_design,     
      FUN = svytotal) %>% 
        tidytable::mutate(
        stratum = STRATUM_NEW,
        biomass_mt = wtcpue * 0.001,
        biomass_var = se^2 * 1e-6) %>% 
      tidytable::select(stratum, biomass_mt, biomass_var) %>% 
       tidytable::summarise(
        biomass_mt = sum(biomass_mt),
        biomass_var = sum(biomass_var))
    
      survey::svyby(formula = ~wtcpue,          
        by = ~stratum,        
        design = orig_design,     
        FUN = svytotal) %>% 
          tidytable::mutate(
          stratum = stratum,
          biomass_mt = wtcpue * 0.001,
          biomass_var = se^2 * 1e-6) %>% 
        tidytable::select(stratum, biomass_mt, biomass_var) %>% 
         tidytable::summarise(
          biomass_mt = sum(biomass_mt),
          biomass_var = sum(biomass_var))
    
    
    ## Calculate stratum-level mean/var weight CPUE
    survey::svyby(formula = ~wtcpue,          
      by = ~STRATUM_NEW,        
      design = post_design,     
      FUN = svymean) %>% 
    tidytable::mutate(stratum = STRATUM_NEW,
        wtcpue_mean = wtcpue ,
        wtcpue_var = se^2) %>% 
    tidytable::select(stratum, wtcpue_mean, wtcpue_var) %>% 
    tidytable::left_join(    
    ## Calculate stratum-level biomass with variances
      survey::svyby(formula = ~wtcpue,          
        by = ~STRATUM_NEW,        
        design = post_design,     
        FUN = svytotal) %>% 
          tidytable::mutate(
          stratum = STRATUM_NEW,
          biomass_mt = wtcpue * 0.001,
          biomass_var = se^2 * 1e-6) %>% 
        tidytable::select(stratum, biomass_mt, biomass_var),
      by = "stratum") %>% 
    tidytable::left_join(    
      ## Calculate stratum-level mean/var numbers CPUE
      survey::svyby(formula = ~numcpue,          
        by = ~STRATUM_NEW,        
        design = post_design,     
        FUN = svymean) %>% 
      tidytable::mutate(stratum = STRATUM_NEW,
          numcpue_mean = numcpue ,
          numcpue_var = se^2) %>% 
      tidytable::select(stratum, numcpue_mean, numcpue_var),
      by = "stratum") %>% 
      tidytable::left_join(    
        ## Calculate stratum-level abundance with variances
          survey::svyby(formula = ~numcpue,          
            by = ~STRATUM_NEW,        
            design = post_design,     
            FUN = svytotal) %>% 
              tidytable::mutate(
              stratum = STRATUM_NEW,
              population_count = numcpue,
          population_var = se^2) %>% 
            tidytable::select(stratum, population_count, population_var),
          by = "stratum") %>% 
     tidytable::mutate(survey_definition_id = 47,
              survey = "GOA",
              year = iyear,
              species_code = ispp) -> stratum_estimates

    
    stratum_estimates %>% 
      summarise(
        biomass_mt = sum(biomass_mt),
        biomass_var = sum(biomass_var),
        population_count = sum(population_count),
        population_var = sum(population_var)
      )
    
      original_est %>% 
        filter(year == iyear & species_code == ispp) %>% 
          summarise(
            biomass_mt = sum(biomass_mt),
            biomass_var = sum(biomass_var),
            population_count = sum(population_count),
            population_var = sum(population_var)
          )
    
    
    ## Aggregate stratum-level estimates up to the Central/Western/Eastern
    ## management areas as well as GOA-wide
    gp_data_2025$survey$YEAR = iyear
    subarea_estimates <- 
      gapindex::calc_biomass_subarea(gapdata = gp_data_2025,  
                                     biomass_stratum = stratum_estimates) 
    
    ## Add to restrat_df, express variance as 95% CIs. Truncate lower CIs to 
    ## zero if they are negative. 
    restrat_df <- 
      rbind(
        restrat_df, 
        with(subarea_estimates, 
             data.frame(
               YEAR = YEAR,
               SPECIES_CODE = SPECIES_CODE ,
               EST_TYPE = "PS",
               AREA_ID = AREA_ID,
               BIOMASS_MT = BIOMASS_MT,
               BIOMASS_LCI = BIOMASS_MT - sqrt(BIOMASS_VAR) * 1.96,
               BIOMASS_HCI = BIOMASS_MT + sqrt(BIOMASS_VAR) * 1.96,
               POPULATION_COUNT = POPULATION_COUNT,
               POPULATION_LCI = POPULATION_COUNT - sqrt(POPULATION_VAR) * 1.96,
               POPULATION_HCI = POPULATION_COUNT + sqrt(POPULATION_VAR) * 1.96
             )
        ) |>
          transform(
            BIOMASS_LCI = ifelse(test = BIOMASS_LCI < 0, 
                                 yes = 0, 
                                 no = BIOMASS_LCI),
            POPULATION_LCI = ifelse(test = POPULATION_LCI < 0, 
                                    yes = 0, 
                                    no = POPULATION_LCI)
          )
      )
    
  }
  cat("Finished with", ispp, "\n")
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Merge all estimators together
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
all_ests <- 
  rbind(original_est,
        restrat_df) |>
  transform(REGION = dplyr::case_when(
    AREA_ID == 803 ~ "Central GOA",
    AREA_ID == 804 ~ "Eastern GOA",
    AREA_ID == 805 ~ "Western GOA",
    AREA_ID == 99903 ~ "GOA"
  )
  ) |>
  transform(YEAR = dplyr::case_when(
    EST_TYPE == "PS" ~ (YEAR + 0.25),
    .default = YEAR
  )
  ) |>
  subset(subset = !(REGION == "Eastern GOA" & YEAR %in% c(2001, 2001.25)))
all_ests$BIOMASS_LCI <- ifelse(test = all_ests$BIOMASS_LCI < 0, 
                               yes = 0,
                               no = all_ests$BIOMASS_LCI)
all_ests$POPULATION_LCI <- ifelse(test = all_ests$POPULATION_LCI < 0, 
                                  yes = 0,
                                  no = all_ests$POPULATION_LCI)

## Order regions so that it goes from W -> C -> E GOA
all_ests$REGION <- factor(x = all_ests$REGION, 
                          levels = c("GOA", "Western GOA", "Central GOA", "Eastern GOA"))

## Save output
write.csv(x = all_ests,
          file = "plots/all_ests.csv", row.names = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  Output plots
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (ispp in spp_codes) {
  
  stock_name <- 
    gp_data$species$REPORT_NAME_SCIENTIFIC[gp_data$species$SPECIES_CODE == ispp]
  
  if (all_ests %>% 
      filter(REGION != 'GOA' & 
             SPECIES_CODE == ispp) %>% nrow == 0) next 
  
  biomass <- ggplot(
    data = all_ests %>% filter(REGION != 'GOA' & SPECIES_CODE == ispp), 
    aes(x = YEAR, 
        y = BIOMASS_MT / 1000, 
        colour = EST_TYPE, 
        shape = EST_TYPE)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = BIOMASS_LCI / 1000, ymax = BIOMASS_HCI  / 1000), 
                    linewidth = 0.75, 
                    size = 0.5) +
    facet_wrap(facets = ~REGION, 
               ncol = 1, 
               scales = 'free_y') +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey biomass (1000s mt)", 
         colour = "Index type", 
         shape = "Index type", 
         title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  numbers <- ggplot(
    data = all_ests %>% filter(REGION != 'GOA' & SPECIES_CODE == ispp), 
    aes(x = YEAR, y = POPULATION_COUNT / 1000000, 
        colour = EST_TYPE, 
        shape = EST_TYPE)) +
    geom_line(linewidth = 0.75, 
              linetype = "dashed") +
    geom_pointrange(aes(ymin = POPULATION_LCI / 1000000, 
                        ymax = POPULATION_HCI / 1000000), 
                    linewidth = 0.75, 
                    size = 0.5) +
    facet_wrap(~REGION, 
               ncol = 1, 
               scales = 'free_y') +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey numbers (millions)", 
         colour = "Index type", 
         shape = "Index type", 
         title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  ggsave(filename = here::here('plots', 
                               paste0(gsub(x = stock_name, 
                                           pattern = " ", 
                                           replacement = "_"), 
                                      '_subreg_num.png')),
         plot = numbers,
         width = 11, height = 7, units = "in")
  
  
  ggsave(filename = here::here('plots', paste0(gsub(x = stock_name, 
                                                    pattern = " ", 
                                                    replacement = "_"), 
                                               '_subreg_biom.png')),
         plot = biomass,
         width = 11, height = 7, units = "in")
  
  biomass_region <- ggplot(
    data = all_ests %>% filter(REGION == 'GOA' & SPECIES_CODE == ispp), 
    aes(x = YEAR, 
        y = BIOMASS_MT * 1e-3, 
        colour = EST_TYPE, shape = EST_TYPE)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = BIOMASS_LCI * 1e-3, 
                        ymax = BIOMASS_HCI * 1e-3), 
                    linewidth = 0.75, size = 0.5) +
    theme_bw(base_size = 14) + 
    labs(x = "", y = "Survey biomass (thousand mt)", 
         colour = "Index type", shape = "Index type", title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma')
  
  num_region <- ggplot(
    data = all_ests %>% filter(REGION == 'GOA' & 
                                 SPECIES_CODE == ispp & 
                                 POPULATION_COUNT > 0), 
    aes(x = YEAR, 
        y = POPULATION_COUNT * 1e-6, 
        colour = EST_TYPE, shape = EST_TYPE)) +
    geom_line(linewidth = 0.75, linetype = "dashed") +
    geom_pointrange(aes(ymin = POPULATION_LCI * 1e-6, 
                        ymax = POPULATION_HCI * 1e-6), 
                    linewidth = 0.75, size = 0.5) +
    theme_bw(base_size = 14) + 
    labs(x = "Year", y = "Survey numbers (millions)", 
         colour = "Index type", 
         shape = "Index type") +
    scico::scale_color_scico_d(palette = 'roma')
  
  ggsave(filename = here::here('plots', paste0(gsub(x = stock_name, 
                                                    pattern = " ", 
                                                    replacement = "_"), 
                                               '_region.png')),
         plot =   plot_grid(biomass_region, num_region, ncol = 1),
         width = 11, height = 7, units = "in")
}
