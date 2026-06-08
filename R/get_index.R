#' Compute Design-Based Abundance Indices
#'
#' @description
#' Calculates design-based, area-weighted biomass and population abundance indices 
#' and their associated variances by year, species, and stratum. This function is 
#' tailored for stratified-random fishery trawl surveys (specifically optimized for 
#' Gulf of Alaska (GOA) data structures).
#'
#' @param data A named `list` containing at least two data frames or tidytables:
#'   \itemize{
#'     \item \code{cpue}: Must contain columns \code{year}, \code{species_code}, 
#'       \code{stratum}, \code{wtcpue} (weight catch-per-unit-effort), and 
#'       \code{numcpue} (number catch-per-unit-effort).
#'     \item \code{strata}: Must contain stratum metadata including \code{stratum}, 
#'       \code{area}, \code{area_id}, and \code{design_year}.
#'   }
#'   Defaults to \code{NULL}.
#'
#' @details
#' The function executes the following analytical steps:
#' \enumerate{
#'   \item Computes the sample mean and sample variance of the mean for both weight 
#'         and number CPUE within each year-species-stratum combination.
#'   \item Filters out the historical \code{design_year == 1984} from the stratum 
#'         metadata to handle legacy survey design changes.
#'   \item Joins CPUE statistics with stratum areas to scale up the local density metrics.
#'   \item Calculates total design-based metrics using the following area-weighted equations:
#'         \deqn{Biomass (MT) = Area \times \bar{X}_{wt} \times 0.001}
#'         \deqn{Biomass Variance = Area^2 \times Var(\bar{X}_{wt}) \times 10^{-6}}
#'         \deqn{Abundance = Area \times \bar{X}_{num}}
#'         \deqn{Abundance Variance = Area^2 \times Var(\bar{X}_{num})}
#'   \item Appends a \code{subreg} label mapping specific \code{area_id} codes to 
#'         regional management areas (Central GOA, Eastern GOA, Western GOA).
#' }
#'
#' @return A \code{tidytable} containing the computed design-based index with the columns:
#'   \item{year}{Survey year}
#'   \item{species_code}{Unique code representing the species}
#'   \item{stratum}{Stratum identifier}
#'   \item{area_id}{Management area ID number}
#'   \item{subreg}{The regional geographic name (e.g., 'Central GOA')}
#'   \item{biomass_mt}{Total biomass estimate in Metric Tons}
#'   \item{biomass_var}{Variance of the biomass estimate}
#'   \item{population_count}{Total population abundance estimate in numbers}
#'   \item{population_var}{Variance of the abundance estimate}
#'
#' @importFrom tidytable summarise left_join filter mutate case_when select
#' @importFrom stats var na.omit
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Example list structure
#' survey_data <- list(
#'   cpue = data.frame(year = 2025, species_code = 21740, stratum = 10, wtcpue = 12.5, numcpue = 5),
#'   strata = data.frame(stratum = 10, area = 4500, area_id = 803, design_year = 2025)
#' )
#' 
#' # Run index calculation
#' biomass_index <- get_index_db(data = survey_data)
#' }
get_index_db <- function(data = NULL) {
  
  # compute design-based indices
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
  
  # output return
  db_index
  
}

#' Perform Post-Stratification on Survey Data
#'
#' @description
#' This function performs a post-stratification analysis on catch-per-unit-effort (CPUE) 
#' data using the 2025 stratification design. It extracts the 2025 stratum area configurations, 
#' identifies all unique combinations of species and years present in the dataset, and 
#' maps over them to calculate restratified index values.
#'
#' @param data A named list containing at least two data frames or tidytables: 
#'   `strata` (containing columns `design_year`, `stratum`, and `area`) and 
#'   `cpue` (containing columns `species_code` and `year`). Defaults to `NULL`.
#' @param goa_stations_hist Data object containing historical station allocation or location data 
#'   passed directly to the underlying \code{\code{\link{restratify}}} function.
#'
#' @return A \code{tidytable} (or data frame) containing the combined post-stratified index results 
#'   across all species and years, generated iteratively by \code{\code{\link{restratify}}}.
#'
#' @export
#'
#' @importFrom tidytable tidytable distinct
#' @importFrom purrr pmap_dfr
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' index_results <- get_index_ps(
#'   data = survey_data_list,
#'   goa_stations_hist = historical_stations
#' )
#' }
get_index_ps <- function(data = NULL, goa_stations_hist){
  
  # perform post-stratification
  new_strata_areas <- tidytable::tidytable(STRATUM_NEW = data$strata[design_year == 2025]$stratum,
                                           Freq = data$strata[design_year == 2025]$area)
  
  run_grid <- data$cpue %>% 
    tidytable::distinct(species_code, year)
  
  purrr::pmap_dfr(run_grid, ~restratify(data, goa_stations_hist, new_strata_areas, ispp = ..1, iyear = ..2))
  
}


#' Restratify Survey Data and Calculate Post-Stratified Index Estimates
#'
#' @description
#' This function takes survey catch-per-unit-effort (CPUE) data for a single species and year, 
#' maps it from historical strata configurations into a new 2025 stratification schema, handles 
#' singleton strata via statistical imputation, and calculates post-stratified biomass and 
#' population count estimates with their corresponding variances using the \code{survey} package.
#'
#' @details
#' The function executes the following sequence:
#' \enumerate{
#'   \item \textbf{Filtering:} Subsets CPUE data to the specified species and year, keeping valid non-negative data.
#'   \item \textbf{Data Joining:} Pairs historical hauls with the new stratum IDs (\code{STRATUM_NEW}), filters baseline stratum definitions from 2024, and pairs them with 2025 areas.
#'   \item \textbf{Singleton Imputation:} Identifies strata (old or new) containing only a single station (a "singleton"). It duplicates these stations into the dataset as a proxy mechanism to ensure the \code{survey} package has sufficient degrees of freedom to calculate variance without crashing.
#'   \item \textbf{Survey Estimation:} Constructs an original \code{\code{\link[survey]{svydesign}}}, projects it via \code{\code{\link[survey]{postStratify}}} onto the new 2025 population areas, and extracts weighted total calculations via \code{\code{\link[survey]{svyby}}}.
#'   \item \textbf{Regional Aggregation:} Groups the calculated metrics into structural operational areas of interest (Eastern GOA, Central GOA, Western GOA, and an overall GOA total).
#' }
#'
#' @param data A named list containing at least:
#'   \itemize{
#'     \item \code{cpue}: A data frame/tidytable with columns \code{species_code}, \code{year}, \code{numcpue}, \code{wtcpue}, \code{hauljoin}, and \code{stratum}.
#'     \item \code{strata}: A data frame/tidytable with columns \code{design_year}, \code{stratum}, \code{area}, and \code{area_id}.
#'   }
#' @param goa_stations_hist A data frame or matrix mapping historical hauls to new stratum boundaries. Must contain columns \code{hauljoin} and \code{STRATUM_NEW}.
#' @param new_strata_areas A data frame or tidytable defining target population weights for post-stratification. Must contain columns \code{STRATUM_NEW} and \code{Freq}.
#' @param ispp Numeric or Character. The target species code to filter and analyze.
#' @param iyear Numeric. The target calendar year to filter and analyze.
#'
#' @return A \code{tidytable} containing aggregated post-stratified metrics for the specified species and year across regional areas. Columns include:
#'   \itemize{
#'     \item \code{year}: The evaluated iteration year (\code{iyear}).
#'     \item \code{species_code}: The evaluated iteration species (\code{ispp}).
#'     \item \code{area_id}: Character labels indicating regional breakdowns (\code{"Eastern GOA"}, \code{"Central GOA"}, \code{"Western GOA"}, or \code{"GOA"}).
#'     \item \code{biomass_mt}: Estimated biomass in metric tons.
#'     \item \code{biomass_var}: Variance of the biomass estimate.
#'     \item \code{population_count}: Estimated population abundance count.
#'     \item \code{population_var}: Variance of the population count estimate.
#'   }
#'   Returns \code{NULL} implicitly if no records or zero biomass are found for the given combination.
#'
#' @export
#' 
#' @importFrom tidytable filter select left_join drop_na summarise mutate case_when bind_rows
#' @importFrom survey svydesign postStratify svyby svytotal
#'
restratify <- function(data, goa_stations_hist, new_strata_areas, ispp, iyear){
  
  # filter to species and year of interest, and join to new strata
  cod <- data$cpue %>% 
    tidytable::filter(species_code == ispp, year == iyear, !is.na(numcpue)) %>% 
    tidytable::select(hauljoin, year, species_code, stratum, wtcpue, numcpue) %>% 
    tidytable::filter(wtcpue >= 0,
                      numcpue >= 0)
  
  if (nrow(cod) == 0) return()
  
  cod2 <- cod %>% 
    tidytable::left_join(as.data.frame(goa_stations_hist)[, c("hauljoin", "STRATUM")], by = "hauljoin") %>% 
    tidytable::rename(STRATUM_NEW = STRATUM) %>% 
    tidytable::left_join(data$strata[design_year == 2024, c("stratum", "area")], by = "stratum") %>% 
    tidytable::left_join(new_strata_areas, by = "STRATUM_NEW") %>% 
    tidytable::drop_na()
  
  if (sum(cod2$wtcpue) == 0) return()
  
  ## Impute stations where stratum effort is 1
  singleton_new_strata <- which(table(cod2$STRATUM_NEW) == 1) %>% names() %>% as.numeric()
  singleton_old_strata <- which(table(cod2$stratum) == 1) %>% names() %>% as.numeric()
  cod2 <- cod2 %>% 
    tidytable::bind_rows(cod2 %>% 
                           tidytable::filter(STRATUM_NEW %in% singleton_new_strata | stratum %in% singleton_old_strata))

  # filter out strata with no stations (after imputation)
  new_strata_areas_real <- new_strata_areas %>% 
    tidytable::left_join(cod2 %>% 
                           tidytable::summarise(n = .N, .by = STRATUM_NEW)) %>% 
    tidytable::drop_na() %>%
    tidytable::select(-n)
  
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
    population = new_strata_areas_real,
    partial = TRUE)

  # Calculate post-stratified index estimates with variances
  stratum_biom <- survey::svyby(formula = ~wtcpue,          
                by = ~STRATUM_NEW,        
                design = post_design,     
                FUN = svytotal) %>% 
    tidytable::mutate(
      stratum = STRATUM_NEW,
      biomass_mt = wtcpue * 0.001,
      biomass_var = se^2 * 1e-6) %>% 
    tidytable::select(stratum, biomass_mt, biomass_var)
  
  stratum_num <- survey::svyby(formula = ~numcpue,          
                               by = ~STRATUM_NEW,        
                               design = post_design,     
                               FUN = svytotal) %>% 
    tidytable::mutate(
      stratum = STRATUM_NEW,
      population_count = numcpue,
      population_var = se^2) %>% 
    tidytable::select(stratum, population_count, population_var)
  
  # summarize by goa areas
  stratum_biom %>% 
    tidytable::left_join(data$strata %>% 
                           tidytable::filter(design_year == 2025)) %>% 
    tidytable::summarise(
      biomass_mt = sum(biomass_mt, na.rm = TRUE),
      biomass_var = sum(biomass_var, na.rm = TRUE),
      .by = area_id) %>% 
    tidytable::mutate(subreg = tidytable::case_when(area_id == 803 ~ "Eastern GOA",
                                                     area_id == 804 ~ "Central GOA",
                                                     area_id == 805 ~ "Western GOA")) %>% 
    tidytable::bind_rows(stratum_biom %>% 
                           tidytable::summarise(
                             biomass_mt = sum(biomass_mt, na.rm = TRUE),
                             biomass_var = sum(biomass_var, na.rm = TRUE)) %>% 
                           tidytable::mutate(area_id = 99903, subreg = "GOA")) %>% 
    tidytable::left_join(stratum_num %>% 
                           tidytable::left_join(data$strata %>% 
                                                  tidytable::filter(design_year == 2025)) %>% 
                           tidytable::summarise(
                             population_count = sum(population_count, na.rm = TRUE),
                             population_var = sum(population_var, na.rm = TRUE),
                             .by = area_id) %>% 
                           tidytable::mutate(subreg = tidytable::case_when(area_id == 803 ~ "Eastern GOA",
                                                                            area_id == 804 ~ "Central GOA",
                                                                            area_id == 805 ~ "Western GOA")) %>% 
                           tidytable::bind_rows(stratum_num %>% 
                                                  tidytable::summarise(
                                                    population_count = sum(population_count, na.rm = TRUE),
                                                    population_var = sum(population_var, na.rm = TRUE)) %>% 
                                                  tidytable::mutate(area_id = 99903, subreg = "GOA"))) %>% 
    tidytable::mutate(
      year = iyear,
      species_code = ispp) %>% 
    tidytable::select(year, species_code, area_id, subreg, biomass_mt, biomass_var, population_count, population_var)

}

#' Simulate Survey Index Under Reduced Sampling Scenarios
#'
#' @description
#' This function simulates a reduced-effort fisheries survey design. It sub-samples historical 
#' and 2025 survey hauls down to a target effort threshold (\code{test}) using proportional 
#' allocation across strata. It then calculates and returns comparative index estimates 
#' using baseline design-based, post-stratified, and updated 2025 estimation methods.
#'
#' @details
#' The simulation follows a structured process:
#' \enumerate{
#'   \item \textbf{Survey Filtering:} Keeps only the survey years where the absolute number of historical hauls meets or exceeds the \code{test} sample size threshold.
#'   \item \textbf{Proportional Allocation:} Calculates the historical distribution of haul density (\code{p_haul}) across strata within each year, and allocates the target \code{test} number of hauls to each stratum using this ratio.
#'   \item \textbf{Stratified Sub-sampling:} Groups and nests the CPUE data, applying \code{\code{\link[purrr]{map2}}} and \code{\code{\link[tidytable]{slice_sample}}} to randomly draw the allocated number of hauls without replacement.
#'   \item \textbf{Data Splitting:} Generates partitioned sub-sampled datasets separating historical years (\code{< 2025}) from the target evaluation year (\code{== 2025}).
#'   \item \textbf{Index Evaluation:} Computes three distinct population indices from the sub-sampled data for comparison: Design-Based Historical, Post-Stratified Historical, and Design-Based 2025.
#' }
#'
#' @param data A named list containing master biological and survey design datasets:
#'   \itemize{
#'     \item \code{cpue}: Data frame/tidytable containing columns \code{year}, \code{stratum}, \code{hauljoin}, and species catch metrics.
#'     \item \code{haul}: Data frame/tidytable containing baseline environmental/station haul info.
#'     \item \code{strata}: Data frame/tidytable defining stratum population metrics.
#'   }
#' @param hauls A data frame/tidytable containing station metadata used to calculate the baseline annual haul effort via counts (\code{.N}).
#' @param test Integer. The total target number of stations (hauls) to down-sample the annual survey to.
#' @param goa_stations_hist Data object passed directly to \code{\code{\link{get_index_ps}}} to handle post-stratification geometry mapping.
#'
#' @return A \code{tidytable} containing combined, aggregated index estimations for all sub-sampled parameters. Columns include:
#'   \itemize{
#'     \item \code{year}: The survey calendar year.
#'     \item \code{species_code}: The taxonomic identifier for the species.
#'     \item \code{area_id}: Regional identifier label (included if returning from \code{get_index_ps}).
#'     \item \code{biomass_mt}: Summed biomass estimate in metric tons.
#'     \item \code{biomass_var}: Summed variance of the biomass estimate.
#'     \item \code{population_count}: Summed numerical abundance estimate.
#'     \item \code{population_var}: Summed variance of the abundance estimate.
#'     \item \code{est_type}: Character flag denoting the calculation framework (\code{"Historical"}, \code{"Post-stratified"}, or \code{"2025"}).
#'   }
#'
#' @export
#'
#' @importFrom tidytable summarise filter distinct mutate select left_join drop_na arrange nest unnest bind_rows case_when
#' @importFrom purrr map2
#' @importFrom dplyr slice_sample
#'
sim_db <- function(data, 
                   hauls,
                   test,
                   goa_stations_hist){
  
  # reduce number of stations overall based on total number of stations
  surveys = tidytable::summarise(hauls, n = .N, .by = year) %>% 
    tidytable::filter(n >= test)
  
  # allocate to strata based on historical haul density, then sample within strata
  samp_haul_dat <- data$cpue %>% 
    tidytable::filter(year %in% surveys$year) %>%
    tidytable::distinct(year, stratum, hauljoin) %>% 
    tidytable::summarise(n_haul = .N,
                         .by = c(year, stratum)) %>% 
    tidytable::mutate(p_haul = n_haul / sum(n_haul),
                      .by = year) %>% 
    tidytable::mutate(samp_haul = round(p_haul * test, digits = 0)) %>% 
    tidytable::select(year, stratum, samp_haul)
  
  subcpue <- data$cpue %>%
    tidytable::distinct(year, stratum, hauljoin) %>% 
    tidytable::left_join(samp_haul_dat) %>% 
    tidytable::drop_na() %>%
    tidytable::arrange(year) %>%
    tidytable::nest(.by = c(year, stratum, samp_haul)) %>% 
    tidytable::mutate(data = map2(data, samp_haul, ~ slice_sample(.x, n = .y))) %>% 
    tidytable::unnest(data) %>%
    tidytable::left_join(data$cpue)
  
  subhaul <- data$haul %>% 
    tidytable::left_join(
      subcpue %>% 
        tidytable::distinct(year, hauljoin) %>% 
        tidytable::mutate(selected = 1)) %>%
    tidytable::filter(selected == 1) %>%
    tidytable::select(-selected)
  
  sub_data_pre <- list(
    cpue = subcpue %>% tidytable::filter(year < 2025),
    haul = subhaul %>% tidytable::filter(year < 2025),
    strata = data$strata)
  
  sub_data_25 <- list(
    cpue = subcpue %>% tidytable::filter(year == 2025),
    haul = subhaul %>% tidytable::filter(year == 2025),
    strata = data$strata)
  
  # get sub-sampled index
  sub_index <- get_index_db(sub_data_pre) %>% # historical index with reduced stations
    tidytable::drop_na() %>% 
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         biomass_var = sum(biomass_var),
                         population_count = sum(population_count),
                         population_var = sum(population_var),
                         .by = c(year, species_code, area_id, subreg)) %>% 
    tidytable::bind_rows(get_index_db(sub_data_pre) %>% # historical index with reduced stations
                           tidytable::drop_na() %>% 
                           tidytable::summarise(biomass_mt = sum(biomass_mt),
                                                biomass_var = sum(biomass_var),
                                                population_count = sum(population_count),
                                                population_var = sum(population_var),
                                                .by = c(year, species_code)) %>% 
                           tidytable::mutate(area_id = 99903, subreg = "GOA")) %>% 
    tidytable::mutate(est_type = 'Historical') %>% 
    tidytable::bind_rows(get_index_ps(sub_data_pre, goa_stations_hist) %>% # post-stratified index with reduced stations
                           tidytable::mutate(est_type = 'Post-stratified')) %>% 
    tidytable::bind_rows(get_index_db(sub_data_25) %>% # historical index with reduced stations
                           tidytable::drop_na() %>% 
                           tidytable::summarise(biomass_mt = sum(biomass_mt),
                                                biomass_var = sum(biomass_var),
                                                population_count = sum(population_count),
                                                population_var = sum(population_var),
                                                .by = c(year, species_code, area_id, subreg)) %>% 
                           tidytable::bind_rows(get_index_db(sub_data_25) %>% # historical index with reduced stations
                                                  tidytable::drop_na() %>% 
                                                  tidytable::summarise(biomass_mt = sum(biomass_mt),
                                                                       biomass_var = sum(biomass_var),
                                                                       population_count = sum(population_count),
                                                                       population_var = sum(population_var),
                                                                       .by = c(year, species_code)) %>% 
                                                  tidytable::mutate(area_id = 99903, subreg = "GOA")) %>% 
                           tidytable::mutate(est_type = '2025'))
  
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
                         .by = c(iteration, subtest, year, est_type)) %>% 
    tidytable::mutate(species_code = species[1])
}


