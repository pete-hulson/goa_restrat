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

#' Post-Stratify Historical Survey Data to a New Spatial Stratification Grid
#'
#' @description
#' Spatially reassigns historical survey haul stations (pre-2025) into a new 
#' 2025 stratum framework using geographic intersection. It then constructs a 
#' population area frequency framework and map-iterates over species-year 
#' combinations to compute post-stratified abundance indices.
#'
#' @param data A named \code{list} containing at least two data frames or tidytables:
#'   \itemize{
#'     \item \code{haul}: Historical haul metadata containing \code{year}, 
#'           \code{latitude_dd_start}, \code{latitude_dd_end}, \code{longitude_dd_start}, 
#'           \code{longitude_dd_end}, and \code{survey_definition_id}.
#'     \item \code{strata}: Stratum metadata containing columns \code{design_year}, 
#'           \code{stratum}, and \code{area}.
#'     \item \code{cpue}: Catch-per-unit-effort data containing at least \code{species_code} 
#'           and \code{year} used to build the iterative estimation grid.
#'   }
#'   Defaults to \code{NULL}.
#' @param goa_strata_2025 An \code{sf} (simple features) spatial object representing the 
#'   updated 2025 Gulf of Alaska (GOA) stratification grid. Must include a \code{STRATUM} 
#'   attribute column matching the spatial geometries.
#'
#' @details
#' The function processes through three main procedural steps:
#' \enumerate{
#'   \item \strong{Spatial Overlay:} Filters hauls up to the year 2023, calculates 
#'         midpoint coordinates for each haul sequence, converts them into spatial 
#'         points (\code{EPSG:4326}), and runs a geometric intersection (\code{sf::st_intersection}) 
#'         against the 2025 strata boundaries. This explicitly assigns historical hauls to a \code{STRATUM_NEW} label.
#'   \item \strong{Area Frequency Setup:} Creates a reference frame (\code{new_strata_areas}) 
#'         mapping the 2025 strata IDs to their total spatial area sizes (\code{Freq}), 
#'         which is required for survey post-stratification expansion weights.
#'   \item \strong{Mapping Execution:} Generates a unique cross-grid of all \code{species_code} 
#'         and \code{year} records inside the CPUE dataset and iteratively processes them 
#'         using \code{purrr::pmap_dfr()} via a sub-function called \code{restratify()}.
#' }
#'
#' @return A \code{tidytable} or data frame combining row-bound results across all 
#'   mapped species and years returned by the underlying \code{restratify()} function.
#'
#' @importFrom tidytable filter mutate select rename tidytable distinct
#' @importFrom sf st_as_sf st_intersection
#' @importFrom purrr pmap_dfr
#' 
#' @note 
#' This function relies closely on an external or unexported helper function 
#' named \code{restratify(data, goa_stations_hist, ispp, iyear)}. Ensure that this 
#' function is loaded in your active workspace environment.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming 'survey_list' matches required structures and 'goa_sf' is an sf polygon layer
#' ps_index <- get_index_ps(data = survey_list, goa_strata_2025 = goa_sf)
#' }
get_index_ps <- function(data = NULL, goa_strata_2025){

  # reassign stations pre-2025 to the new 2025 strata
  goa_stations_hist <- data$haul %>% 
    tidytable::filter(year <= 2023) %>% 
    tidytable::mutate(lat = (latitude_dd_start + latitude_dd_end) / 2,
                      lon = (longitude_dd_start + longitude_dd_end) / 2) %>%
    tidytable::select(-c(survey_definition_id, latitude_dd_start, latitude_dd_end, longitude_dd_start, longitude_dd_end)) %>% 
    sf::st_as_sf(coords = c("lon", "lat"), crs = "EPSG:4326") %>% 
    sf::st_intersection(y = goa_strata_2025[, c("STRATUM")]) %>% 
    tidytable::rename(STRATUM_NEW = STRATUM)

  # perform post-stratification
  new_strata_areas <- tidytable::tidytable(STRATUM_NEW = data$strata[design_year == 2025]$stratum,
    Freq = data$strata[design_year == 2025]$area)

  run_grid <- data$cpue %>% 
    tidytable::distinct(species_code, year)

  purrr::pmap_dfr(run_grid, ~restratify(data, goa_stations_hist, new_strata_areas, ispp = ..1, iyear = ..2))

}

#' Core Post-Stratification Worker for a Single Species-Year Combination
#'
#' @description
#' Sub-wrapper function called dynamically within a loop or mapping function 
#' (e.g., \code{purrr::pmap_dfr}) to calculate post-stratified survey totals 
#' (biomass and population abundance) and variances for a unique species 
#' and year intersection.
#'
#' @param data A named \code{list} containing at least two data frames or tidytables:
#'   \itemize{
#'     \item \code{cpue}: Catch-per-unit-effort data frame containing \code{species_code}, 
#'           \code{year}, \code{hauljoin}, \code{stratum}, \code{wtcpue}, and \code{numcpue}.
#'     \item \code{strata}: Historical stratum metadata containing columns \code{design_year}, 
#'           \code{stratum}, and \code{area}.
#'   }
#' @param goa_stations_hist A spatial overlay data frame or spatial object linking historic 
#'   trawl hauls to new management frameworks. It must contain the key columns \code{hauljoin} 
#'   and \code{STRATUM_NEW}.
#' @param new_strata_areas A data frame or tidytable tracking the modern stratification 
#'   sizes, containing at least \code{STRATUM_NEW} and \code{Freq} (representing stratum areas).
#' @param ispp An \code{integer} or \code{numeric} code representing the target species filter.
#' @param iyear An \code{integer} representing the target survey year filter.
#'
#' @details
#' The calculation workflow includes the following steps:
#' \enumerate{
#'   \item \strong{Subsetting & Validation:} Filters CPUE data down to the \code{ispp}/\code{iyear} combo. 
#'         If no records exist or if total weight catch is exactly zero, the function safely exits and returns \code{NULL}.
#'   \item \strong{Multi-way Joins:} Links the filtered catch data with the new spatial stratum tracking 
#'         (\code{goa_stations_hist}), the legacy 1984 stratum area sizes (used for Finite Population Correction \code{fpc}), 
#'         and the passed population stratification size frequencies (\code{new_strata_areas}).
#'   \item \strong{Singleton Stratum Imputation:} A vital statistical step. If an active old or new stratum 
#'         contains only a single station (sample size $n=1$), variance calculation mathematically drops. 
#'         The function flags these "singletons" and duplicates them via a \code{bind_rows()} copy step to force an 
#'         artificial sample size calculation threshold.
#'   \item \strong{Survey Design Compilation:} Initializes a baseline \code{survey::svydesign()} tracking 
#'         historical strata, and updates it into a post-stratified framework using \code{survey::postStratify()} 
#'         mapped against the new stratum configurations.
#'   \item \strong{Aggregation:} Runs standard \code{survey::svyby(..., FUN = svytotal)} expansions for both 
#'         \code{wtcpue} and \code{numcpue}, groups totals across the geographic area, converts units (Weight to Metric Tons), 
#'         and flattens the result into a clean summary.
#' }
#'
#' @return A \code{tidytable} with a single row corresponding to the post-stratified (\code{est_type = "PS"}) 
#'   biomass estimate, variance, population count, and population count variance for the evaluated 
#'   species and year. Returns \code{NULL} (or an empty expression) if zero rows or zero total catches are met.
#' 
#' @note 
#' Ensure that you explicitly update the \code{FUN = svytotal} calls within this function to 
#' \code{FUN = survey::svytotal} unless the \code{survey} library namespace is loaded globally 
#' into your R session environment via \code{library(survey)}.
#'
#' @importFrom tidytable filter select left_join drop_na bind_rows summarise mutate
#' @importFrom survey svydesign postStratify svyby
#' 
#' @export
#'
#' @seealso \code{\link{get_index_ps}}
restratify <- function(data, goa_stations_hist, new_strata_areas, ispp, iyear){

  # filter to species and year of interest, and join to new strata
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
  tidytable::mutate(
    year = iyear,
    species_code = ispp) %>% 
  tidytable::left_join(
    survey::svyby(formula = ~numcpue,          
      by = ~STRATUM_NEW,        
      design = post_design,     
      FUN = svytotal) %>% 
    tidytable::mutate(
      stratum = STRATUM_NEW,
      population_count = numcpue,
      population_var = se^2) %>% 
    tidytable::select(stratum, population_count, population_var) %>% 
    tidytable::summarise(
      population_count = sum(population_count, na.rm = TRUE),
      population_var = sum(population_var, na.rm = TRUE)) %>% 
    tidytable::mutate(
      year = iyear,
      species_code = ispp)) %>% 
  tidytable::select(year, species_code, biomass_mt, biomass_var, population_count, population_var)
  
}

#' Simulate Design-Based and Post-Stratified Performance Under Reduced Sampling Effort
#'
#' @description
#' Runs a survey design simulation by subsampling historical survey stations down to a 
#' fixed target sample size. Stations are allocated across strata proportionally based on 
#' historical sampling density, and a randomized draw without replacement is executed. 
#' The function then calculates and compares total biomass/abundance indices using 
#' both historical design-based (\code{get_index_db}) and updated post-stratified (\code{get_index_ps}) methods.
#'
#' @param data A named \code{list} containing the complete baseline survey datasets:
#'   \itemize{
#'     \item \code{cpue}: Catch-per-unit-effort data frame containing at least \code{year}, \code{stratum}, \code{hauljoin}, and catch metrics.
#'     \item \code{haul}: Complete haul metadata table containing at least \code{year} and \code{hauljoin}.
#'     \item \code{strata}: Baseline stratum metadata table.
#'   }
#' @param hauls A data frame or tidytable tracking total historical stations per year, containing at least \code{year}.
#' @param test An \code{integer} specifying the target total number of sampling stations to simulate across the entire survey area for a given year.
#' @param goa_strata_2025 An \code{sf} (simple features) spatial object containing the updated 2025 spatial stratification grid (passed directly to \code{get_index_ps}).
#'
#' @details
#' The simulation algorithm processes through the following steps:
#' \enumerate{
#'   \item \strong{Year Selection:} Filters out any survey years where the total historical station count in \code{hauls} was less than the target \code{test} size.
#'   \item \strong{Proportional Allocation:} Calculates the baseline historical distribution of stations across strata (\code{p_haul}) within each valid year. It multiplies this probability by \code{test} and rounds to the nearest integer to derive a target sample size (\code{samp_haul}) per stratum.
#'   \item \strong{Stratified Random Subsampling:} Nests the haul identifiers by year and stratum, and utilizes \code{purrr::map2()} alongside \code{slice_sample()} to randomly select \code{samp_haul} stations without replacement.
#'   \item \strong{Data Slicing:} Subsets the master \code{cpue} and \code{haul} tables down to only include the randomly selected \code{hauljoin} identifiers.
#'   \item \strong{Index Generation:} 
#'     \itemize{
#'       \item Computes design-based estimates via \code{get_index_db()}, aggregating local stratum values to total annual estimates (\code{est_type = 'Historical'}).
#'       \item Computes post-stratified estimates via \code{get_index_ps()}, modeling performance against the new spatial layers (\code{est_type = 'Post-stratified'}).
#'     }
#' }
#'
#' @return A \code{tidytable} combining both estimation approaches with columns:
#'   \item{year}{Simulation survey year}
#'   \item{species_code}{Unique code representing the species}
#'   \item{biomass_mt}{Total simulated biomass estimate in Metric Tons}
#'   \item{biomass_var}{Total variance of the simulated biomass estimate}
#'   \item{population_count}{Total simulated population abundance estimate}
#'   \item{population_var}{Total variance of the simulated abundance estimate}
#'   \item{est_type}{Label indicating estimation framework: \code{'Historical'} or \code{'Post-stratified'}}
#'
#' @importFrom tidytable summarise filter distinct mutate select left_join drop_na arrange nest unnest bind_rows
#' @importFrom purrr map2
#' @importFrom dplyr slice_sample
#' 
#' @export
#'
#' @seealso \code{\link{get_index_db}}, \code{\link{get_index_ps}}
sim_db <- function(data, 
                   hauls,
                   test,
                   goa_strata_2025){
  
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
                         .by = c(year, species_code)) %>% 
    tidytable::mutate(est_type = 'Historical') %>% 
    tidytable::bind_rows(get_index_ps(sub_data_pre, goa_strata_2025) %>% # post-stratified index with reduced stations
      tidytable::mutate(est_type = 'Post-stratified')) %>% 
    tidytable::bind_rows(get_index_db(sub_data_25) %>% # 2025 index with reduced stations
      tidytable::drop_na() %>% 
      tidytable::summarise(biomass_mt = sum(biomass_mt),
                           biomass_var = sum(biomass_var),
                           population_count = sum(population_count),
                           population_var = sum(population_var),
                           .by = c(year, species_code)) %>% 
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
                         .by = c(iteration, subtest, year)) %>% 
    tidytable::mutate(species_code = species[1])
}


