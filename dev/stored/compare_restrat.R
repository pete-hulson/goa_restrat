
# load packages ----
# load tidyverse and afscdata package
library(tidyverse)
library(scico)
library(afscdata)
# to install afscdata package
# if(!isTRUE("afscdata" %in% rownames(installed.packages()))) {
#   devtools::install_github("afsc-assessments/afscdata", force = TRUE)
# }

# pull data ----
# connect to database
db = 'afsc'
conn = afscdata::connect(db)  

# list tables in gap_products scema (if interested)
# odbc::odbcListObjects(conn, schema = "GAP_PRODUCTS")

## tier 3 stocks ----
# species codes for tier 3 species in goa
species_t3 = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
# query design-based biomass data
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id == 47,
                area_id %in% c(803, 804, 805, 99903),
                year %in% c(2019, 2025),
                species_code %in% species_t3) %>% 
  dplyr::select(year, area_id, species_code, biomass_mt, biomass_var) %>% 
  dplyr::collect() -> compare_t3
# write out data to store
vroom::vroom_write(compare_t3, here::here('t3_biom.csv'), delim = ',')

# query design-based age comps
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_agecomp')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id == 47,
                area_id %in% c(803, 804, 805, 99903),
                year %in% c(2019, 2025),
                species_code %in% species_t3,
                age > 0) %>% 
  dplyr::select(year, area_id, species_code, sex, age, apop = population_count) %>% 
  dplyr::collect() -> compare_t3_apop
# write out data to store
vroom::vroom_write(compare_t3_apop, here::here('t3_apop.csv'), delim = ',')

# query design-based length comps
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_sizecomp')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id == 47,
                area_id %in% c(803, 804, 805, 99903),
                year %in% c(2019, 2025),
                species_code %in% species_t3,
                length_mm > 0) %>% 
  dplyr::select(year, area_id, species_code, sex, length = length_mm, lpop = population_count) %>% 
  dplyr::mutate(length = length / 10) %>% 
  dplyr::collect() -> compare_t3_lpop
# write out data to store
vroom::vroom_write(compare_t3_lpop, here::here('t3_lpop.csv'), delim = ',')


## tier 5 stocks ----
# species codes for tier 5 species in goa
species_t5 = c(310, 400, 406, 410, 420, 425, 435, 440, 445, 450, 455, 460, 471, 472, 475, 477, 480, 483, 485, 490, 495, 10170, 10210, 10220, 10250, 10270, 10285, 30020, 30100, 30430, 30475, 30535, 30560, 30576)
# query design-based biomass data
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id == 47,
                area_id %in% c(803, 804, 805, 99903),
                year %in% c(2019, 2025),
                species_code %in% species_t5) %>% 
  dplyr::select(year, area_id, species_code, biomass_mt, biomass_var) %>% 
  dplyr::collect() -> compare_t5
# write out data to store
vroom::vroom_write(compare_t5, here::here('t5_biom.csv'), delim = ',')

# plot tier 3 results ----
# set up survey data by stock
db <- compare_t3 %>% 
  tidytable::mutate(# cause i just like west to the left/top and east to the right/bottom
    region = factor(case_when(area_id == 803 ~ "Central GOA",
                              area_id == 804 ~ "Eastern GOA",
                              area_id == 805 ~ "Western GOA",
                              area_id == 99903 ~ "GOA"), levels = c("Western GOA", "Central GOA", "Eastern GOA", "GOA")),
    # rename species codes to stock names
    stock = factor(case_when(species_code == 10110 ~ "1: arrowtooth",
                             species_code == 10130 ~ "2: flathead",
                             species_code == 10180 ~ "3: dover",
                             species_code == 10200 ~ "4: rex",
                             species_code == 10261 ~ "5: n. rock sole",
                             species_code == 10262 ~ "6: s. rock sole",
                             species_code == 20510 ~ "7: sablefish",
                             species_code == 21720 ~ "8: p. cod",
                             species_code == 21740 ~ "9: pollock",
                             species_code %in% c(30050, 30051, 30052) ~ "10: re-bs",
                             species_code == 30060 ~ "11: pop",
                             species_code == 30420 ~ "12: n. rockfish",
                             species_code %in% c(30150, 30152) ~ "13: dusky"),
                   levels = paste0(seq(1:13), ": ", c("arrowtooth", "flathead", "dover", "rex", "n. rock sole", "s. rock sole", "sablefish", "p. cod", "pollock", "re-bs", "pop", "n. rockfish", "dusky")))) %>% 
  tidytable::summarise(biom = sum(biomass_mt),
                       biom_sd = sqrt(sum(biomass_var)), .by = c(year, region, stock))

## biomass ----
db %>% 
  tidytable::filter(year == 2019) %>% 
  tidytable::mutate(old = biom) %>% 
  tidytable::select(-year, -biom, -biom_sd) %>% 
  tidytable::left_join(db %>% 
                         tidytable::filter(year == 2025) %>% 
                         tidytable::mutate(new = biom) %>% 
                         tidytable::select(-year, -biom, -biom_sd)) %>% 
  tidytable::mutate(perc = case_when(old > 0 ~ 100 * (new - old) / old,
                                     .default = 0),
                    perc_diff = scales::percent(perc / 100, accuracy = 1, trim = FALSE),
                    pjust = case_when(perc > 0 ~ -0.5,
                                      perc < 0 ~ 1)) %>% 
  ggplot(aes(x = stock, y = perc, fill = stock)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = perc_diff, vjust = pjust), fontface = "bold") +
  facet_wrap(~region, ncol = 1) +
  ylim(-100, 100) +
  scale_x_discrete(breaks = unique(db$stock), 
                   labels = c(1:length(unique(db$stock)))) +
  theme_bw(base_size = 14) + 
  labs(x = "Stock #", y = "% difference in 2019 biomass", fill = "Tier 3 Stock:") +
  geom_abline(intercept = 0, slope = 0, color = "black") +
  scico::scale_fill_scico_d(palette = 'roma')

## biomass cv ----
db %>% 
  tidytable::filter(year == 2019) %>% 
  tidytable::mutate(old = biom_sd / biom) %>% 
  tidytable::select(-year, -biom, -biom_sd) %>% 
  tidytable::left_join(db %>% 
                         tidytable::filter(year == 2025) %>% 
                         tidytable::mutate(new = biom_sd / biom) %>% 
                         tidytable::select(-year, -biom, -biom_sd)) %>% 
  tidytable::mutate(perc = case_when(old > 0 ~ 100 * (new - old) / old,
                                     .default = 0),
                    perc_diff = scales::percent(perc / 100, accuracy = 1, trim = FALSE),
                    pjust = case_when(perc > 0 ~ -0.5,
                                      perc < 0 ~ 1)) %>% 
  ggplot(aes(x = stock, y = perc, fill = stock)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = perc_diff, vjust = pjust), fontface = "bold") +
  facet_wrap(~region, ncol = 1) +
  ylim(-100, 100) +
  scale_x_discrete(breaks = unique(db$stock), 
                   labels = c(1:length(unique(db$stock)))) +
  theme_bw(base_size = 14) + 
  labs(x = "Stock #", y = "% difference in 2019 CV[biomass]", fill = "Tier 3 Stock:") +
  geom_abline(intercept = 0, slope = 0, color = "black") +
  scico::scale_fill_scico_d(palette = 'roma')

## age comps ----
# for combined sex (noting that there are no subareas for age comps)
compare_t3_apop %>% 
  # rename species codes to stock names
  tidytable::mutate(stock = factor(case_when(species_code == 10110 ~ "arrowtooth",
                           species_code == 10130 ~ "flathead",
                           species_code == 10180 ~ "dover",
                           species_code == 10200 ~ "rex",
                           species_code == 10261 ~ "n. rock sole",
                           species_code == 10262 ~ "s. rock sole",
                           species_code == 20510 ~ "sablefish",
                           species_code == 21720 ~ "p. cod",
                           species_code == 21740 ~ "pollock",
                           species_code %in% c(30050, 30051, 30052) ~ "re-bs",
                           species_code == 30060 ~ "pop",
                           species_code == 30420 ~ "n. rockfish",
                           species_code %in% c(30150, 30152) ~ "dusky"),
                 levels = c("arrowtooth", "flathead", "dover", "rex", "n. rock sole", "s. rock sole", "sablefish", "p. cod", "pollock", "re-bs", "pop", "n. rockfish", "dusky"))) %>% 
  tidytable::summarise(apop = sum(apop), .by= c(year, stock, age)) %>% 
  tidytable::mutate(tot = sum(apop), .by = c(year, stock)) %>% 
  tidytable::mutate(acomp = apop / tot) %>% 
  ggplot(aes(x = age, y = acomp, color = as.factor(year))) +
  geom_point() +
  geom_line() +
  facet_wrap(~stock, scales = 'free_x') +
  theme_bw(base_size = 14) +
  labs(x = "Age", y = "Proportion", color = "Year") +
  scico::scale_color_scico_d(palette = 'roma')
  
## length comps ----

# for combined sex (noting that we're looking at gulf-wide for age comps)
compare_t3_lpop %>% 
  tidytable::filter(area_id == 99903) %>% 
  # rename species codes to stock names
  tidytable::mutate(stock = factor(case_when(species_code == 10110 ~ "arrowtooth",
                                             species_code == 10130 ~ "flathead",
                                             species_code == 10180 ~ "dover",
                                             species_code == 10200 ~ "rex",
                                             species_code == 10261 ~ "n. rock sole",
                                             species_code == 10262 ~ "s. rock sole",
                                             species_code == 20510 ~ "sablefish",
                                             species_code == 21720 ~ "p. cod",
                                             species_code == 21740 ~ "pollock",
                                             species_code %in% c(30050, 30051, 30052) ~ "re-bs",
                                             species_code == 30060 ~ "pop",
                                             species_code == 30420 ~ "n. rockfish",
                                             species_code %in% c(30150, 30152) ~ "dusky"),
                                   levels = c("arrowtooth", "flathead", "dover", "rex", "n. rock sole", "s. rock sole", "sablefish", "p. cod", "pollock", "re-bs", "pop", "n. rockfish", "dusky"))) %>% 
  tidytable::summarise(lpop = sum(lpop), .by= c(year, stock, length)) %>% 
  tidytable::mutate(tot = sum(lpop), .by = c(year, stock)) %>% 
  tidytable::mutate(lcomp = lpop / tot) %>% 
  ggplot(aes(x = length, y = lcomp, color = as.factor(year))) +
  geom_point() +
  geom_line() +
  facet_wrap(~stock, scales = 'free_x') +
  theme_bw(base_size = 14) +
  labs(x = "Length (cm)", y = "Proportion", color = "Year") +
  scico::scale_color_scico_d(palette = 'roma')




# plot tier 5 results ----
# set up survey data by stock
db <- compare_t5 %>% 
  tidytable::mutate(# cause i just like west to the left/top and east to the right/bottom
    region = factor(case_when(area_id == 803 ~ "Central GOA",
                              area_id == 804 ~ "Eastern GOA",
                              area_id == 805 ~ "Western GOA",
                              area_id == 99903 ~ "GOA"), levels = c("Western GOA", "Central GOA", "Eastern GOA", "GOA")),
    # rename species codes to stock names
    stock = factor(case_when(species_code == 310 ~ "1: spiny dogfish",
                             species_code %in% c(400, 406, 410, 425, 435, 445, 450, 455, 460, 471, 472, 475, 477, 480, 483, 485, 490, 495) ~ "2: other skates",
                             species_code == 420	~ "3: big skate",
                             species_code == 440	~ "4: longnose skate",
                             species_code %in% c(10170, 10210, 10220, 10250, 10270, 10285) ~ "5: shallow h2o flats",
                             species_code == 30020	~ "6: shortspine thornyhead",
                             species_code == 30100	~	"7: silvergray rockfish (orx)",
                             species_code == 30430	~	"8: redstripe rockfish (orx)",
                             species_code == 30475	~	"9: redbanded rockfish (orx)",
                             species_code == 30535	~	"10: harlequin rockfish (orx)",
                             species_code == 30560	~	"11: sharpchin rockfish (orx)",
                             species_code == 30576	~	"12: shortraker rockfish"),
                   levels = c("1: spiny dogfish", "2: other skates", "3: big skate", "4: longnose skate", "5: shallow h2o flats", "6: shortspine thornyhead", "7: silvergray rockfish (orx)", "8: redstripe rockfish (orx)", "9: redbanded rockfish (orx)", "10: harlequin rockfish (orx)", "11: sharpchin rockfish (orx)", "12: shortraker rockfish"))) %>% 
  tidytable::summarise(biom = sum(biomass_mt),
                       biom_sd = sqrt(sum(biomass_var)), .by = c(year, region, stock))

## biomass ----
db %>% 
  tidytable::filter(year == 2019) %>% 
  tidytable::mutate(old = biom) %>% 
  tidytable::select(-year, -biom, -biom_sd) %>% 
  tidytable::left_join(db %>% 
                         tidytable::filter(year == 2025) %>% 
                         tidytable::mutate(new = biom) %>% 
                         tidytable::select(-year, -biom, -biom_sd)) %>% 
  tidytable::mutate(perc = case_when(old > 0 ~ 100 * (new - old) / old,
                                     .default = 0),
                    perc_diff = scales::percent(perc / 100, accuracy = 1, trim = FALSE),
                    pjust = case_when(perc > 0 ~ -0.5,
                                      perc < 0 ~ 1)) %>% 
  ggplot(aes(x = stock, y = perc, fill = stock)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = perc_diff, vjust = pjust), fontface = "bold") +
  facet_wrap(~region, ncol = 1) +
  ylim(-100, 150) +
  scale_x_discrete(breaks = unique(db$stock), 
                   labels = c(1:length(unique(db$stock)))) +
  theme_bw(base_size = 14) + 
  labs(x = "Stock #", y = "% difference in 2019 biomass", fill = "Tier 4/5 Stock:") +
  geom_abline(intercept = 0, slope = 0, color = "black") +
  scico::scale_fill_scico_d(palette = 'roma')

## biomass cv ----
db %>% 
  tidytable::filter(year == 2019) %>% 
  tidytable::mutate(old = biom_sd / biom) %>% 
  tidytable::select(-year, -biom, -biom_sd) %>% 
  tidytable::left_join(db %>% 
                         tidytable::filter(year == 2025) %>% 
                         tidytable::mutate(new = biom_sd / biom) %>% 
                         tidytable::select(-year, -biom, -biom_sd)) %>% 
  tidytable::mutate(perc = case_when(old > 0 ~ 100 * (new - old) / old,
                                     .default = 0),
                    perc_diff = scales::percent(perc / 100, accuracy = 1, trim = FALSE),
                    pjust = case_when(perc > 0 ~ -0.5,
                                      perc < 0 ~ 1)) %>% 
  ggplot(aes(x = stock, y = perc, fill = stock)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = perc_diff, vjust = pjust), fontface = "bold") +
  facet_wrap(~region, ncol = 1) +
  ylim(-100, 100) +
  scale_x_discrete(breaks = unique(db$stock), 
                   labels = c(1:length(unique(db$stock)))) +
  theme_bw(base_size = 14) + 
  labs(x = "Stock #", y = "% difference in 2019 CV[biomass]", fill = "Tier 4/5 Stock:") +
  geom_abline(intercept = 0, slope = 0, color = "black") +
  scico::scale_fill_scico_d(palette = 'roma')

