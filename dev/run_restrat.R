# script to compute goa survey indices under restratification

# load/source libraries/functions ----
library(tidyverse)
library(vroom)
library(here)
library(afscdata)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)


# set parameters for running stuff ----
# query data? - if first time running, or if data.rds file not in data folder, you will need to change this to TRUE
run_query = FALSE
# run terra::extract function from shape file? - if first time running, or if new_haul.csv file not in data folder, you will need to change this to TRUE
run_terra = FALSE


# query data ----

# tier 3 species codes
species_t3 = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)

# tier 5 species codes
species_t5 = c(310, 400, 406, 410, 420, 425, 435, 440, 445, 450, 455, 460, 471, 472, 475, 477, 480, 483, 485, 490, 495, 10170, 10210, 10220, 10250, 10270, 10285, 30020, 30100, 30430, 30475, 30535, 30560, 30576)

# put 'em together
species = c(species_t3, species_t5)

# run query
if(isTRUE(run_query)){
  data <- query_data(species)
} else{data <- readRDS(here::here('data', 'data.rds'))}


# reclassify haul station locations ----
new_haul <- restrat(data, run_terra)


# compute abundance indices (biom & numbers) ----
index <- get_index(data, new_haul)


## check that computed indices matches with gap produced indices (within 0.1% on average) ---
compare_index(data, index)


# plot comparison between restratified and og indices ----

# example for pacific cod
species = 21720
stock_name = "Pcod"

plot_restrat(index, species, stock_name)

# example for complex (rebs)
species = c(30050, 30051, 30052)
stock_name = "REBS"

plot_restrat(index, species, stock_name)



