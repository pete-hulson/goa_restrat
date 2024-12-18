##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Reclassify historical GOA station locations to GOA 2025 strata
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Connect to Oracle
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# devtools::install_github("afsc-gap-products/gapindex", force = TRUE)
library(terra)
library(RODBC)
library(gapindex)
channel <- gapindex::get_connected(check_access = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Query GOA haul locations for 2019
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
racebase_haul <- RODBC::sqlQuery(
  channel,
  query = "SELECT HAULJOIN, YEAR, STRATUM, LONGITUDE_DD_START, LATITUDE_DD_START
FROM GAP_PRODUCTS.AKFIN_CRUISE
JOIN GAP_PRODUCTS.AKFIN_HAUL
ON GAP_PRODUCTS.AKFIN_CRUISE.CRUISEJOIN = GAP_PRODUCTS.AKFIN_HAUL.CRUISEJOIN
WHERE  GAP_PRODUCTS.AKFIN_CRUISE.SURVEY_DEFINITION_ID = 47
AND GAP_PRODUCTS.AKFIN_CRUISE.YEAR = 2019")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Import new stratum shapefile
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
strata_2025 <- terra::vect(x = "data/GOA/shapefiles_akgfmaps/goa_strata_2025.gpkg")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Reclassify haul station locations
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create spatial object with historical stations
haul_locs_lat <-
  terra::vect(x = racebase_haul,
              geom = c("LONGITUDE_DD_START", "LATITUDE_DD_START"),
              crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

## Project to same projection as the new strata
haul_locs_aea <- terra::project(x = haul_locs_lat,
                                terra::crs(x = strata_2025))

## Reclassify each station to the new strata. Takes a while...
haul_locs_2025 <- terra::extract(x = strata_2025, y = haul_locs_aea)

## Attach HAULJOINs
haul_locs_2025$HAULJOIN[haul_locs_2025$id.y] <- haul_locs_aea$HAULJOIN[haul_locs_2025$id.y]

## Save
saveRDS(object = haul_locs_2025, "haul_locs_2025.RDS")