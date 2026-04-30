## ENVST 325 Final Project
## Author: Jacqueline Reynaga
## Date Created: 4-23-26
## Date Last Updated: 4-30-26


library(FedData)
library(terra)
library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(sf)
library(dplyr)
library(rgbif)
library(wk)


## shapefiles
all_counties <- counties(state = "NY", cb = TRUE)

cayuga_shape <- all_counties %>%
  filter(NAME == "Cayuga")
cortland_shape <- all_counties %>%
  filter(NAME == "Cortland")
madison_shape <- all_counties %>%
  filter(NAME == "Madison")
onondaga_shape <- all_counties %>%
  filter(NAME == "Onondaga")
oswego_shape <- all_counties %>%
  filter(NAME == "Oswego")

## wkt files
ca_rounded <- st_geometry(cayuga_shape) %>%
  st_set_precision(0.00001) %>% 
  st_as_text()
ca_wkt <- st_geometry(cayuga_shape) %>%
  st_set_precision(0.00001) %>% 
  st_as_text()

co_wkt <- st_as_text(st_geometry(cortland_shape))
ma_wkt <- st_as_text(st_geometry(madison_shape))
on_wkt <- st_as_text(st_geometry(onondaga_shape))
os_wkt <- st_as_text(st_geometry(oswego_shape))

## rasters

cayuga_raster2019 <- get_nlcd(template = cayuga_shape,
                        label = "cayuga_county2019",
                        year = 2019,
                        dataset = "landcover",
                        force.redo = TRUE)

cayuga_raster2021 <- get_nlcd(template = cayuga_shape,
                          label = "cayuga_county2021",
                          year = 2021,
                          dataset = "landcover",
                          force.redo = TRUE)

plot(cayuga_raster2019)
plot(cayuga_raster2021)


cortland_raster2019 <- get_nlcd(template = cortland_shape,
                          label = "courtland_county2019",
                          year = 2019,
                          dataset = "landcover",
                          force.redo = TRUE)

cortland_raster2021 <- get_nlcd(template = cortland_shape,
                            label = "courtland_county2021",
                            year = 2021,
                            dataset = "landcover",
                            force.redo = TRUE)

plot(cortland_raster2019)
plot(cortland_raster2021)


madison_raster2019 <- get_nlcd(template = madison_shape,
                            label = "madison_county2019",
                            year = 2019,
                            dataset = "landcover",
                            force.redo = TRUE)

madison_raster2021 <- get_nlcd(template = madison_shape,
                           label = "madison_county2021",
                           year = 2021,
                           dataset = "landcover",
                           force.redo = TRUE)

plot(madison_raster2019)
plot(madison_raster2021)


onondaga_raster2019 <- get_nlcd(template = onondaga_shape,
                             label = "onondonga_county2019",
                             year = 2019,
                             dataset = "landcover",
                             force.redo = TRUE)

onondaga_raster2021 <- get_nlcd(template = onondaga_shape,
                             label = "onondonga_county2021",
                             year = 2021,
                             dataset = "landcover",
                             force.redo = TRUE)

plot(onondaga_raster2019)
plot(onondaga_raster2021)


oswego_raster2019 <- get_nlcd(template = oswego_shape,
                              label = "oswego_county2019",
                              year = 2019,
                              dataset = "landcover",
                              force.redo = TRUE)

oswego_raster2021 <- get_nlcd(template = oswego_shape,
                            label = "oswego_county2021",
                            year = 2021,
                            dataset = "landcover",
                            force.redo = TRUE)

plot(oswego_raster2019)
plot(oswego_raster2021)



sp_name <- "Peromyscus maniculatus"
sp_backbone <- name_backbone(name = sp_name)
sp_key <- sp_backbone$usageKey

occ <- occ_search(
  taxonKey = sp_key,
  geometry = ca_wkt,
  year = 2021,
  basisOfRecord = "OBSERVATION"
)

animalKey <- name_backbone(name = "Animalia")
plantKey <- name_backbone(name = "Plantae")


ca_ccw <- wk_orient(
  wkt(ca_wkt),
  direction = wk_counterclockwise()
)

ca_ccw_shape <- as.character(ca_ccw)[1]


occ_download(pred_or(pred("taxonKey", animalKey$usageKey),
                     pred("taxonKey", plantKey$usageKey)),
             pred("hasCoordinate", TRUE),
             pred("year", 2021),
             pred_within(ca_ccw_shape))
occ_download(pred_or(pred("taxonKey", animalKey$usageKey),
                     pred("taxonKey", plantKey$usageKey)),
             pred("hasCoordinate", TRUE),
             pred("year", 2021),
             pred_within(ca_ccw_shape))
occ_download(pred_or(pred("taxonKey", animalKey$usageKey),
                     pred("taxonKey", plantKey$usageKey)),
             pred("hasCoordinate", TRUE),
             pred("year", 2021),
             pred_within(ca_ccw_shape))
occ_download(pred_or(pred("taxonKey", animalKey$usageKey),
                     pred("taxonKey", plantKey$usageKey)),
             pred("hasCoordinate", TRUE),
             pred("year", 2021),
             pred_within(ca_ccw_shape))
occ_download(pred_or(pred("taxonKey", animalKey$usageKey),
                     pred("taxonKey", plantKey$usageKey)),
             pred("hasCoordinate", TRUE),
             pred("year", 2021),
             pred_within(ca_ccw_shape))

ca_species_prep = occ_download_prep(
  pred_or(pred("taxonKey", animalKey),
          pred("taxonKey", plantKey)),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  pred_within(ca_ccw_shape))
out_test = occ_download_queue(.list = list(queries))

nchar((ca_ccw_shape))


str(ca_ccw_shape, list.len = nchar((ca_ccw_shape)))







matched_commas <- gregexpr(",", ca_ccw_shape, fixed = TRUE)
n_commas <- length(matched_commas[[1]])







