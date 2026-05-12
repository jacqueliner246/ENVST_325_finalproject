## ENVST 325 Final Project
## Author: Jacqueline Reynaga
## Date Created: 4-23-26
## Date Last Updated: 5-12-26


library(FedData)
library(terra)
library(tigris)
options(tigris_use_cache = TRUE)
library(ggplot2)
library(sf)
library(sp)
library(dplyr)
library(rgbif)
library(wk)


# get county areas --------------------------------------------------------

## shapefiles
### get all new york counties
all_counties <- counties(state = "NY", cb = TRUE)
### separate into central new york counties
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
ca_wkt <- st_as_text(st_geometry(cayuga_shape))
co_wkt <- st_as_text(st_geometry(cortland_shape))
ma_wkt <- st_as_text(st_geometry(madison_shape))
on_wkt <- st_as_text(st_geometry(onondaga_shape))
os_wkt <- st_as_text(st_geometry(oswego_shape))


# land cover data ---------------------------------------------------------

## rasters
cayuga_raster2021 <- get_nlcd(template = cayuga_shape,
                          label = "cayuga_county2021",
                          year = 2021,
                          dataset = "landcover",
                          force.redo = TRUE)
plot(cayuga_raster2021)

cortland_raster2021 <- get_nlcd(template = cortland_shape,
                            label = "courtland_county2021",
                            year = 2021,
                            dataset = "landcover",
                            force.redo = TRUE)
plot(cortland_raster2021)

madison_raster2021 <- get_nlcd(template = madison_shape,
                           label = "madison_county2021",
                           year = 2021,
                           dataset = "landcover",
                           force.redo = TRUE)
plot(madison_raster2021)

onondaga_raster2021 <- get_nlcd(template = onondaga_shape,
                             label = "onondonga_county2021",
                             year = 2021,
                             dataset = "landcover",
                             force.redo = TRUE)
plot(onondaga_raster2021)

oswego_raster2021 <- get_nlcd(template = oswego_shape,
                            label = "oswego_county2021",
                            year = 2021,
                            dataset = "landcover",
                            force.redo = TRUE)
plot(oswego_raster2021)

## get land cover values for each raster
lc_cayuga <- values(cayuga_raster2021)
lc_cortland <- values(cortland_raster2021)
lc_madison <- values(madison_raster2021)
lc_onondaga <- values(onondaga_raster2021)
lc_oswego <- values(oswego_raster2021)

## get landcover numerical values and their associations
legend <- pal_nlcd()
landcover_legend <- legend[,2]

condensed_landcover_values <- c('water', 'water', 'developed', 'developed', 'developed', 'developed', 'barren',
                                'forest', 'forest', 'forest', 'herbaceous', 'herbaceous', 'herbaceous', 'herbaceous',
                                'herbaceous', 'herbaceous', 'farm', 'farm', 'herbaceous', 'herbaceous')
landcover_legend$newClass <- condensed_landcover_values

## changing to simpler landcover classes
cayuga_rastor_df <- as.data.frame(cayuga_raster2021, na.rm = FALSE)
cayuga_rastor_df2 <- cayuga_rastor_df %>%
  left_join(landcover_legend, by = "Class") %>%
  select(Class = newClass)
cayuga_rastor_new<- setValues(cayuga_raster2021, as.matrix(cayuga_rastor_df2))
plot(cayuga_rastor_new)
plot(cayuga_raster2021)

# gbif data prep ----------------------------------------------------------
## do not run if you already have data

## set options
options(gbif_user = "") # fill in with gbif username
options(gbif_email = "") # fill in with gbif associated email
options(gbif_pwd = "") # fill in with gbif account password

## get all animal observations in each county shape file
animalKey <- name_backbone(name = "Animalia")

## need to reorient each shape counter clockwise
ca_ccw <- wk_orient(
  wkt(ca_wkt),
  direction = wk_counterclockwise()
)
ca_ccw_shape <- as.character(ca_ccw)

co_ccw <- wk_orient(
  wkt(co_wkt),
  direction = wk_counterclockwise()
)
co_ccw_shape <- as.character(co_ccw)

ma_ccw <- wk_orient(
  wkt(ma_wkt),
  direction = wk_counterclockwise()
)
ma_ccw_shape <- as.character(ma_ccw)

on_ccw <- wk_orient(
  wkt(ca_wkt),
  direction = wk_counterclockwise()
)
on_ccw_shape <- as.character(on_ccw)

os_ccw <- wk_orient(
  wkt(os_wkt),
  direction = wk_counterclockwise()
)
os_ccw_shape <- as.character(os_ccw)

## request download of data
ca_gbif_down <- occ_download(pred("taxonKey", animalKey$usageKey),
             # pred("hasCoordinate", TRUE),
             pred_or(pred("year", 2014),
                     pred("year", 2024)),
             pred_within(ca_ccw_shape))
## wait until data is downloaded
ca_gbif_meta <- occ_download_wait(ca_gbif_down, status_ping = 10, quiet = FALSE)
## get zip file
ca_gbif_get <- occ_download_get(ca_gbif_down)
ca_gbif_data <- occ_download_import(ca_gbif_get)
write.csv(ca_gbif_data, file = "cayugaGBIF.csv")
cayuga_GBIF_df <- read.csv("cayugaGBIF.csv")

# co_gbif <- occ_download(pred("taxonKey", animalKey$usageKey),
#              pred("hasCoordinate", TRUE),
#              pred_or(pred("year", 2000),
#                      pred("year", 2021)),
#              pred_within(co_ccw_shape))
# co_gbif_meta <- occ_download_wait(co_gbif, status_ping = 10, quiet = FALSE)
# co_gbif_get <- occ_download_get(co_gbif)
# co_gbif_data <- occ_download_import(co_gbif_get)
# write.csv(co_gbif_data, file = "cortland_GBIF.csv")
# cortland_GBIF_df <- read.csv("cortland_GBIF.csv")
# 
# ma_gbif <- occ_download(pred("taxonKey", animalKey$usageKey),
#              pred("hasCoordinate", TRUE),
#              pred_or(pred("year", 2000),
#                      pred("year", 2021)),
#              pred_within(ma_ccw_shape))
# ma_gbif_meta <- occ_download_wait(ma_gbif, status_ping = 10, quiet = FALSE)
# ma_gbif_get <- occ_download_get(ma_gbif)
# ma_gbif_data <- occ_download_import(ma_gbif_get)
# write.csv(ma_gbif_data, file = "madison_GBIF.csv")
# madison_GBIF_df <- read.csv("madison_GBIF.csv")
# 
# on_gbif <- occ_download(pred("taxonKey", animalKey$usageKey),
#              pred("hasCoordinate", TRUE),
#              pred_or(pred("year", 2000),
#                      pred("year", 2021)),
#              pred_within(on_ccw_shape))
# on_gbif_meta <- occ_download_wait(on_gbif, status_ping = 10, quiet = FALSE)
# on_gbif_get <- occ_download_get(on_gbif)
# on_gbif_data <- occ_download_import(on_gbif_get)
# write.csv(on_gbif_data, file = "onondaga_GBIF.csv")
# onondaga_GBIF_df <- read.csv("onondaga_GBIF.csv")
# 
# os_gbif <- occ_download(pred("taxonKey", animalKey$usageKey),
#              pred("hasCoordinate", TRUE),
#              pred_or(pred("year", 2000),
#                      pred("year", 2021)),
#              pred_within(os_ccw_shape))
# os_gbif_meta <- occ_download_wait(os_gbif, status_ping = 10, quiet = FALSE)
# os_gbif_get <- occ_download_get(os_gbif)
# os_gbif_data <- occ_download_import(os_gbif_get)
# write.csv(os_gbif_data, file = "oswego_GBIF.csv")
# oswego_GBIF_df <- read.csv("oswego_GBIF.csv")


# working with gbif data --------------------------------------------------

## manipulating gbif data
cayuga_GBIF_df <- cayuga_GBIF_df[,
  c("scientificName", "taxonKey", "family", "familyKey", "species",
    "decimalLongitude", "decimalLatitude",
    "year", "month", "day", "eventDate", 
    "countryCode", "municipality", "stateProvince", 
    "catalogNumber", "mediaType", "datasetKey")
]

cayuga_GBIF_df$lon <- as.double(cayuga_GBIF_df$decimalLongitude)
cayuga_GBIF_df$lat <- as.double(cayuga_GBIF_df$decimalLatitude)
cayuga_GBIF_sf <- st_as_sf(cayuga_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
st_crs(cayuga_GBIF_sf) <- st_crs(cayuga_shape)

plot(cayuga_GBIF_sf)

# cortland_GBIF_df <- cortland_GBIF_df[,
#                                  c("scientificName", "taxonKey", "family", "familyKey", "species",
#                                    "decimalLongitude", "decimalLatitude",
#                                    "year", "month", "day", "eventDate", 
#                                    "countryCode", "municipality", "stateProvince", 
#                                    "catalogNumber", "mediaType", "datasetKey")
# ]
# 
# cortland_GBIF_df$lon <- as.double(cortland_GBIF_df$decimalLongitude)
# cortland_GBIF_df$lat <- as.double(cortland_GBIF_df$decimalLatitude)
# cortland_GBIF_sf <- st_as_sf(cortland_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
# st_crs(cortland_GBIF_sf) <- st_crs(cortland_shape)
# 
# plot(cortland_GBIF_sf)
# 
# madison_GBIF_df <- madison_GBIF_df[,
#                                  c("scientificName", "taxonKey", "family", "familyKey", "species",
#                                    "decimalLongitude", "decimalLatitude",
#                                    "year", "month", "day", "eventDate", 
#                                    "countryCode", "municipality", "stateProvince", 
#                                    "catalogNumber", "mediaType", "datasetKey")
# ]
# 
# madison_GBIF_df$lon <- as.double(madison_GBIF_df$decimalLongitude)
# madison_GBIF_df$lat <- as.double(madison_GBIF_df$decimalLatitude)
# madison_GBIF_sf <- st_as_sf(madison_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
# st_crs(madison_GBIF_sf) <- st_crs(madison_shape)
# 
# plot(madison_GBIF_sf)
# 
# onondaga_GBIF_df <- onondaga_GBIF_df[,
#                                  c("scientificName", "taxonKey", "family", "familyKey", "species",
#                                    "decimalLongitude", "decimalLatitude",
#                                    "year", "month", "day", "eventDate", 
#                                    "countryCode", "municipality", "stateProvince", 
#                                    "catalogNumber", "mediaType", "datasetKey")
# ]
# 
# onondaga_GBIF_df$lon <- as.double(onondaga_GBIF_df$decimalLongitude)
# onondaga_GBIF_df$lat <- as.double(onondaga_GBIF_df$decimalLatitude)
# onondaga_GBIF_sf <- st_as_sf(onondaga_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
# st_crs(onondaga_GBIF_sf) <- st_crs(onondaga_shape)
# 
# plot(onondaga_GBIF_sf)
# 
# oswego_GBIF_df <- oswego_GBIF_df[,
#                                  c("scientificName", "taxonKey", "family", "familyKey", "species",
#                                    "decimalLongitude", "decimalLatitude",
#                                    "year", "month", "day", "eventDate", 
#                                    "countryCode", "municipality", "stateProvince", 
#                                    "catalogNumber", "mediaType", "datasetKey")
# ]
# 
# oswego_GBIF_df$lon <- as.double(oswego_GBIF_df$decimalLongitude)
# oswego_GBIF_df$lat <- as.double(oswego_GBIF_df$decimalLatitude)
# oswego_GBIF_sf <- st_as_sf(oswego_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
# st_crs(oswego_GBIF_sf) <- st_crs(oswego_shape)
# 
# plot(oswego_GBIF_sf)

## get family data
cayuga_GBIF_sf_2014 <- cayuga_GBIF_sf %>% 
  filter(year == 2015)
cayuga_GBIF_sf_2024 <- cayuga_GBIF_sf %>% 
  filter(year == 2025)
cayuga_family_abundance2014 <- table(cayuga_GBIF_sf_2014$family)
cayuga_family_abundance2014 <- as.data.frame(cayuga_family_abundance2014)
cayuga_family_abundance2014 <- rename(cayuga_family_abundance2014, c(family = Var1, freq_2014 = Freq))
cayuga_family_abundance2024 <- table(cayuga_GBIF_sf_2024$family)
cayuga_family_abundance2024 <- as.data.frame(cayuga_family_abundance2024) 
cayuga_family_abundance2024 <- rename(cayuga_family_abundance2024, c(family = Var1, freq_2024 = Freq))
cayuga_family_abundance <- merge(cayuga_family_abundance2014, cayuga_family_abundance2024, all = TRUE)
cayuga_family_abundance[is.na(cayuga_family_abundance)] <- 0
cayuga_family_abundance$difference <- cayuga_family_abundance$freq_2024 - cayuga_family_abundance$freq_2014
cayuga_family_abundance <- cayuga_family_abundance %>% 
  mutate(plot = difference >= 0)

ggplot(cayuga_family_abundance, aes(y=reorder(family, difference), x=difference, fill = plot)) + 
  geom_bar(position=position_dodge(), stat="identity") + 
  # geom_text(aes(x = 100, label = difference), 
  #               hjust = 0.5, position=position_dodge(width=2),
  #           size = 2)+
  scale_fill_manual(values = c('#952611','#004965'))+
  labs(x = 'Change between family observation counts in 2015 vs 2025 (Cayuga County)', y = "Family")+
  theme_classic()+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())





cayuga_top10_families <- cayuga_family_abundance %>%
  top_n(10, Freq)

cayuga_GBIF_sf_families <- cayuga_GBIF_sf %>% 
  filter(family %in% c(cayuga_top10_families$Var1))

ggplot() +
  geom_sf(data = cayuga_shape) +
  geom_sf(data = cayuga_GBIF_sf_families[, 1], aes(col = cayuga_GBIF_sf_families$family)) +
  theme_bw() +
  # theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Taxonomic families")) +
  # scale_color_manual(values = c("darkorange", "forestgreen")) +
  labs(title = "Occurrences of Anatidae and Cardinalidae in cayuga county")

cortland_family_abundance <- table(cortland_GBIF_sf$family)
cortland_family_abundance <- as.data.frame(cayuga_family_abundance) 
cortland_top10_families <- cayuga_family_abundance %>%
  top_n(10, Freq)

cayuga_GBIF_sf_families <- cayuga_GBIF_sf %>% 
  filter(family %in% c(cayuga_top10_families$Var1))

ggplot() +
  geom_sf(data = cayuga_shape) +
  geom_sf(data = cayuga_GBIF_sf_families[, 1], aes(col = cayuga_GBIF_sf_families$family)) +
  theme_bw() +
  # theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Taxonomic families")) +
  # scale_color_manual(values = c("darkorange", "forestgreen")) +
  labs(title = "Occurrences of Anatidae and Cardinalidae in cayuga county")

cayuga_family_abundance <- table(cayuga_GBIF_sf$family)
cayuga_family_abundance <- as.data.frame(cayuga_family_abundance) 
cayuga_top10_families <- cayuga_family_abundance %>%
  top_n(10, Freq)

cayuga_GBIF_sf_families <- cayuga_GBIF_sf %>% 
  filter(family %in% c(cayuga_top10_families$Var1))

ggplot() +
  geom_sf(data = cayuga_shape) +
  geom_sf(data = cayuga_GBIF_sf_families[, 1], aes(col = cayuga_GBIF_sf_families$family)) +
  theme_bw() +
  # theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Taxonomic families")) +
  # scale_color_manual(values = c("darkorange", "forestgreen")) +
  labs(title = "Occurrences of Anatidae and Cardinalidae in cayuga county")

cayuga_family_abundance <- table(cayuga_GBIF_sf$family)
cayuga_family_abundance <- as.data.frame(cayuga_family_abundance) 
cayuga_top10_families <- cayuga_family_abundance %>%
  top_n(10, Freq)

cayuga_GBIF_sf_families <- cayuga_GBIF_sf %>% 
  filter(family %in% c(cayuga_top10_families$Var1))

ggplot() +
  geom_sf(data = cayuga_shape) +
  geom_sf(data = cayuga_GBIF_sf_families[, 1], aes(col = cayuga_GBIF_sf_families$family)) +
  theme_bw() +
  # theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Taxonomic families")) +
  # scale_color_manual(values = c("darkorange", "forestgreen")) +
  labs(title = "Occurrences of Anatidae and Cardinalidae in cayuga county")

cayuga_family_abundance <- table(cayuga_GBIF_sf$family)
cayuga_family_abundance <- as.data.frame(cayuga_family_abundance) 
cayuga_top10_families <- cayuga_family_abundance %>%
  top_n(10, Freq)

cayuga_GBIF_sf_families <- cayuga_GBIF_sf %>% 
  filter(family %in% c(cayuga_top10_families$Var1))

ggplot() +
  geom_sf(data = cayuga_shape) +
  geom_sf(data = cayuga_GBIF_sf_families[, 1], aes(col = cayuga_GBIF_sf_families$family)) +
  theme_bw() +
  # theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Taxonomic families")) +
  # scale_color_manual(values = c("darkorange", "forestgreen")) +
  labs(title = "Occurrences of Anatidae and Cardinalidae in cayuga county")

cayuga_family_abundance <- table(cayuga_GBIF_sf$family)
cayuga_family_abundance <- as.data.frame(cayuga_family_abundance) 
cayuga_top10_families <- cayuga_family_abundance %>%
  top_n(10, Freq)

cayuga_GBIF_sf_families <- cayuga_GBIF_sf %>% 
  filter(family %in% c(cayuga_top10_families$Var1))

ggplot() +
  geom_sf(data = cayuga_shape) +
  geom_sf(data = cayuga_GBIF_sf_families[, 1], aes(col = cayuga_GBIF_sf_families$family)) +
  theme_bw() +
  # theme(legend.position = "bottom") +
  guides(col = guide_legend(title = "Taxonomic families")) +
  # scale_color_manual(values = c("darkorange", "forestgreen")) +
  labs(title = "Occurrences of Anatidae and Cardinalidae in cayuga county")
