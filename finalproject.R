## ENVST 325 Final Project
## Author: Jacqueline Reynaga
## Date Created: 4-23-26
## Date Last Updated: 5-13-26


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
library(ggrepel)
library(tidyverse)
library(emmeans)


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


# simplify landcover values -----------------------------------------------

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
lc_colors <- c('#D2CDC0', '#B50000','#FBF65D', '#38814F', '#85C750', '#5475A8')

## changing to simpler landcover classes
cayuga_rastor_df <- as.data.frame(cayuga_raster2021, na.rm = FALSE)
cayuga_rastor_df <- cayuga_rastor_df %>%
  left_join(landcover_legend, by = "Class") %>%
  select(Class = newClass)
cayuga_rastor_new <- setValues(cayuga_raster2021, as.matrix(cayuga_rastor_df))
plot(cayuga_rastor_new, col = lc_colors)

## calculate landcover percentages
ca_lc_counts <- freq(cayuga_rastor_new) %>% 
  select(-layer)
ca_lc_counts$percent <- round(ca_lc_counts$count / sum(ca_lc_counts$count), 3)
ca_lc_counts$county <- 'cayuga'


## changing to simpler landcover classes
cortland_rastor_df <- as.data.frame(cortland_raster2021, na.rm = FALSE)
cortland_rastor_df <- cortland_rastor_df %>%
  left_join(landcover_legend, by = "Class") %>%
  select(Class = newClass)
cortland_rastor_new <- setValues(cortland_raster2021, as.matrix(cortland_rastor_df))
plot(cortland_rastor_new, col = lc_colors)

## calculate landcover percentages
co_lc_counts <- freq(cortland_rastor_new) %>% 
  select(-layer)
co_lc_counts$percent <- round(co_lc_counts$count / sum(co_lc_counts$count), 3)
co_lc_counts$county <- 'cortland'


## changing to simpler landcover classes
madison_rastor_df <- as.data.frame(madison_raster2021, na.rm = FALSE)
madison_rastor_df <- madison_rastor_df %>%
  left_join(landcover_legend, by = "Class") %>%
  select(Class = newClass)
madison_rastor_new <- setValues(madison_raster2021, as.matrix(madison_rastor_df))
plot(madison_rastor_new, col = lc_colors)

## calculate landcover percentages
ma_lc_counts <- freq(madison_rastor_new) %>% 
  select(-layer)
ma_lc_counts$percent <- round(ma_lc_counts$count / sum(ma_lc_counts$count), 3)
ma_lc_counts$county <- 'madison'


## changing to simpler landcover classes
onondaga_rastor_df <- as.data.frame(onondaga_raster2021, na.rm = FALSE)
onondaga_rastor_df <- onondaga_rastor_df %>%
  left_join(landcover_legend, by = "Class") %>%
  select(Class = newClass)
onondaga_rastor_new <- setValues(onondaga_raster2021, as.matrix(onondaga_rastor_df))
plot(onondaga_rastor_new, col = lc_colors)

## calculate landcover percentages
on_lc_counts <- freq(onondaga_rastor_new) %>% 
  select(-layer)
on_lc_counts$percent <- round(on_lc_counts$count / sum(on_lc_counts$count), 3)
on_lc_counts$county <- 'onondaga'


## changing to simpler landcover classes
oswego_rastor_df <- as.data.frame(oswego_raster2021, na.rm = FALSE)
oswego_rastor_df <- oswego_rastor_df %>%
  left_join(landcover_legend, by = "Class") %>%
  select(Class = newClass)
oswego_rastor_new <- setValues(oswego_raster2021, as.matrix(oswego_rastor_df))
plot(oswego_rastor_new, col = lc_colors)

## calculate landcover percentages
os_lc_counts <- freq(oswego_rastor_new) %>% 
  select(-layer)
os_lc_counts$percent <- round(os_lc_counts$count / sum(os_lc_counts$count), 3)
os_lc_counts$county <- 'oswego'


# combine all county landcover data
all_lc_counts <- rbind(ca_lc_counts, co_lc_counts, ma_lc_counts, on_lc_counts, os_lc_counts)


# plotting landcover percentages for each county -------------------------------

## pie chart label locations
ca_pie_chart <- ca_lc_counts %>% 
  mutate(csum = rev(cumsum(rev(percent))), 
         pos = percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent/2, pos))

## pie chart
ggplot(ca_lc_counts, aes(x = "", y = percent, fill = value)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = lc_colors) +
  labs(title = "Caygua County") +
  geom_label_repel(data = ca_pie_chart,
                   aes(y = pos, label = paste0(paste0(value," "), paste0(percent*100, "%")), fill = NULL),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  theme(panel.background = element_blank(),
        title = element_text(hjust = 1.5),
        legend.position = "none", 
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text()) 


## pie chart label locations
co_pie_chart <- co_lc_counts %>% 
  mutate(csum = rev(cumsum(rev(percent))), 
         pos = percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent/2, pos))

## pie chart
ggplot(co_lc_counts, aes(x = "", y = percent, fill = value)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = lc_colors) +
  labs(title = "Cortland County") +
  geom_label_repel(data = co_pie_chart,
                   aes(y = pos, label = paste0(paste0(value," "), paste0(percent*100, "%")), fill = NULL),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  theme(panel.background = element_blank(),
        title = element_text(hjust = 1.5),
        legend.position = "none", 
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text())


## pie chart label locations
ma_pie_chart <- ma_lc_counts %>% 
  mutate(csum = rev(cumsum(rev(percent))), 
         pos = percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent/2, pos))

## pie chart
ggplot(ma_lc_counts, aes(x = "", y = percent, fill = value)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = lc_colors) +
  labs(title = "Madison County") +
  geom_label_repel(data = ma_pie_chart,
                   aes(y = pos, label = paste0(paste0(value," "), paste0(percent*100, "%")), fill = NULL),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  theme(panel.background = element_blank(),
        title = element_text(hjust = 1.5),
        legend.position = "none",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text()) 


## pie chart label locations
on_pie_chart <- on_lc_counts %>% 
  mutate(csum = rev(cumsum(rev(percent))), 
         pos = percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent/2, pos))

## pie chart
ggplot(on_lc_counts, aes(x = "", y = percent, fill = value)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = lc_colors) +
  labs(title = "Onondaga County") +
  geom_label_repel(data = on_pie_chart,
                   aes(y = pos, label = paste0(paste0(value," "), paste0(percent*100, "%")), fill = NULL),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  theme(panel.background = element_blank(),
        title = element_text(hjust = 1.5),
        legend.position = "none",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text()) 


## pie chart label locations
os_pie_chart <- os_lc_counts %>% 
  mutate(csum = rev(cumsum(rev(percent))), 
         pos = percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), percent/2, pos))

## pie chart
ggplot(os_lc_counts, aes(x = "", y = percent, fill = value)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = lc_colors) +
  labs(title = "Oswego County") +
  geom_label_repel(data = os_pie_chart,
                   aes(y = pos, label = paste0(paste0(value," "), paste0(percent*100, "%")), fill = NULL),
                   size = 2, nudge_x = 1, show.legend = FALSE) +
  theme(panel.background = element_blank(),
        title = element_text(hjust = 1.5),
        legend.position = "none",
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text())


## get the top three landcover percentages for each county
county_lc <- all_lc_counts %>% 
  group_by(county) %>% 
  slice_max(order_by = percent, n = 3) %>% 
  select(county, value)
county_lc$order <- c(rep(c("first", "second", "third"), 5))
county_lc <- county_lc %>% 
  pivot_wider(names_from = order, values_from = value)

# gbif data prep ----------------------------------------------------------
## do not run if you already have data, dowloading data takes a long time

## set options
options(gbif_user = "") # fill in with gbif username
options(gbif_email = "") # fill in with gbif associated email
options(gbif_pwd = "") # fill in with gbif account password

## get all bird observations in each county shape file
birdKey <- name_backbone(name = "Aves")

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
  wkt(on_wkt),
  direction = wk_counterclockwise()
)
on_ccw_shape <- as.character(on_ccw)

os_ccw <- wk_orient(
  wkt(os_wkt),
  direction = wk_counterclockwise()
)
os_ccw_shape <- as.character(os_ccw)

## request download of data
ca_gbif_down <- occ_download(pred("taxonKey", birdKey$usageKey),
             pred_or(pred("year", 2014),
                     pred("year", 2024)),
             pred_within(ca_ccw_shape))
## notifies when data is downloaded
ca_gbif_meta <- occ_download_wait(ca_gbif_down, status_ping = 10, quiet = FALSE) 
## get zip file
ca_gbif_get <- occ_download_get(ca_gbif_down) 
ca_gbif_data <- occ_download_import(ca_gbif_get)
## create csv
write.csv(ca_gbif_data, file = "cayugaGBIF.csv")
cayuga_GBIF_df <- read.csv("cayugaGBIF.csv")


## request download of data
co_gbif_down <- occ_download(pred("taxonKey", birdKey$usageKey),
                             pred_or(pred("year", 2014),
                                     pred("year", 2024)),
                             pred_within(co_ccw_shape))
## notifies when data is downloaded
co_gbif_meta <- occ_download_wait(co_gbif_down, status_ping = 10, quiet = FALSE) 
## get zip file
co_gbif_get <- occ_download_get(co_gbif_down) 
co_gbif_data <- occ_download_import(co_gbif_get)
## create csv
write.csv(co_gbif_data, file = "cortlandGBIF.csv")
cortland_GBIF_df <- read.csv("cortlandGBIF.csv")


## request download of data
ma_gbif_down <- occ_download(pred("taxonKey", birdKey$usageKey),
                             pred_or(pred("year", 2014),
                                     pred("year", 2024)),
                             pred_within(ma_ccw_shape))
## notifies when data is downloaded
ma_gbif_meta <- occ_download_wait(ma_gbif_down, status_ping = 10, quiet = FALSE) 
## get zip file
ma_gbif_get <- occ_download_get(ma_gbif_down) 
ma_gbif_data <- occ_download_import(ma_gbif_get)
## create csv
write.csv(ma_gbif_data, file = "madisonGBIF.csv")
madison_GBIF_df <- read.csv("madisonGBIF.csv")


## request download of data
on_gbif_down <- occ_download(pred("taxonKey", birdKey$usageKey),
                             pred_or(pred("year", 2014),
                                     pred("year", 2024)),
                             pred_within(on_ccw_shape)) 
## notifies when data is downloaded
on_gbif_meta <- occ_download_wait(on_gbif_down, status_ping = 10, quiet = FALSE) 
## get zip file
on_gbif_get <- occ_download_get(on_gbif_down) 
on_gbif_data <- occ_download_import(on_gbif_get)
## create csv
write.csv(on_gbif_data, file = "onondagaGBIF.csv")
onondaga_GBIF_df <- read.csv("onondagaGBIF.csv")


## request download of data
os_gbif_down <- occ_download(pred("taxonKey", birdKey$usageKey),
                             pred_or(pred("year", 2014),
                                     pred("year", 2024)),
                             pred_within(os_ccw_shape))
## notifies when data is downloaded
os_gbif_meta <- occ_download_wait(os_gbif_down, status_ping = 10, quiet = FALSE) 
## get zip file
os_gbif_get <- occ_download_get(os_gbif_down) 
os_gbif_data <- occ_download_import(os_gbif_get)
## create csv
write.csv(os_gbif_data, file = "oswegoGBIF.csv")
oswego_GBIF_df <- read.csv("oswegoGBIF.csv")


# working with gbif data --------------------------------------------------

## manipulating gbif data
cayuga_GBIF_df <- cayuga_GBIF_df[,
  c("scientificName", "taxonKey", "classKey", "family", "familyKey", "species",
    "decimalLongitude", "decimalLatitude",
    "year", "month", "day", "eventDate", 
    "countryCode", "municipality", "stateProvince", 
    "catalogNumber", "mediaType", "datasetKey",
    "basisOfRecord", "individualCount")
]
## replace NA values with 1 for number of observations
cayuga_GBIF_df <- cayuga_GBIF_df %>% 
  mutate(individualCount = ifelse(is.na(individualCount), 1, individualCount))
## turn into shapefile
cayuga_GBIF_df$lon <- as.double(cayuga_GBIF_df$decimalLongitude)
cayuga_GBIF_df$lat <- as.double(cayuga_GBIF_df$decimalLatitude)
cayuga_GBIF_sf <- st_as_sf(cayuga_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
st_crs(cayuga_GBIF_sf) <- st_crs(cayuga_shape)

plot(cayuga_GBIF_sf)


## manipulating gbif data
cortland_GBIF_df <- cortland_GBIF_df[,
                                 c("scientificName", "taxonKey", "classKey", "family", "familyKey", "species",
                                   "decimalLongitude", "decimalLatitude",
                                   "year", "month", "day", "eventDate", 
                                   "countryCode", "municipality", "stateProvince", 
                                   "catalogNumber", "mediaType", "datasetKey",
                                   "basisOfRecord", "individualCount")
]
## replace NA values with 1 for number of observations
cortland_GBIF_df <- cortland_GBIF_df %>% 
  mutate(individualCount = ifelse(is.na(individualCount), 1, individualCount))
## turn into shapefile
cortland_GBIF_df$lon <- as.double(cortland_GBIF_df$decimalLongitude)
cortland_GBIF_df$lat <- as.double(cortland_GBIF_df$decimalLatitude)
cortland_GBIF_sf <- st_as_sf(cortland_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
st_crs(cortland_GBIF_sf) <- st_crs(cortland_shape)

plot(cortland_GBIF_sf)


## manipulating gbif data
madison_GBIF_df <- madison_GBIF_df[,
                                     c("scientificName", "taxonKey", "classKey", "family", "familyKey", "species",
                                       "decimalLongitude", "decimalLatitude",
                                       "year", "month", "day", "eventDate", 
                                       "countryCode", "municipality", "stateProvince", 
                                       "catalogNumber", "mediaType", "datasetKey",
                                       "basisOfRecord", "individualCount")
]
## replace NA values with 1 for number of observations
madison_GBIF_df <- madison_GBIF_df %>% 
  mutate(individualCount = ifelse(is.na(individualCount), 1, individualCount))
## turn into shapefile
madison_GBIF_df$lon <- as.double(madison_GBIF_df$decimalLongitude)
madison_GBIF_df$lat <- as.double(madison_GBIF_df$decimalLatitude)
madison_GBIF_sf <- st_as_sf(madison_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
st_crs(madison_GBIF_sf) <- st_crs(madison_shape)

plot(madison_GBIF_sf)


## manipulating gbif data
onondaga_GBIF_df <- onondaga_GBIF_df[,
                                   c("scientificName", "taxonKey", "classKey", "family", "familyKey", "species",
                                     "decimalLongitude", "decimalLatitude",
                                     "year", "month", "day", "eventDate", 
                                     "countryCode", "municipality", "stateProvince", 
                                     "catalogNumber", "mediaType", "datasetKey",
                                     "basisOfRecord", "individualCount")
]
## replace NA values with 1 for number of observations
onondaga_GBIF_df <- onondaga_GBIF_df %>% 
  mutate(individualCount = ifelse(is.na(individualCount), 1, individualCount))
## turn into shapefile
onondaga_GBIF_df$lon <- as.double(onondaga_GBIF_df$decimalLongitude)
onondaga_GBIF_df$lat <- as.double(onondaga_GBIF_df$decimalLatitude)
onondaga_GBIF_sf <- st_as_sf(onondaga_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
st_crs(onondaga_GBIF_sf) <- st_crs(onondaga_shape)

plot(onondaga_GBIF_sf)


## manipulating gbif data
oswego_GBIF_df <- oswego_GBIF_df[,
                                     c("scientificName", "taxonKey", "classKey", "family", "familyKey", "species",
                                       "decimalLongitude", "decimalLatitude",
                                       "year", "month", "day", "eventDate", 
                                       "countryCode", "municipality", "stateProvince", 
                                       "catalogNumber", "mediaType", "datasetKey",
                                       "basisOfRecord", "individualCount")
]
## replace NA values with 1 for number of observations
oswego_GBIF_df <- oswego_GBIF_df %>% 
  mutate(individualCount = ifelse(is.na(individualCount), 1, individualCount))
## turn into shapefile
oswego_GBIF_df$lon <- as.double(oswego_GBIF_df$decimalLongitude)
oswego_GBIF_df$lat <- as.double(oswego_GBIF_df$decimalLatitude)
oswego_GBIF_sf <- st_as_sf(oswego_GBIF_df, coords = c("lon", "lat"), remove = FALSE)
st_crs(oswego_GBIF_sf) <- st_crs(oswego_shape)

plot(oswego_GBIF_sf)


## year separations
cayuga_GBIF_sf_2014 <- cayuga_GBIF_sf %>% 
  filter(year == 2014)
cayuga_GBIF_sf_2024 <- cayuga_GBIF_sf %>% 
  filter(year == 2024)

cortland_GBIF_sf_2014 <- cortland_GBIF_sf %>% 
  filter(year == 2014)
cortland_GBIF_sf_2024 <- cortland_GBIF_sf %>% 
  filter(year == 2024)

madison_GBIF_sf_2014 <- madison_GBIF_sf %>% 
  filter(year == 2014)
madison_GBIF_sf_2024 <- madison_GBIF_sf %>% 
  filter(year == 2024)

onondaga_GBIF_sf_2014 <- onondaga_GBIF_sf %>% 
  filter(year == 2014)
onondaga_GBIF_sf_2024 <- onondaga_GBIF_sf %>% 
  filter(year == 2024)

oswego_GBIF_sf_2014 <- oswego_GBIF_sf %>% 
  filter(year == 2014)
oswego_GBIF_sf_2024 <- oswego_GBIF_sf %>% 
  filter(year == 2024)

## get family data
cayuga_family_abundance2014 <- na.omit(cayuga_GBIF_sf_2014) %>% 
  group_by(family) %>% 
  summarize(freq_2014 = sum(individualCount))
cayuga_family_abundance2024 <- na.omit(cayuga_GBIF_sf_2024) %>% 
  group_by(family) %>% 
  summarize(freq_2024 = sum(individualCount))
cayuga_family_abundance2014 <- as.data.frame(cayuga_family_abundance2014) %>% 
  select(-geometry)
cayuga_family_abundance2024 <- as.data.frame(cayuga_family_abundance2024)%>% 
  select(-geometry)
cayuga_family_abundance <- merge(cayuga_family_abundance2014, cayuga_family_abundance2024, all = TRUE)
## replace NA values with 0 to represent no individuals present
cayuga_family_abundance[is.na(cayuga_family_abundance)] <- 0
## get the difference of individual abundances across families between years
cayuga_family_abundance$difference <- cayuga_family_abundance$freq_2024 - cayuga_family_abundance$freq_2014


cortland_family_abundance2014 <- na.omit(cortland_GBIF_sf_2014) %>% 
  group_by(family) %>% 
  summarize(freq_2014 = sum(individualCount))
cortland_family_abundance2024 <- na.omit(cortland_GBIF_sf_2024) %>% 
  group_by(family) %>% 
  summarize(freq_2024 = sum(individualCount))
cortland_family_abundance2014 <- as.data.frame(cortland_family_abundance2014) %>% 
  select(-geometry)
cortland_family_abundance2024 <- as.data.frame(cortland_family_abundance2024)%>% 
  select(-geometry)
cortland_family_abundance <- merge(cortland_family_abundance2014, cortland_family_abundance2024, all = TRUE)
## replace NA values with 0 to represent no individuals present
cortland_family_abundance[is.na(cortland_family_abundance)] <- 0
## get the difference of individual abundances across families between years
cortland_family_abundance$difference <- cortland_family_abundance$freq_2024 - cortland_family_abundance$freq_2014


madison_family_abundance2014 <- na.omit(madison_GBIF_sf_2014) %>% 
  group_by(family) %>% 
  summarize(freq_2014 = sum(individualCount))
madison_family_abundance2024 <- na.omit(madison_GBIF_sf_2024) %>% 
  group_by(family) %>% 
  summarize(freq_2024 = sum(individualCount))
madison_family_abundance2014 <- as.data.frame(madison_family_abundance2014) %>% 
  select(-geometry)
madison_family_abundance2024 <- as.data.frame(madison_family_abundance2024)%>% 
  select(-geometry)
madison_family_abundance <- merge(madison_family_abundance2014, madison_family_abundance2024, all = TRUE)
## replace NA values with 0 to represent no individuals present
madison_family_abundance[is.na(madison_family_abundance)] <- 0
## get the difference of individual abundances across families between years
madison_family_abundance$difference <- madison_family_abundance$freq_2024 - madison_family_abundance$freq_2014


onondaga_family_abundance2014 <- na.omit(onondaga_GBIF_sf_2014) %>% 
  group_by(family) %>% 
  summarize(freq_2014 = sum(individualCount))
onondaga_family_abundance2024 <- na.omit(onondaga_GBIF_sf_2024) %>% 
  group_by(family) %>% 
  summarize(freq_2024 = sum(individualCount))
onondaga_family_abundance2014 <- as.data.frame(onondaga_family_abundance2014) %>% 
  select(-geometry)
onondaga_family_abundance2024 <- as.data.frame(onondaga_family_abundance2024)%>% 
  select(-geometry)
onondaga_family_abundance <- merge(onondaga_family_abundance2014, onondaga_family_abundance2024, all = TRUE)
## replace NA values with 0 to represent no individuals present
onondaga_family_abundance[is.na(onondaga_family_abundance)] <- 0
## get the difference of individual abundances across families between years
onondaga_family_abundance$difference <- onondaga_family_abundance$freq_2024 - onondaga_family_abundance$freq_2014


oswego_family_abundance2014 <- na.omit(oswego_GBIF_sf_2014) %>% 
  group_by(family) %>% 
  summarize(freq_2014 = sum(individualCount))
oswego_family_abundance2024 <- na.omit(oswego_GBIF_sf_2024) %>% 
  group_by(family) %>% 
  summarize(freq_2024 = sum(individualCount))
oswego_family_abundance2014 <- as.data.frame(oswego_family_abundance2014) %>% 
  select(-geometry)
oswego_family_abundance2024 <- as.data.frame(oswego_family_abundance2024)%>% 
  select(-geometry)
oswego_family_abundance <- merge(oswego_family_abundance2014, oswego_family_abundance2024, all = TRUE)
## replace NA values with 0 to represent no individuals present
oswego_family_abundance[is.na(oswego_family_abundance)] <- 0
## get the difference of individual abundances across families between years
oswego_family_abundance$difference <- oswego_family_abundance$freq_2024 - oswego_family_abundance$freq_2014

# analysis ----------------------------------------------------------------

## family abundance
cayuga_family_abundance$county <- 'cayuga'
cortland_family_abundance$county <- 'cortland'
madison_family_abundance$county <- 'madison'
onondaga_family_abundance$county <- 'onondaga'
oswego_family_abundance$county <- 'oswego'

### combining all counties abundance data
county_family_abundance <- rbind(cayuga_family_abundance, cortland_family_abundance,
                                 madison_family_abundance, onondaga_family_abundance,
                                 oswego_family_abundance)
### look at the bottom and top 3 family changes
county_family_abundance %>%
  top_n(-3, difference)
county_family_abundance %>%
  top_n(3, difference)

### create model to see how family abundances changes are influenced by landcover
county_fam_abundance_lc <- merge(county_family_abundance, county_lc)

family.model <- lm(data = county_fam_abundance_lc, difference~first+second) # best fit with only top 2
summary(family.model)

emmeans(family.model, ~first+second)

## overall abundance change per county
county_abundance_change <- county_fam_abundance_lc %>% 
  group_by(county, first, second, third) %>%
  summarize(abundance_change = sum(difference))

### create model to see how overall abundance changes are influenced by landcover
overall.model <- lm(data = county_abundance_change, abundance_change~first+second) # best fit with only top 2
summary(overall.model)

emmeans(overall.model, ~first+second)

ggplot(county_abundance_change, aes(x=first, y=second, fill = county)) +
  geom_bar(position=position_dodge(), stat="identity", color = 'black') +
  scale_fill_manual(values = c('#004965','#952611', '#004965', '#004965', '#004965'))+
  labs(x = 'County', y = "Change in total number of species")+
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

## species number changes
ca_species <- data.frame(species2014 = length(unique(cayuga_GBIF_sf_2014$species)),
                     species2024 = length(unique(cayuga_GBIF_sf_2024$species)))
ca_species$county = 'cayuga'
co_species <- data.frame(species2014 = length(unique(cortland_GBIF_sf_2014$species)),
                     species2024 = length(unique(cortland_GBIF_sf_2024$species)))
co_species$county = 'cortland'
ma_species <- data.frame(species2014 = length(unique(madison_GBIF_sf_2014$species)),
                     species2024 = length(unique(madison_GBIF_sf_2024$species)))
ma_species$county = 'madison'
on_species <- data.frame(species2014 = length(unique(onondaga_GBIF_sf_2014$species)),
                     species2024 = length(unique(onondaga_GBIF_sf_2024$species)))
on_species$county = 'onondaga'
os_species <- data.frame(species2014 = length(unique(oswego_GBIF_sf_2014$species)),
                     species2024 = length(unique(oswego_GBIF_sf_2024$species)))
os_species$county = 'oswego'

### combine all counties
county_species <- rbind(ca_species, co_species, ma_species, on_species, os_species)
species_count <- merge(county_abundance_change, county_species)
## get the difference of total species count between years
species_count$species_change <- species_count$species2024 - species_count$species2014

## plot species changes per county
ggplot(species_count, aes(x=reorder(county, -species_change), y=species_change, fill = county)) +
  geom_bar(position=position_dodge(), stat="identity", color = 'black') +
  scale_fill_manual(values = c('#004965','#952611', '#004965', '#004965', '#004965'))+
  labs(x = 'County', y = "Change in total number of species")+
  geom_hline(aes(yintercept = 0))+
  theme_classic()+
  theme(legend.position = 'none',
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))

### create model to see how overall abundance changes are influenced by landcover
species.model <- lm(data = species_count, species_change~first+second) # best fit with only top 2
summary(species.model)

emmeans(species.model, ~first+second)

## interesting case: cayuga family
cayuga_biggest_change <- cayuga_family_abundance %>%
  top_n(-1, difference)

cayuga_GBIF_sf_big_change <- cayuga_GBIF_sf %>%
  filter(family == cayuga_biggest_change$family)

ggplot() +
  geom_sf(data = cayuga_shape) +
  geom_sf(data = cayuga_GBIF_sf_big_change[, 1], aes(col = as.factor(cayuga_GBIF_sf_big_change$year))) +
  theme_void() +
  scale_color_manual(values = c("#d5d602", "#ca436f")) +
  labs(title = "Occurrences of Anatidae in Cayuga County\nin 2014 and 2024", col = "Year") 
  theme(legend.position = "none")
  
  
## looking at difference in observation count between years

obs_data <- data.frame('county' = rep(c('cayuga', 'cortland', 'madison', 'onondaga', 'oswego'), 2),
                       'obs' = c(nrow(cayuga_GBIF_sf_2014), nrow(cortland_GBIF_sf_2014), 
                                     nrow(madison_GBIF_sf_2014),
                                     nrow(onondaga_GBIF_sf_2014), nrow(oswego_GBIF_sf_2014),
                                 nrow(cayuga_GBIF_sf_2024), nrow(cortland_GBIF_sf_2024), 
                                 nrow(madison_GBIF_sf_2024),
                                 nrow(onondaga_GBIF_sf_2024), nrow(oswego_GBIF_sf_2024)),
                       'year' = c(rep(2014, 5), rep(2024, 5)))

ggplot(obs_data, aes(x = county , y= obs, fill = as.factor(year))) +
  geom_bar(position="dodge", stat = "identity") +
  theme_classic() +
  labs(x = "County", y = "Number of Observations", fill = "Year") +
  scale_fill_manual(values = c("#d5d602", "#ca436f")) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"))
