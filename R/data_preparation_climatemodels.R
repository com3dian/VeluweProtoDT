# Pipeline: Data preparation for climate models: bud burst dates and temperature #####

# Author: Cherine Jantzen 
# Created: 28/11/2023
# Last updated: 19/12/2023

# I. Load packages & retrieve data ----------------------------------------------------------

# load packages
library(dplyr)
library(geosphere)
library(here)
library(tidyr)

# DwC-A files
event <- read.csv(here::here("data", "event.txt"), sep = "\t")
occ <- read.csv(here::here("data","occurrence.txt"), sep = "\t")
mof <- read.csv(here::here("data","extendedmeasurementorfact.txt"), sep = "\t")

#  processed bud burst data
bud_burst_dates <- read.csv(here::here("data","annual_budburst_per_tree.csv"))

# temperature data
temp <- read.csv(here::here("data", "Tg1_seasonalTemperature_Dec1987_to_June2023.csv")) %>% 
  rename(Longitude = "x", 
         Latitude = "y")


# II. Prepare data for the climate models -----------------------------------------

## 1. Get relevant information to map trees to temperature grid cells ####
# The temperature data of the KNMI is gridded and for trees to be modeled with their 
# respective temperatures, they first need to be matched to the temperature from the grid cell closest to them.  

# get organismID & scientific name from occurrence file
d_bb <- occ %>% 
  dplyr::select("eventID", "organismID", "scientificName") %>% 
  dplyr::right_join(event, by = "eventID", relationship = "many-to-many")

# connect measurements with events (i.e., trees) & filter for Hoge Veluwe
d_bb <- d_bb %>% 
  dplyr::left_join(bud_burst_dates, by = c("year", "scientificName", "organismID", "verbatimLocality")) %>% 
  dplyr::filter(verbatimLocality == "Hoge Veluwe")

# get the distinct longitude and latitude for temperature
lon_lat_temp <- temp %>%
  tidyr::unite("combcoord", Longitude, Latitude, sep = "_", remove = FALSE) %>% 
  dplyr::distinct(., combcoord, .keep_all = TRUE) %>% 
  dplyr::select("Longitude", "Latitude")

# get all individual trees
trees <- d_bb %>% 
  dplyr::distinct(., organismID, .keep_all = TRUE) %>% 
  dplyr::filter(decimalLongitude != "NA")

# select only coordinates of each individual tree
tree_cord <- trees %>% 
  dplyr::select("decimalLongitude", "decimalLatitude") 


## 2. Map trees to temperature grid cells ####

# calculate the geographic distance between each tree & each temperature coordinate pair 
dist <- as.data.frame(geosphere::distm(tree_cord, lon_lat_temp))

# find the minimum distance between tree and temperature coordinates
dist$minPos <- apply(dist, 1, which.min)

# assign position name to coordinate pairs to match with positions of minimum value
lon_lat_temp$Pos <- 1:nrow(lon_lat_temp)

# assign coordinates of the temperature measures closest to each tree to bud burst data
d_bb_1 <- dplyr::left_join(dist, lon_lat_temp, by = c("minPos" = "Pos")) %>% 
  dplyr::select("Longitude", "Latitude") %>% 
  dplyr::rename(tempLon = "Longitude", tempLat = "Latitude") %>% 
  dplyr::bind_cols(trees, .) %>% 
  dplyr::select("organismID", "tempLon", "tempLat") %>%
  dplyr::right_join(d_bb, by = "organismID")



# III. Create location IDs for each temperature measure point ------------------

# locID for each temperature coordinate pair
locID <-
  d_bb_1 %>% 
  tidyr::unite("combcoord", tempLon, tempLat, sep = "_", remove = FALSE) %>%
  dplyr::distinct(., combcoord) %>%
  dplyr::mutate(locID = paste0("loc", c(1:length(combcoord))))

# locID for each temperature measure & exclusion of locations without trees
temp <-
  temp %>% 
  tidyr::unite("combcoord", Longitude, Latitude, sep = "_", remove = FALSE) %>%
  dplyr::left_join(locID, by = "combcoord") %>% 
  dplyr::filter(locID != "NA")


# IV. Annual average bud burst dates  ----------------------------

# assign locID for bud burst measures and get annual average bud burst date per locID
d_bb_2 <-
  d_bb_1 %>% 
  tidyr::unite("combcoord", tempLon, tempLat, sep = "_", remove = FALSE) %>%
  dplyr::left_join(locID, by = "combcoord") %>%
  dplyr::group_by(locID, year, scientificName) %>%
  dplyr::summarise(avg_bud_burst_DOY = mean(bud_burst_DOY, na.rm = TRUE)) %>%
  dplyr::mutate(avg_bud_burst_date = as.Date(avg_bud_burst_DOY - 1, origin = paste0(year, "-01-01"))) %>%
  dplyr::ungroup()


# V. Save output files ----------------------------------------------------

# bud burst data 
write.csv(d_bb_2, file = here::here("data", "budburst_climwin_input.csv"), sep = ',', row.names = FALSE)

# temperature data
write.csv(temp, file = here::here("data", "temp_climwin_input.csv"), sep = ',', row.names = FALSE)
