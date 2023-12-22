# Pipeline: Data preparation for climate models: bud burst dates and temperature #####

# Authors: Cherine Jantzen, Stefan Vriend
# Created: 2023-11-28
# Last updated: 2023-12-21

# I. Load packages & retrieve data ----------------------------------------------------------

# load packages
library(dplyr)
library(geosphere)
library(here)
library(tidyr)
library(lubridate)

# Read in DwC-A tables
event <- read.csv(here::here("data", "event.txt"), sep = "\t")
occ <- read.csv(here::here("data","occurrence.txt"), sep = "\t")
mof <- read.csv(here::here("data","extendedmeasurementorfact.txt"), sep = "\t")

#  Read in processed bud burst data
bud_burst_dates <- read.csv(here::here("data","annual_budburst_per_tree.csv"))

# Read in temperature data
temp <- read.csv(here::here("data", "Tg1_seasonalTemperature_Dec1987_to_June2023.csv")) %>%
  dplyr::rename("Longitude" = "x",
                "Latitude" = "y")


# II. Prepare data for the climate models -----------------------------------------

## 1. Get relevant information to map trees to temperature grid cells ####
# The temperature data of the KNMI is gridded and for trees to be modeled with their
# respective temperatures, they first need to be matched to the temperature from the grid cell closest to them.

# Get organismID & scientific name from occurrence file and connect measurements with events (i.e., trees)
budburst <- dplyr::right_join(occ %>%
                                dplyr::select("eventID", "organismID", "scientificName"),
                              event, by = "eventID", relationship = "many-to-many") %>%
  dplyr::right_join(bud_burst_dates, by = c("year", "scientificName", "organismID", "verbatimLocality")) %>%
  # filter for Hoge Veluwe
  dplyr::filter(verbatimLocality == "Hoge Veluwe")

# Get the distinct longitude and latitude for temperature
lon_lat_temp <- temp %>%
  dplyr::distinct(Longitude, Latitude)

# Get all individual trees with location information
trees <- budburst %>%
  dplyr::distinct(organismID, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(decimalLongitude))

# Select coordinates of each individual tree
tree_coords <- trees %>%
  dplyr::select("decimalLongitude", "decimalLatitude")


## 2. Map trees to temperature grid cells ####

# Calculate the geographic distance between each tree & each temperature coordinate pair
distance <- as.data.frame(geosphere::distm(tree_coords, lon_lat_temp))

# Find the minimum distance between tree and temperature coordinates
distance$minPos <- apply(distance, 1, which.min)

# Assign position name to coordinate pairs to match with positions of minimum value
lon_lat_temp$Pos <- 1:nrow(lon_lat_temp)

# Assign coordinates of the temperature measures closest to each tree to bud burst data
budburst1 <- dplyr::left_join(distance, lon_lat_temp, by = c("minPos" = "Pos")) %>%
  dplyr::select("tempLon" = "Longitude",
                "tempLat" = "Latitude") %>%
  dplyr::bind_cols(trees, .) %>%
  dplyr::select("organismID", "tempLon", "tempLat") %>%
  dplyr::right_join(budburst, by = "organismID")


# III. Create location IDs for each temperature measure point ------------------

# Assign locID to each temperature-coordinate pair
# Exclude trees without location information
temp_locations <- budburst1 %>%
  dplyr::distinct(tempLon, tempLat) %>%
  tidyr::drop_na() %>%
  dplyr::mutate(locID = paste0("loc", 1:dplyr::n()))

# Add locID to each single temperature measure
temp <-
  temp %>%
  dplyr::left_join(temp_locations, by = c("Latitude" = "tempLat",
                                          "Longitude" = "tempLon"))

# IV. Annual average bud burst dates  ----------------------------

# Assign locID to bud burst measures and get annual average bud burst date per locID
avg_annual_budburst_dates <-
  budburst1 %>%
  dplyr::left_join(temp_locations, by = c("tempLat", "tempLon")) %>%
  dplyr::summarise(avg_bud_burst_DOY = mean(bud_burst_DOY, na.rm = TRUE),
                   .by = c("locID", "year", "scientificName")) %>%
  dplyr::mutate(avg_bud_burst_date = avg_bud_burst_DOY + lubridate::make_date(year, 1, 1) - 1)

# V. Save output files ----------------------------------------------------

# Save bud burst data
write.csv(avg_annual_budburst_dates, file = here::here("data", "budburst_climwin_input.csv"),
          row.names = FALSE)

# Save temperature data
write.csv(temp, file = here::here("data", "temp_climwin_input.csv"),
          row.names = FALSE)
