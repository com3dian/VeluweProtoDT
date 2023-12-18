# Pipeline: Data processing of bud burst DwC-A - Calculation of bud burst dates per tree and year ####

# Author: Cherine Jantzen
# Created: 14/11/2023
# Last updated: 15/12/2023

# Part I: Preparation  ----------------------------------------------------

# load packages
library(tidyverse)
library(here)

# load DwC-A files
event <- read.csv(here::here("data", "event.txt"), sep = "\t")
occ <- read.csv(here::here("data", "occurrence.txt"), sep = "\t")
mof <- read.csv(here::here("data", "extendedmeasurementorfact.txt"), sep = "\t")

# connect measurements with events (i.e., trees)
d_bb <- dplyr::left_join(event, mof, by = "eventID")

# get organism ID for each event
d_bb <-
  occ %>%
  dplyr::select("eventID", "organismID", "scientificName") %>%
  dplyr::right_join(d_bb, by = "eventID", relationship = "many-to-many")


# Part II: Queries --------------------------------------------------------

# 1. Define bud burst criteria ####

## set measurement type criterion: TopScore (= bud burst stage (PO:0025532) of the tree crown)
crit_measType <- "bud burst stage (PO:0025532) of the tree crown"

## first day where measurement value (of the tree crown) > 1 is bud burst date
crit_measValue <- 1


# 2. Trees with measures above bud burst criterion ####

## filter for measurement type criterion
d_bb <-
  d_bb %>%
  dplyr::filter(measurementType == crit_measType) %>%
  ## convert date to day of year for easier calculations
  dplyr::mutate(DOY = lubridate::yday(eventDate))

## get minimum dates per tree per year above measurement value criterion
min_d_bb <- d_bb |>
  dplyr::filter(measurementValue >= crit_measValue) %>%
  dplyr::filter(DOY == min(DOY), .by = c("year", "organismID")) %>%
  dplyr::rename("min_DOY" = "DOY")

## Add minimum dates to bud burst data
d_bb_1 <- dplyr::left_join(d_bb,
                           min_d_bb %>%
                             dplyr::select("year", "organismID", "min_DOY"),
                           by = c("year", "organismID"))


# 3. Deal with irregular measures in 1991 (influence of late frost) ####

## filter for 1991, measures below criterion & dates earlier than minimum DOY
q3_1 <- d_bb_1 %>%
  dplyr::filter(year == 1991 & measurementValue < crit_measValue & DOY < min_DOY) %>%
  dplyr::rename(DOY_1991 = "DOY",
                measurementValue_1991 = "measurementValue")

## add 1991 DOY and measurements to bud burst table
d_bb_2 <-
  q3_1 %>%
  dplyr::select("year", "organismID", "DOY_1991", "measurementValue_1991") %>%
  dplyr::right_join(d_bb,
                    by = c("year" = "year", "organismID" = "organismID"),
                    relationship = "many-to-many", keep = FALSE)

## combine real values with corrected values for 1991
d_bb_2 <- d_bb_2 %>% dplyr::mutate(newDOY = dplyr::if_else(year == 1991, DOY_1991, DOY),
                                   newmeasurementValue = dplyr::if_else(year == 1991,
                                                                        measurementValue_1991,
                                                                        measurementValue))

# test <- d_bb_1 |>
#   dplyr::group_by(year, organismID, measurementValue) %>%
#   dplyr::filter(measurementValue < crit_measValue) %>%
#   dplyr::summarise(MaxDOY = max(DOY)) %>%
#   dplyr::ungroup()

# 4. Trees with measures below bud burst criterion ####
q3_2 <-
  d_bb_2 %>%
  dplyr::group_by(year, organismID, newmeasurementValue) %>%
  dplyr::filter(newmeasurementValue < crit_measValue) %>%
  dplyr::summarise(MaxDOY = max(newDOY)) %>%
  dplyr::ungroup()

## get maximum of MaxDOY and measurementValue per tree and year
q4 <- q3_2 %>%
  dplyr::group_by(year, organismID) %>%
  dplyr::summarise(MaxofMaxDOY = max(MaxDOY),
                   MaxmeasurementValue = max(newmeasurementValue)) %>%
  dplyr::ungroup()

# 5. Interpolation ####

## connect tables with total scores above and below criterion
d_bb_int <- dplyr::inner_join(min_d_bb, q4, by = c("organismID", "year"))

## interpolate the bud burst DOY for all trees that have never been measured at criterion value
q5 <-
  d_bb_int %>%
  dplyr::select("year", "organismID", "MaxofMaxDOY", "MaxmeasurementValue", "MinOfMinDOY", "measurementValue") %>%
  dplyr::mutate(Diff_Date = MinOfMinDOY - MaxofMaxDOY,
                Diff_Score = measurementValue - MaxmeasurementValue,
                S_per_D = Diff_Score/Diff_Date,
                dtrc = (crit_measValue - MaxmeasurementValue)/S_per_D,
                int_Date = MaxofMaxDOY + dtrc,
                int_Score = MaxmeasurementValue + (dtrc * S_per_D)) %>%
  dplyr::filter(measurementValue > crit_measValue)

# 6. Create final table ####
q6 <- min_d_bb %>%
  dplyr::select("year", "organismID", "verbatimLocality", "MinOfMinDOY", "measurementValue", "scientificName") %>%
  dplyr::left_join(q5 %>%
                     dplyr::select(organismID, year, int_Date, int_Score),
                   by = c("year", "organismID"))

## calculate the average bud burst date per tree within a year & create final table
q7 <- q6 %>%
  dplyr::mutate(bb_date_doy = dplyr::if_else(measurementValue == crit_measValue, MinOfMinDOY, int_Date)) %>%
  dplyr::group_by(year, verbatimLocality, organismID) %>%
  dplyr::mutate(n_Tree = n(),
                bb_date = as.Date(round(bb_date_doy) - 1, origin = paste0(year, "-01-01"))) %>% # rounding up or down here?
  dplyr::ungroup() %>%
  dplyr::select("year", "verbatimLocality", "scientificName", "organismID", "bb_date_doy", "bb_date", "n_Tree")

## save as CSV
write.csv(q7, here::here("data", "annual_budburst_per_tree.csv"), row.names = FALSE)
