# Pipeline: Data processing of bud burst DwC-A - Calculation of bud burst dates per tree and year ####

# Authors: Cherine Jantzen, Stefan Vriend
# Created: 14/11/2023
# Last updated: 18/12/2023

# Part I: Preparation  ----------------------------------------------------

# Load packages
library(tidyverse)
library(here)

# Load DwC-A files
event <- read.csv(here::here("data", "event.txt"), sep = "\t")
occ <- read.csv(here::here("data", "occurrence.txt"), sep = "\t")
mof <- read.csv(here::here("data", "extendedmeasurementorfact.txt"), sep = "\t")

# Connect measurements with events (i.e., trees)
d_bb <- dplyr::left_join(event, mof, by = "eventID")

# Get organism ID for each event
d_bb <-
  occ %>%
  dplyr::select("eventID", "organismID", "scientificName") %>%
  dplyr::right_join(d_bb, by = "eventID", relationship = "many-to-many")


# Part II: Queries --------------------------------------------------------

# 1. Define bud burst criteria ####

# Set measurement type criterion: TopScore (= bud burst stage (PO:0025532) of the tree crown)
crit_measType <- "bud burst stage (PO:0025532) of the tree crown"

# First day where measurement value (of the tree crown) > 1 is bud burst date
crit_measValue <- 1


# 2. Select dates before reaching criterion and when reaching criterion per tree per year ####

# Filter for tree crown, which is used to determine bud burst dates
d_bb_crown <-
  d_bb %>%
  dplyr::filter(measurementType == crit_measType) %>%
  ## Convert date to day of year for easier calculations
  dplyr::mutate(DOY = lubridate::yday(eventDate))

# Bud burst date is the first date where the bud burst stage of the tree crown is scored >= 1
# First, we get the earliest dates (and associated bud burst stage values) per tree per year that matches this criterion
min_bb <- d_bb_crown %>%
  dplyr::filter(measurementValue >= crit_measValue) %>%
  dplyr::summarise(min_DOY_above_criterion = min(DOY),
                   min_value = measurementValue[DOY == min_DOY_above_criterion],
                   .by = c("year", "organismID"))

# Join earliest date and value back into main data frame
d_bb_crown2 <- dplyr::left_join(d_bb_crown,
                                min_bb,
                                by = c("year", "organismID"))

# Second, we also need the latest date (and associated value) at which the bud burst stage was below the criterion
# This is used to interpolate a bud burst date in cases where the tree reached stage 1 in-between two field visits
# In other cases, where stage 1 was observed during a field visit, the date of that visit is used as the bud burst date
max_bb <- d_bb_crown2 %>%
  dplyr::filter(measurementValue < crit_measValue & DOY < min_DOY_above_criterion,
                .by = c("year", "organismID")) %>%
  dplyr::summarise(max_DOY_below_criterion = max(DOY),
                   max_value = measurementValue[DOY == max_DOY_below_criterion],
                   .by = c("year", "organismID"))

# Join earliest date at criterion and latest date below criterion, and their recorded bud burst values
min_max_bb <- dplyr::left_join(min_bb,
                               max_bb,
                               by = c("year", "organismID"))

# 3. Calculate bud burst dates per tree per year ####

# For trees for which a stage of 1 was observed during a visit, use the date of that visit as the bud burst date
match_criterion <- min_max_bb %>%
  dplyr::filter(min_value == crit_measValue) %>%
  dplyr::mutate(bud_burst_date = min_DOY_above_criterion + lubridate::make_date(year, 1, 1) - 1) %>%
  dplyr::rename("bud_burst_DOY" = "min_DOY_above_criterion")

# For trees for which a stage 1 was NOT observed during a visit, (linearly) interpolate the date
# from the latest date before criterion and the earliest date after reaching criterion
interpolated <- min_max_bb %>%
  dplyr::filter(min_value != crit_measValue) %>%
  dplyr::mutate(diff_date = min_DOY_above_criterion - max_DOY_below_criterion,
                diff_value = min_value - max_value,
                value_per_day = diff_value / diff_date,
                days_to_reach_criterion = (crit_measValue - max_value) / value_per_day,
                interpolated_DOY = max_DOY_below_criterion + days_to_reach_criterion,
                interpolated_value  = max_value + days_to_reach_criterion * value_per_day) %>%
  dplyr::mutate(bud_burst_date = round(interpolated_DOY) + lubridate::make_date(year, 1, 1) - 1) %>%
  dplyr::rename("bud_burst_DOY" = "interpolated_DOY")

# Merge determined or calculated bud burst dates and bud burst DOYs per tree per year together
# and add auxiliary info (i.e., locality, scientific name) back in from main file
bud_burst_dates <- dplyr::bind_rows(match_criterion %>%
                                      dplyr::select("year", "organismID", "bud_burst_date", "bud_burst_DOY"),
                                    interpolated %>%
                                      dplyr::select("year", "organismID", "bud_burst_date", "bud_burst_DOY")) %>%
  dplyr::left_join(d_bb_crown %>%
                     dplyr::select("year", "organismID", "verbatimLocality", "scientificName") %>%
                     dplyr::distinct(),
                   by = c("year", "organismID")) %>%
  dplyr::arrange(year, organismID)

# Save dates as csv
# We save both "bud_burst_date" (whole "integer" dates) and "bud_burst_DOY" (allowing non-integers)
write.csv(bud_burst_dates, here::here("data", "annual_budburst_per_tree.csv"), row.names = FALSE)
