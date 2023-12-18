# Pipeline: Data processing of bud burst DwC-A - Calculation of bud burst dates per tree and year ####

# Author: Cherine Jantzen
# Created: 14/11/2023
# Last updated: 15/12/2023

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


# 2. Trees with measures above bud burst criterion ####

# Filter for tree crown, which is used to determine bud burst dates
d_bb <-
  d_bb %>%
  dplyr::filter(measurementType == crit_measType) %>%
  ## convert date to day of year for easier calculations
  dplyr::mutate(DOY = lubridate::yday(eventDate))

# Bud burst date is the first date where the bud burst stage of the tree crown is scored >= 1
# First, we get the earliest dates (and associated bud burst stage values) per tree per year that matches this criterion
min_d_bb <- d_bb %>%
  dplyr::filter(measurementValue >= crit_measValue) %>%
  dplyr::summarise(min_DOY_above_criterion = min(DOY),
                   min_value = measurementValue[DOY == min_DOY_above_criterion],
                   .by = c("year", "organismID"))

min_d_bb1 <- d_bb |>
  dplyr::filter(measurementValue >= crit_measValue) |>
  dplyr::filter(DOY == min(DOY), .by = c("year", "organismID")) |>
  dplyr::rename("min_DOY" = "DOY")

# Join earliest date and value back into main data frame
d_bb_1 <- dplyr::left_join(d_bb,
                           min_d_bb,
                           by = c("year", "organismID"))

# Second, we also need the latest date (and associated value) at which the bud burst stage was NOT above the criterion
# This is used to interpolate a bud burst date in cases where the tree reached stage 1 in-between two field visits
max_d_bb <- d_bb_1 %>%
  dplyr::filter(measurementValue < crit_measValue & DOY < min_DOY_above_criterion,
                .by = c("year", "organismID")) %>%
  dplyr::summarise(max_DOY_below_criterion = max(DOY),
                   max_value = measurementValue[DOY == max_DOY_below_criterion],
                   .by = c("year", "organismID"))

# Join latest date and value back into main data frame
test <- dplyr::left_join(min_d_bb,
                         max_d_bb,
                         by = c("year", "organismID"))

trees_match_criterion <- test %>%
  dplyr::filter(min_value == crit_measValue) |>
  dplyr::mutate(bud_burst_date = min_DOY_above_criterion + lubridate::make_date(year, 1, 1) - 1)

interpolated_trees <- test %>%
  dplyr::filter(min_value != crit_measValue) |>
  dplyr::mutate(diff_date = min_DOY_above_criterion - max_DOY_below_criterion,
                diff_value = min_value - max_value,
                value_per_day = diff_value / diff_date,
                days_to_reach_criterion = (crit_measValue - max_value) / value_per_day,
                interpolated_DOY = max_DOY_below_criterion + days_to_reach_criterion,
                interpolated_value  = max_value + days_to_reach_criterion * value_per_day) |>
  dplyr::mutate(bud_burst_date = round(interpolated_DOY) + lubridate::make_date(year, 1, 1) - 1)

test4 <- dplyr::bind_rows(trees_match_criterion |>
                            dplyr::select("year", "organismID", "bud_burst_date"),
                          interpolated_trees |>
                            dplyr::select("year", "organismID", "bud_burst_date")) |>
  dplyr::left_join(d_bb |>
                     dplyr::select("year", "organismID", "verbatimLocality", "scientificName") |>
                     dplyr::distinct(),
                   by = c("year", "organismID")) |>
  dplyr::arrange(year, organismID)

test4 |>
  dplyr::left_join(q7 |> dplyr::select("year", "organismID", "bb_date"),
                   by = c("year", "organismID")) |>
  dplyr::filter(bb_date != bud_burst_date) |>
  View()


waldo::compare(test4 |>
                 dplyr::arrange(year, organismID),
               q7 |> dplyr::select("year", "organismID", "bud_burst_date" = "bb_date", "verbatimLocality", "scientificName") |> dplyr::arrange(year, organismID))

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
                   MaxmeasurementValue = newmeasurementValue[MaxDOY == MaxofMaxDOY]) %>%
  dplyr::ungroup()

# 5. Interpolation ####

## connect tables with total scores above and below criterion
d_bb_int <- dplyr::inner_join(min_d_bb, q4, by = c("organismID", "year"))

## interpolate the bud burst DOY for all trees that have never been measured at criterion value
q5 <-
  d_bb_int %>%
  dplyr::select("year", "organismID", "MaxofMaxDOY", "MaxmeasurementValue", "min_DOY_above_criterion", "min_value") %>%
  dplyr::mutate(Diff_Date = min_DOY_above_criterion  - MaxofMaxDOY,
                Diff_Score = min_value - MaxmeasurementValue,
                S_per_D = Diff_Score/Diff_Date,
                dtrc = (crit_measValue - MaxmeasurementValue)/S_per_D,
                int_Date = MaxofMaxDOY + dtrc,
                int_Score = MaxmeasurementValue + (dtrc * S_per_D)) %>%
  dplyr::filter(min_value > crit_measValue)

# 6. Create final table ####
q6 <- min_d_bb1 %>%
  dplyr::select("year", "organismID", "verbatimLocality", "min_DOY", "measurementValue", "scientificName") %>%
  dplyr::left_join(q5 %>%
                     dplyr::select(organismID, year, int_Date, int_Score),
                   by = c("year", "organismID"))

## calculate the average bud burst date per tree within a year & create final table
q7 <- q6 %>%
  dplyr::mutate(bb_date_doy = dplyr::if_else(measurementValue == crit_measValue, min_DOY, int_Date)) %>%
  dplyr::group_by(year, verbatimLocality, organismID) %>%
  dplyr::mutate(n_Tree = n(),
                bb_date = as.Date(round(bb_date_doy) - 1, origin = paste0(year, "-01-01"))) %>% # rounding up or down here?
  dplyr::ungroup() %>%
  dplyr::select("year", "verbatimLocality", "scientificName", "organismID", "bb_date_doy", "bb_date", "n_Tree")

## save as CSV
write.csv(q7, here::here("data", "annual_budburst_per_tree.csv"), row.names = FALSE)
