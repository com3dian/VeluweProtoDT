# Pipeline: Data processing of bud burst DwC-A - Calculation of bud burst dates per tree and year ####

# Created: 14/11/2023
# Author: Cherine Jantzen 


# Part I: Preparation  ----------------------------------------------------

# load packages
library(dplyr)
library(lubridate)

# load DwC-A files
event <- read.csv('data/event.txt', sep = '\t')
occ <- read.csv('data/occurrence.txt', sep = '\t')
mof <- read.csv('data/extendedmeasurementorfact.txt', sep = '\t')

# connect measurements with events (and therefore trees)
d_bb <- dplyr::left_join(event, mof, by = 'eventID', relationship = "many-to-many")

# get organism ID for each event
d_bb <- occ %>% 
  dplyr::select('eventID', 'organismID', 'scientificName') %>% 
  dplyr::right_join(d_bb, by = 'eventID', relationship = "many-to-many")


# Part II: Queries --------------------------------------------------------

# 1. Define bud burst criteria ####

## set measurement type criterium: TopScore (= bud burst stage (PO:0025532) of the tree crown)
crit_measType <- 'bud burst stage (PO:0025532) of the tree crown'

## first day where measurement value (of the tree crown) > 1 is bud burst date 
crit_measValue <- 1 


# 2. Trees with measures above bud burst criterium ####

## filter for measurement type criterium
d_bb <- d_bb %>% 
  dplyr::filter(measurementType == crit_measType)

## convert date to day of year for easier calculations
d_bb$DOY <- lubridate::yday(d_bb$eventDate)

## get only entries above measurement value criterium
q1 <- 
  d_bb %>% 
  dplyr::group_by(year, organismID, measurementValue) %>%
  dplyr::slice_min(order_by = DOY) %>%
  dplyr::rename(MinDOY = 'DOY') %>%
  dplyr::filter(measurementValue >= crit_measValue) %>%   
  dplyr::ungroup()

## get the minimum of the minimum date
q2 <- 
  q1 %>% 
  group_by(year, organismID) %>%
  slice_min(order_by = MinDOY) %>%
  rename(MinOfMinDOY = 'MinDOY') %>%
  ungroup()

## Add min of min April Date (of q2) to bud burst data
d_bb_1 <- dplyr::left_join(d_bb, q2 %>% 
                             dplyr::select('year', 'organismID', 'MinOfMinDOY'), 
                           by = c('year', 'organismID'), relationship = 'many-to-many')


# 3. Deal with irregular measures in 1991 (influence of late frost) ####

## filter for 1991, measures below criterium & dates earlier than MinOfMinDOY
q3_1 <- d_bb_1 %>% 
  dplyr::filter(year == 1991 & measurementValue < crit_measValue & DOY < MinOfMinDOY) 

## add 1991 DOY and measurements to bud burst table
d_bb_2 <-
  q3_1 %>% 
  dplyr::select('year', 'organismID', 'DOY', 'measurementValue') %>%
  dplyr::right_join(d_bb, by = c('year', 'organismID', 'DOY', 'measurementValue'), relationship = "many-to-many", keep = TRUE) %>%
  dplyr::rename(DOY_1991 = 'DOY.x',
                measurementValue_1991 = 'measurementValue.x',
                organismID = 'organismID.y',
                year = 'year.y',
                DOY = 'DOY.y',
                measurementValue = 'measurementValue.y') %>%
  dplyr::select(!c('year.x', 'organismID.x'))

## combine real values with corrected values for 1991
d_bb_2 <- d_bb_2 %>% dplyr::mutate(newDOY = dplyr::if_else(year == 1991, DOY_1991, DOY),
                         newmeasurementValue = dplyr::if_else(year == 1991, measurementValue_1991, measurementValue))

# 4. Trees with measures below bud burst criterium ####
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

## connect tables with total scores above and below criterium
d_bb_int <- dplyr::inner_join(q2, q4, by = c('organismID', 'year'))

## interpolate the bud burst DOY for all trees that have never been measured at criterium value
q5 <-
  d_bb_int %>% 
  dplyr::select('year', 'organismID', 'MaxofMaxDOY', 'MaxmeasurementValue', 'MinOfMinDOY', 'measurementValue') %>%
  dplyr::mutate(Diff_Date = MinOfMinDOY - MaxofMaxDOY,
                Diff_Score = measurementValue - MaxmeasurementValue,
                S_per_D = Diff_Score/Diff_Date,
                dtrc = (crit_measValue - MaxmeasurementValue)/S_per_D,
                int_Date = MaxofMaxDOY + dtrc,
                int_Score = MaxmeasurementValue + (dtrc * S_per_D)) %>%
  dplyr::filter(measurementValue > crit_measValue)

# 6. Create final table ####
q6 <- q2 %>% 
  dplyr::select('year', 'organismID', 'verbatimLocality', 'MinOfMinDOY', 'measurementValue', 'scientificName') %>%
  dplyr::left_join(q5 %>% 
                     dplyr::select(organismID, year, int_Date, int_Score),
                   by = c('year', 'organismID')) 

## calculate the average bud burst date per tree within a year & create final table
q7 <- q6 %>% 
  dplyr::mutate(bb_date_doy = dplyr::if_else(measurementValue == crit_measValue, MinOfMinDOY, int_Date)) %>%
  dplyr::group_by(year, verbatimLocality, organismID) %>%
  dplyr::mutate(n_Tree = n(),
                bb_date = as.Date(round(bb_date_doy) - 1, origin = paste0(year, "-01-01"))) %>%
  dplyr::ungroup() %>% 
  dplyr::select('year', 'verbatimLocality', 'scientificName', 'organismID', 'bb_date_doy', 'bb_date', 'n_Tree')

## save as CSV
write.csv(d7, 'annual_budburst_per_tree.csv', row.names = FALSE)
