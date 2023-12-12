### Pipeline: Bud burst data of Animal Ecology/NIOO-KNAW to Darwin Core ###

# Author: Cherine Jantzen
# Created: 30/11/2023
# Last updated: 12/12/2023

# Part I: Retrieve data ---------------------------------------------------

# load packages
library(lubridate)
library(tidyverse)
library(taxize)

# get input data
# TODO get data through dataverse API
# d_tree = tbl_Tree.csv
# d_area = tbl_Area.csv
# d_bb = tbl_budburst.csv
# d_tsp = tbl_TreeSpecies.csv

# Part II: create event file (= core of DwC-Archive) ----------------------

## 1. Create help file to create event IDs ####

# assign area names and abbreviations to sampled trees
d_tree <- d_tree %>%
  dplyr::left_join(d_area %>%
                     dplyr::select('AreaID', 'AreaName', 'AreaAbr' = 'AreaShortName'),
                   by = 'AreaID')

# assigns area names to bud burst data
d_bb <- d_bb %>%
  dplyr::left_join(d_tree %>%
                     dplyr::select('Area' = 'AreaName', 'TreeID', 'AreaAbr'),
                   by = 'TreeID')

# rename Hoge Veluwe to avoid problems with space between words
d_bb <- d_bb %>%
  dplyr::mutate(Area = stringr::str_replace(string = Area, pattern = " ", replacement = "_"))

# create eventDate and (hierarchical) eventID of:
# level 1 (<area abbreviation><year>; e.g., HV1988)
# level 2 (<level 1 ID>_<day of year>; e.g., HV1988_119)
# level 3 (<level 2 ID>_<TreeID>; e.g., HV1988_119_412)
h1 <-
  d_bb %>%
  dplyr::select('Year', 'Month', 'Day', 'BudburstID', 'Area', 'AreaAbr', 'TreeID') %>%
  dplyr::mutate('eventDate' = lubridate::make_date(year = Year, month = Month, day = Day),
                'DOY' = lubridate::yday(eventDate),
                'eventID_L1' = paste(AreaAbr, Year, sep = ''),
                'eventID_L2' = paste(eventID_L1, DOY, sep = '_'),
                'eventID_L3' = paste(eventID_L2, TreeID, sep = '_'))

## 2. Create event file for Level 1: Years ####

# get all areas sampled within each year
vL_y <-
  d_bb %>%
  dplyr::group_by(Year) %>%
  dplyr::distinct(., Area, .keep_all = TRUE) %>%
  dplyr::summarise(location = paste(Area, collapse = ", ")) %>%
  dplyr::ungroup()

# create event file
d_ev1 <-
  h1 %>%
  dplyr::select('eventID_L1', 'Year') %>%
  dplyr::distinct(., eventID_L1, .keep_all = TRUE) %>%
  dplyr::mutate('eventDate' = as.character(Year),
                'month' = NA,
                'day' = NA,
                'samplingProtocol' = NA,
                'sampleSizeValue' = NA,
                'sampleSizeUnit' = NA,
                'parentEventID' = NA,
                'decimalLatitude' = NA,
                'decimalLongitude' = NA,
                'geodeticDatum' = NA,
                'minimumElevationInMeters' = NA,
                'maximumElevationInMeters' = NA,
                'verbatimLocality' = vL_y$location[match(.$Year, vL_y$Year)]) %>%
  dplyr::rename('eventID' = 'eventID_L1',
                'year' = 'Year')

## 3. Create event file for level 2: Sampling day within each year ####

# get all the areas that have been sampled on a specific day (in each year)
vL_d <-
  h1 %>%
  dplyr::group_by(eventDate) %>%
  dplyr::distinct(., Area, .keep_all = TRUE) %>%
  dplyr::summarise(location = paste(Area, collapse = ", ")) %>%
  dplyr::ungroup()

# create event file
d_ev2 <-
  h1 %>%
  dplyr::select('eventID_L2', 'eventID_L1', 'eventDate', 'Year', 'Month', 'Day') %>%
  dplyr::distinct(., eventID_L2, .keep_all = T) %>%
  dplyr::mutate('samplingProtocol' = NA,
                'sampleSizeValue' = NA,
                'sampleSizeUnit' = NA,
                'decimalLatitude' = NA,
                'decimalLongitude' = NA,
                'geodeticDatum' = NA,
                'minimumElevationInMeters' = NA,
                'maximumElevationInMeters' = NA,
                'verbatimLocality' = vL_d$location[match(.$eventDate, vL_d$eventDate)]) %>%
  dplyr::rename(eventID = 'eventID_L2',
                parentEventID = 'eventID_L1',
                year = 'Year',
                month = 'Month',
                day = 'Day') %>%
  # convert dates to characters to avoid merging problems later on
  dplyr::mutate(eventDate = as.character(eventDate))


## 4. Create event file for level 3: bud burst observation on each tree within each year ####

# connect Tree table to bud burst table and h1
d_bb <- d_bb %>%
  dplyr::left_join(h1 %>%
                     dplyr::select('eventID' = 'eventID_L3', 'BudburstID'),
                   by = 'BudburstID')

# create event file
d_ev3 <-
  h1 %>%
  dplyr::select('eventID_L3', 'eventID_L2', 'eventDate', 'Year', 'Month', 'Day', 'TreeID') %>%
  dplyr::mutate('samplingProtocol' = 'Warmer springs disrupt the synchrony of oak and winter moth phenology, https://doi.org/10.1098/rspb.2000.1363',
                'sampleSizeValue' = 1,
                'sampleSizeUnit' = 'tree',
                'decimalLatitude' = d_tree$Latitude[match(.$TreeID, d_tree$TreeID)],
                'decimalLongitude' = d_tree$Longitude[match(.$TreeID, d_tree$TreeID)],
                'minimumElevationInMeters' = d_tree$Elevation[match(.$TreeID, d_tree$TreeID)],
                'maximumElevationInMeters' = minimumElevationInMeters,
                'verbatimLocality' = d_bb$Area[match(.$eventID_L3, d_bb$eventID)]) %>%
  dplyr::rename('eventID' = 'eventID_L3',
                'parentEventID' = 'eventID_L2',
                'year' = 'Year',
                'month' = 'Month',
                'day' = 'Day') %>%
  dplyr::select(!'TreeID')

# add geodatic Datum only for events where coordinates are given
d_ev3 <-
  d_ev3 %>%
  tidyr::drop_na(decimalLatitude) %>%
  dplyr::mutate('geodeticDatum' = 'EPSG:4326') %>%
  dplyr::select('eventID', 'decimalLatitude', 'geodeticDatum') %>%
  dplyr::right_join(.,d_ev3, by = c('eventID', 'decimalLatitude'), relationship = 'many-to-many') %>%
  # convert dates to characters to avoid merging problems later on
  dplyr::mutate(eventDate = as.character(eventDate))

# combine all three event files into the final event-core file
event <-
  dplyr::bind_rows(d_ev1, d_ev2, d_ev3) %>%
  dplyr::arrange(eventDate)

# add DwC columns that apply to all event levels
event <-
  event %>%
  dplyr::mutate('language' = 'en',
                'country' = 'Netherlands',
                'countryCode' = 'NL',
                'institutionID' = 'https://ror.org/01g25jp36',
                'institutionCode' = 'NIOO',
                'type' = 'Event') %>%
  # reorder event file according to GBIF list
  dplyr::select('eventID', 'parentEventID', 'samplingProtocol', 'sampleSizeValue',
                'sampleSizeUnit', 'eventDate', 'year', 'month', 'day', 'country',
                'countryCode', 'verbatimLocality', 'minimumElevationInMeters',
                'maximumElevationInMeters', 'decimalLatitude', 'decimalLongitude',
                'geodeticDatum', 'type', 'language', 'institutionID', 'institutionCode') %>%
  # rename 'Hoge Veluwe' back to original name
  dplyr::mutate(verbatimLocality = stringr::str_replace(string = verbatimLocality, pattern = "_", replacement = " "))

# save file as text file
write.table(event, file = "data/event.txt", sep = "\t", row.names = FALSE)


# Part III. Create occurrence table ---------------------------------------

# merge tables to assign tree species to each measurement
t_sp <-
  d_tree %>%
  dplyr::select('TreeID', 'TreeSpeciesID', 'Remarks') %>%
  dplyr::left_join(d_tsp, by = 'TreeSpeciesID') %>%
  dplyr::right_join(., d_bb, by = 'TreeID')


## 1. Get the taxonomic information of all species ####

# add scientific names to tree table
t_sp <-
  t_sp %>%
  dplyr::mutate(species = dplyr::case_when(TreeSpeciesName == "European oak" ~ "Quercus robur",
                                           TreeSpeciesName == "American oak" ~ "Quercus rubra",
                                           TreeSpeciesName == "Larch" ~ "Larix kaempferi",
                                           TreeSpeciesName == "Pine" ~ "Pinus sylvestris",
                                           TreeSpeciesName == "Birch" ~ "Betula pendula",
                                           TRUE ~ "Tracheophyta"))

# get all scientific Names to query the taxonomic information in the next step
sciNames <- unique(t_sp$species)

# for all species
tax <- taxize::get_gbifid_(sci = sciNames) %>%
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == 'EXACT') %>%
  tidyr::separate(canonicalname, c('Genus', 'specificEpithet'), remove = FALSE) %>%
  dplyr::select('scientificName' = 'scientificname', 'canonicalname',
                'kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'specificEpithet')


# bind taxonomic information to each observation
t_sp_1 <- dplyr::left_join(t_sp, tax, by = c("species" = "canonicalname"))


## 2. Create occurrence IDs ####

# check whether there is any occasion in which more than one tree was sampled at a sampling event
# (should not be the case here as we know that one measurement is only one tree at a time)
if(d_bb %>% dplyr::count(eventID) %>% dplyr::filter(n > 1) %>% nrow() > 0) {

  stop(paste("In", d_bb %>% dplyr::count(eventID) %>% dplyr::filter(n > 1) %>% nrow(),
             "instances of an event, more than one tree was sampled.",
             "This should not be the case for level-3 events."))

}

# create occurrenceID by extending eventID with '_1'
occID <-
  d_ev3 %>%
  dplyr::arrange(eventDate) %>%
  dplyr::mutate('occurrenceID' = paste(eventID, 1, sep = '_'))

# create occurrence file
occurrence <-
  t_sp_1 %>%
  dplyr::select('eventID', 'ObserverName', 'kingdom', 'phylum', 'class', 'order',
                'family', 'genus', 'specificEpithet', 'scientificName', 'TreeID') %>%
  dplyr::mutate('individualCount' = 1,
                'basisOfRecord' = 'HumanObservation',
                'occurrenceStatus' = 'present',
                'occurrenceRemarks' = NA,
                'occurrenceID' = occID$occurrenceID[match(.$eventID, occID$eventID)]) %>%
  dplyr::rename('recordedBy' = 'ObserverName',
                'organismID' = 'TreeID') %>%
  dplyr::select('eventID', 'occurrenceID', 'recordedBy', 'individualCount', 'basisOfRecord', 'occurrenceStatus',
                'occurrenceRemarks', 'organismID', 'scientificName', 'kingdom', 'phylum', 'class', 'order',
                'family', 'genus', 'specificEpithet')

# save file as text file
write.table(occurrence, file = "data/occurrence.txt", sep = "\t", row.names = FALSE)

# Part IV: Create Measurement or fact file --------------------------------

## 1. Create measurement or fact file ####
MOF <-
  t_sp_1 %>%
  tidyr::pivot_longer(col = c('TreeTopScore', 'TreeAllScore'),
                      names_to = "measurementType",
                      values_to = 'measurementValue')  %>%
  dplyr::select('eventID', 'measurementType', 'measurementValue')%>%
  dplyr::mutate('measurementUnit' = NA,
                'measurementMethod' = 'https://doi.org/10.1098/rspb.2000.1363',
                'measurementRemarks' = NA)

## 2. Create measurementID ####

# number the measurements per occurrence
MOF <-
  MOF %>%
  dplyr::group_by(eventID) %>%
  dplyr::mutate('ID' = 1:dplyr::n()) %>%
  dplyr::ungroup()

# add occurrenceID & create measurementID by extending occurrenceID by number of measurement
MOF <-
  MOF %>%
  dplyr::left_join(occurrence %>%
                     dplyr::select('occurrenceID', 'eventID'),
                   by = 'eventID') %>%
  dplyr::mutate('measurementID' = paste(occurrenceID, ID, sep = '_')) %>%
  dplyr::select(!c('ID', 'occurrenceID')) %>%
  # rename measurement types to fit more controlled vocabulary
  dplyr::mutate(measurementType = dplyr::case_when(measurementType == "TreeTopScore" ~ "bud burst stage (PO:0025532) of the tree crown",
                                                   measurementType == "TreeAllScore" ~ "bud burst stage (PO:0025532) of the whole tree"))

# reorder columns according to GBIF list
eMOF <-
  MOF %>%
  dplyr::select('measurementID', 'eventID', 'measurementType', 'measurementValue',
                'measurementUnit', 'measurementMethod', 'measurementRemarks')

# save file as text file
write.table(eMOF, file = "data/extendedmeasurementorfact.txt", sep = "\t", row.names = FALSE)
