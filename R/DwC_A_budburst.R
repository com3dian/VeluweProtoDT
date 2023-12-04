### Pipeline: Darwin Core Archive for bud burst data of Animal Ecology/NIOO-KNAW ###
# Author: Cherine Jantzen
# Date: 04/12/2023

# Part I: Retrieve data ---------------------------------------------------

# load packages
library(lubridate)
library(tidyverse)
library(taxize)
library(DBI)
library(odbc)
library(rstudioapi)
library(zip)
library(readxl)

# connect to NIOO AnE SQL server to retrieve data
con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = {'SQL Server'},
                      UID = rstudioapi::askForPassword('Uid'),
                      Pwd = rstudioapi::askForPassword('Pwd'),
                      Server = 'ws.ane.nioo.int')

# get input data

## tbl_Area
d_Area <- dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Area")) %>%
  dplyr::collect() 

## tbl_Budburst
d_bb <- dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Budburst")) %>%
  dplyr::collect()

## tbl_Tree
d_tree <- dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Tree")) %>%
  dplyr::collect()

# rename longitude and latitude because names are incorrect in database (temporary)
d_tree <- d_tree %>% 
  dplyr::rename(Longitude_old = 'Latitude',
                Latitude_old = 'Longitude')

## tbl_TreeSpecies
d_tsp <- dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_TreeSpecies")) %>%
  dplyr::collect()

# new coordinates for HV trees (as long as they are not in the database yet - temporary)
dco <- readxl::read_excel('data/new_coordinates_HV_alltrees.xlsx') %>%
  dplyr::rename(Longitude_new = 'Longitude',
                Latitude_new = 'Latitude') 

# Part II: create event file (= core of DwC-Archive) ----------------------

## 1. Create help file to create event IDs ####

# assign area names and abbreviations to sampled trees
d_tree <- d_tree %>%
  dplyr::left_join(d_Area %>% 
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

# add new coordinates for the trees (temporary)
d_tree <- 
  d_tree %>%
  dplyr::left_join(dco, by = c('AreaID', 'SiteNumber', 'TreeNumber')) %>%
  dplyr::mutate(Longitude = dplyr::if_else(AreaID == 11, Longitude_new, Longitude_old),
                Latitude = dplyr::if_else(AreaID == 11, Latitude_new, Latitude_old))

# create eventDate and eventID of level 1 (area abbreviation + Year) 
# and level 2 (level 1 ID + day of year)
# and level 3 (level 2 ID + TreeID)
h1 <- 
  d_bb %>% 
  dplyr::select(Year, Month, Day, BudburstID, Area, AreaAbr, TreeID) %>%
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
  dplyr::distinct(., Area, .keep_all = T) %>%
  dplyr::summarise(location = paste(Area, collapse = ", "))

# create event file
d_ev1 <- 
  h1 %>%
  dplyr::select(eventID_L1, Year) %>%
  dplyr::distinct(., eventID_L1, .keep_all = T) %>%
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
  dplyr::rename(eventID = 'eventID_L1',
                year = 'Year') 

## 3. Create event file for level 2: Sampling day within each year ####

# get all the areas that have been sampled on a specific day (in each year)
vL_d <-
  h1 %>% 
  dplyr::group_by(eventDate) %>%
  dplyr::distinct(., Area, .keep_all = T) %>%
  dplyr::summarise(location = paste(Area, collapse = ", "))

# create event file
d_ev2 <-
  h1 %>%
  dplyr::select(eventID_L2, eventID_L1,  eventDate, Year, Month, Day) %>%
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
                day = 'Day') 

# convert dates to characters to avoid merging problems later on
d_ev2$eventDate <- as.character(d_ev2$eventDate)


## 4. Create event file for level 3: bud burst observation on each tree within each year ####

# connect Tree table to bud burst table and h1
d_bb <- d_bb %>% 
  dplyr::left_join(h1 %>% 
                     dplyr::select('eventID' = 'eventID_L3', 'BudburstID'), 
                   by = 'BudburstID')

# d_tree <- d_tree %>% dplyr::left_join(d_bb %>% 
#                                         dplyr::select('eventID', 'TreeID'),
#                                       by = 'TreeID')

# create event file
d_ev3 <-
  h1 %>%
  dplyr::select(eventID_L3, eventID_L2, eventDate, Year, Month, Day, TreeID) %>%
  dplyr::mutate('samplingProtocol' = 'Warmer springs disrupt the synchrony of oak and winter moth phenology, https://doi.org/10.1098/rspb.2000.1363',
                'sampleSizeValue' = 1,
                'sampleSizeUnit' = 'tree',
                'decimalLatitude' = d_tree$Latitude[match(.$TreeID, d_tree$TreeID)],
                'decimalLongitude' = d_tree$Longitude[match(.$TreeID, d_tree$TreeID)],
                'minimumElevationInMeters' = d_tree$Elevation[match(.$TreeID, d_tree$TreeID)],
                'maximumElevationInMeters' = minimumElevationInMeters,
                'verbatimLocality' = d_bb$Area[match(.$eventID_L3, d_bb$eventID)]) %>%
  dplyr::rename(eventID = 'eventID_L3',
                parentEventID = 'eventID_L2',
                year = 'Year',
                month = 'Month',
                day = 'Day') %>%
  dplyr::select(!TreeID)

# add geodatic Datum only for events where coordinates are given
d_ev3 <- 
  d_ev3 %>%
  tidyr::drop_na(decimalLatitude) %>% 
  dplyr::mutate('geodeticDatum' = 'EPSG:4326') %>% 
  dplyr::select(eventID, decimalLatitude, geodeticDatum) %>% 
  dplyr::right_join(.,d_ev3, by = c('eventID', 'decimalLatitude'), relationship = 'many-to-many')

# convert dates to characters to avoid merging problems later on
d_ev3$eventDate <- as.character(d_ev3$eventDate)

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
                'type' = 'Event')

# reorder event file according to GBIF list
event <-
  event %>% 
  dplyr::select(eventID, parentEventID, samplingProtocol, sampleSizeValue, 
                sampleSizeUnit, eventDate, year, month, day, country, 
                countryCode, verbatimLocality, minimumElevationInMeters, 
                maximumElevationInMeters, decimalLatitude, decimalLongitude, 
                geodeticDatum, type, language, institutionID, institutionCode) 

# rename 'Hoge Veluwe' back to original name
event <- event %>%
  dplyr::mutate(verbatimLocality = stringr::str_replace(string = verbatimLocality, pattern = "_", replacement = " "))

# save file as text file
write.table(event, file = "event.txt", sep = "\t", row.names = F)


# Part III. Create occurrence table ---------------------------------------

# merge tables to assign tree species to each measurement
t_sp <- 
  d_tree %>% 
  dplyr::select(TreeID, TreeSpeciesID, Remarks) %>% 
  dplyr::left_join(d_tsp, by = 'TreeSpeciesID') %>%
  dplyr::right_join(., d_bb, by = 'TreeID')


## 1. Get the taxonomic information of all species ####

# add scientific names to tree table
t_sp <- t_sp %>% mutate(species = dplyr::if_else(TreeSpeciesName  == 'European oak', 'Quercus robur',
                                                 dplyr::if_else(TreeSpeciesName == 'American oak', 'Quercus rubra',
                                                                dplyr::if_else(TreeSpeciesName == 'Larch', 'Larix kaempferi',
                                                                               dplyr::if_else(TreeSpeciesName == 'Pine', 'Pinus sylvestris',
                                                                                              dplyr::if_else(TreeSpeciesName == 'Birch', 'Betula pendula', 'Tracheophyta' ))))))

# get all scientific Names to query the taxonomic information in the next step
sciNames <- unique(t_sp$species)

# for all species
tax <- taxize::get_gbifid_(sci = sciNames) %>% 
  dplyr::bind_rows() %>%
  dplyr::filter(status == "ACCEPTED" & matchtype == 'EXACT') %>%
  tidyr::separate(canonicalname, c('Genus', 'specificEpithet'), remove = F) %>%
  dplyr::select('scientificName' = 'scientificname', canonicalname, kingdom, phylum, class, order, family, genus, specificEpithet)


# bind taxonomic information to each observation
t_sp_1 <- left_join(t_sp, tax, by = c("species" = "canonicalname"))


## 2. Create occurrence IDs ####

# check whether there is any occasion in which more than one tree was sampled at a sampling event
#(should not be the case here as we know that one measurement is only one tree at a time)
d_bb %>% count(eventID) %>% filter(n > 1)

# create occurenceID by extending eventID with '_1' 
occID <- 
  d_ev3 %>% 
  dplyr::arrange(eventDate) %>%
  dplyr::mutate('occurrenceID' = paste(eventID, 1, sep = '_')) 

# create occurrence file
occurrence <- 
  t_sp_1 %>%
  dplyr::select(eventID, ObserverName, kingdom, phylum, class, order, family, genus, specificEpithet, scientificName, TreeID) %>%
  dplyr::mutate('individualCount' = 1,
                'basisOfRecord' = 'HumanObservation',
                'occurrenceStatus' = 'present',
                'occurrenceRemarks' = NA,
                'occurrenceID' = occID$occurrenceID[match(.$eventID, occID$eventID)]) %>%
  dplyr::rename('recordedBy' = 'ObserverName',
                'organismID' = 'TreeID') %>%
  dplyr::select(eventID, occurrenceID, recordedBy, individualCount, occurrenceStatus,
                occurrenceRemarks, organismID, scientificName, kingdom, phylum, class, order, 
                family, genus, specificEpithet)

# save file as text file
write.table(occurrence, file = "occurrence.txt", sep = "\t", row.names = F)


# Part IV: Create Measurement or fact file --------------------------------

## 1. Create measurement or fact file ####
MOF <- 
  t_sp_1 %>% 
  tidyr::pivot_longer(col = c('TreeTopScore', 'TreeAllScore'),
                      names_to = "measurementType",
                      values_to = 'measurementValue')  %>%
  dplyr::select(eventID, measurementType, measurementValue)%>%
  dplyr::mutate('measurementUnit' = NA,
                'measurementMethod' = 'https://doi.org/10.1098/rspb.2000.1363',
                'measurementRemarks' = NA)  

## 2. Create measurementID ####

# number the measurements per occurrence
MOF <-
  MOF %>%
  dplyr::group_by(eventID) %>%
  dplyr::mutate('ID' = c(1:n()))

# add occurrenceID & create measurementID by extending occurrenceID by number of measurement 
MOF <- MOF %>% 
  dplyr::left_join(occurrence %>% 
                     dplyr::select(occurrenceID, eventID), 
                   by = 'eventID') %>%
  dplyr::mutate('measurementID' = paste(occurrenceID, ID, sep = '_')) %>%
  dplyr::select(!c(ID, occurrenceID))

# rename measurement types to fit more controlled vocabulary
MOF$measurementType[MOF$measurementType == 'TreeTopScore'] <- 'bud burst stage (PO:0025532) of the tree crown'
MOF$measurementType[MOF$measurementType == 'TreeAllScore'] <- 'bud burst stage (PO:0025532) of the whole tree' 
  
# reorder columns according to GBIF list
eMOF <-
  MOF %>%
  dplyr::select(measurementID, eventID, measurementType, measurementValue, 
                measurementUnit, measurementMethod, measurementRemarks)

# save file as text file
write.table(eMOF, file = "extendedmeasurementorfact.txt", sep = "\t", row.names = F)


# Part V: Create DwC-A as zip folder -----------------------------------------

# requires the creation of the meta.xml and EML.xml files first 

files = c('event.txt', 'occurrence.txt', 'extendedmeasurementorfact.txt' , "meta.xml", 'EML.xml')
zip::zip("DwC-A_budburst.zip", files, root = ".", mode = "mirror")

zip::zip_list("DwC-A_budburst.zip")
