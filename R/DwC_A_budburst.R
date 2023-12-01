### DwC-A Bud burst NIOO ###
# Date: 30/11/2023

# Part I: Retrieve data ---------------------------------------------------

# load packages
library(dplyr)
library(lubridate)
library(tidyverse)
library(taxize)

# connect to NIOO AnE SQL server to retrieve data
con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = {'SQL Server'},
                      UID = rstudioapi::askForPassword('Uid'),
                      Pwd = rstudioapi::askForPassword('Pwd'),
                      Server = 'ws.ane.nioo.int')

# get raw data

## tbl_Area
d_Area <- tbl(con, dbplyr::in_catalog("AnE_Budburst", "dbo", "tbl_Area")) %>%
  collect() 

## tbl_Budburst
d_bb <- tbl(con, dbplyr::in_catalog("AnE_Budburst", "dbo", "tbl_Budburst")) %>%
  collect()

## tbl_Tree
d_tree <- tbl(con, dbplyr::in_catalog("AnE_Budburst", "dbo", "tbl_Tree")) %>%
  collect()

# rename longitude and latitude because names are incorrect in database (temporary)
d_tree <- d_tree %>% 
  rename(Longitude = 'Latitude',
         Latitude = 'Longitude')

## tbl_TreeSpecies
d_tsp <- tbl(con, dbplyr::in_catalog("AnE_Budburst", "dbo", "tbl_TreeSpecies")) %>%
  collect()

# new coordinates for HV trees (as long as they are not in the database yet - temporary)
dco <- readxl::read_excel('data/new_coordinates_HV_alltrees.xlsx')

# Part II: create event file (= core of DwC-Archive) ----------------------

## 1. Create help file to create event IDs ####

# assign area names and abbreviations to sampled trees
d_tree$AreaName <- d_Area$AreaName [match(d_tree$AreaID, d_Area$AreaID)]
d_tree$AreaAbr <- d_Area$AreaShortName [match(d_tree$AreaID, d_Area$AreaID)]
d_bb$Area <- d_tree$AreaName [match(d_bb$TreeID, d_tree$TreeID)]

# rename Hoge Veluwe to avoid problems with space between words
d_bb$Area [d_bb$Area == 'Hoge Veluwe'] <- 'Hoge_Veluwe'
d_bb$AreaAbr <- d_tree$AreaAbr[match(d_bb$TreeID, d_tree$TreeID)]

# add new coordinates for the trees (temporary)
d_tree <- 
  d_tree %>%
  left_join(., dco, by = c('AreaID', 'SiteNumber', 'TreeNumber')) %>%
  mutate(Longitude = ifelse(AreaID == 11, Longitude.y, Longitude.x),
         Latitude = ifelse(AreaID == 11, Latitude.y, Latitude.x))

# show all areas that have been sampled
unique(d_bb$Area)

# create eventDate and eventID of level 1 (area abbreviation + Year) 
# and level 2 (level 1 ID + day of year)
h1 <- 
  d_bb %>% 
  select(Year, Month, Day, BudburstID, Area, AreaAbr, TreeID) %>%
  mutate('eventDate' = make_date(year = Year, month = Month, day = Day),
         'DOY' = yday(eventDate),
         'eventID_L1' = paste(AreaAbr, Year, sep = ''),
         'eventID_L2' = paste(eventID_L1, DOY, sep = '_')) 


# create eventID for level 3 events (level 2 ID + TreeID)
h1 <- 
  h1 %>%
  mutate('eventID_L3' = paste(eventID_L2, TreeID, sep = '_'))


## 2. Create event file for Level 1: Years ####

# get all areas sampled within each year
vL_y <-
  d_bb %>% 
  group_by(Year) %>%
  distinct(., Area, .keep_all = T) %>%
  summarise(location = paste(Area, collapse = ", "))

# create event file
d_ev1 <- 
  h1 %>%
  select(eventID_L1, Year) %>%
  distinct(., eventID_L1, .keep_all = T) %>%
  mutate('eventDate' = as.character(Year),
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
  rename(eventID = 'eventID_L1',
         year = 'Year') 

## 3. Create event file for level 2: Sampling day within each year ####

# get all the areas that have been sampled on a specific day (in each year)
vL_d <-
  h1 %>% 
  group_by(eventDate) %>%
  distinct(., Area, .keep_all = T) %>%
  summarise(location = paste(Area, collapse = ", "))

# create event file
d_ev2 <-
  h1 %>%
  select(eventID_L2, eventID_L1,  eventDate, Year, Month, Day) %>%
  distinct(., eventID_L2, .keep_all = T) %>%
  mutate('samplingProtocol' = NA,
         'sampleSizeValue' = NA,
         'sampleSizeUnit' = NA,
         'decimalLatitude' = NA,
         'decimalLongitude' = NA,
         'geodeticDatum' = NA,
         'minimumElevationInMeters' = NA,
         'maximumElevationInMeters' = NA,
         'verbatimLocality' = vL_d$location[match(.$eventDate, vL_d$eventDate)]) %>%
  rename(eventID = 'eventID_L2',
         parentEventID = 'eventID_L1',
         year = 'Year',
         month = 'Month',
         day = 'Day') 

# convert dates to characters to avoid merging problems later on
d_ev2$eventDate <- as.character(d_ev2$eventDate)


## 4. Create event file for level 3: bud burst observation on each tree within each year ####

# connect Tree table to bud burst table and h1
d_bb$eventID <- h1$eventID_L3[match(d_bb$BudburstID, h1$BudburstID)]
d_tree$eventID <- d_bb$eventID[match(d_tree$TreeID, d_bb$TreeID)] 

# create event file
d_ev3 <-
  h1 %>%
  select(eventID_L3, eventID_L2, eventDate, Year, Month, Day, TreeID) %>%
  mutate('samplingProtocol' = 'Warmer springs disrupt the synchrony of oak and winter moth phenology, https://doi.org/10.1098/rspb.2000.1363',
         'sampleSizeValue' = 1,
         'sampleSizeUnit' = 'tree',
         'decimalLatitude' = d_tree$Latitude[match(.$TreeID, d_tree$TreeID)],
         'decimalLongitude' = d_tree$Longitude[match(.$TreeID, d_tree$TreeID)],
         'minimumElevationInMeters' = d_tree$Elevation[match(.$TreeID, d_tree$TreeID)],
         'maximumElevationInMeters' = minimumElevationInMeters,
         'verbatimLocality' = d_bb$Area[match(.$eventID_L3, d_bb$eventID)]) %>%
  rename(eventID = 'eventID_L3',
         parentEventID = 'eventID_L2',
         year = 'Year',
         month = 'Month',
         day = 'Day') %>%
  select(!TreeID)

# add geodatic Datum only for events where coordinates are given
d_ev3 <- 
  d_ev3 %>%
  drop_na(decimalLatitude) %>% 
  mutate('geodeticDatum' = 'EPSG:4326') %>% 
  select(eventID, decimalLatitude, geodeticDatum) %>% 
  right_join(.,d_ev3, by = c('eventID', 'decimalLatitude'), relationship = 'many-to-many')

# convert dates to characters to avoid merging problems later on
d_ev3$eventDate <- as.character(d_ev3$eventDate)

# combine all three event files into the final event-core file
event <- 
  rbind(d_ev1, d_ev2, d_ev3) %>%
  arrange(eventDate) 

# add DwC columns that apply to all event levels
event <- 
  event %>%
  mutate('language' = 'en',
         'country' = 'Netherlands',
         'countryCode' = 'NL',
         'institutionID' = 'https://ror.org/01g25jp36',
         'institutionCode' = 'NIOO',
         'type' = 'Event')

# reorder event file according to GBIF list
event <-
  event %>% 
  select(eventID, parentEventID, samplingProtocol, sampleSizeValue, 
         sampleSizeUnit, eventDate, year, month, day, country, 
         countryCode, verbatimLocality, minimumElevationInMeters, 
         maximumElevationInMeters, decimalLatitude, decimalLongitude, 
         geodeticDatum, type, language, institutionID, institutionCode) 

# rename 'Hoge Veluwe' back to original name
event$verbatimLocality <- sub('Hoge_Veluwe', 'Hoge Veluwe', event$verbatimLocality)

# save file as text file
write.table(event, file = "event.txt", sep = "\t", row.names = F)


# Part III. Create occurrence table ---------------------------------------

# merge tables to assign tree species to each measurement
t_sp <- 
  d_tree %>% 
  select(TreeID, TreeSpeciesID, Remarks) %>% 
  left_join(., d_tsp, by = 'TreeSpeciesID') %>%
  right_join(., d_bb, by = 'TreeID')


## 1. Get the taxonomic information of all species ####

# show all sampled species
unique(t_sp$TreeSpeciesName)

# First species: European oak = Quercus robur
get_gbifid(sci = 'Quercus robur')

# select first result of query to proceed
1

# use resulting ID to query the taxonomic classification
QRo <- rbind(classification(2878688, db = "gbif"))  

# store desired taxonomic information
tax <- 
  QRo %>% 
  select(rank, name) %>% 
  rename('European oak' = name)

# repeat for all tree species:
# Second species: American oak = Quercus rubra
get_gbifid(sci = 'Quercus rubra')
1
QRu <- rbind(classification(2880539, db = "gbif")) 
tax <- 
  QRu %>% 
  select(rank, name) %>% 
  rename('American oak' = name) %>%
  right_join(., tax, by = 'rank')

# Third species: Silver birch = Betula pendula
get_gbifid(sci = 'Betula pendula')
Bep <- rbind(classification(5331916, db = "gbif")) 
tax <-
  Bep %>% 
  select(rank, name) %>% 
  rename('Birch' = name) %>%
  right_join(., tax, by = 'rank')

# Forth species: Japanese larch = Larix kaempferi
get_gbifid(sci = 'Larix kaempferi')
1
Lak <- rbind(classification(2686157, db = "gbif")) 
tax <- 
  Lak %>% 
  select(rank, name) %>% 
  rename('Larch' = name) %>%
  right_join(., tax, by = 'rank')

# Fifth species: Scots pine = Pinus sylvestris
get_gbifid(sci = 'Pinus sylvestris')
1
Psy <- rbind(classification(5285637, db = "gbif")) 
tax <- 
  Psy %>% 
  select(rank, name) %>% 
  rename('Pine' = name) %>%
  right_join(., tax, by = 'rank')

# Sixth 'species': Unknown trees = vascular plants = Tracheophyta
get_gbifid(sci = 'Tracheophyta')
Tra <- rbind(classification(7707728, db = "gbif")) 
tax <- 
  Tra %>% 
  select(rank, name) %>% 
  rename('Unknown' = name) %>%
  right_join(., tax, by = 'rank')

# transpose data and assign row/column names
tax_all <- tax %>% t() %>% as.data.frame()
tax_all <- rownames_to_column(tax_all)
colnames(tax_all) <- tax_all[1,]
tax_all <- tax_all[-1,]

# extract the specific epithet of the species name and store in new column
tax_all <- 
  tax_all %>% 
  separate(species, c("Genus", "specificEpithet"), remove = F) %>% 
  select(!Genus)

# replace species with scientific name + author information according to GBIF taxonomy 
# Note: this information is manually gathered so far (How to change that?)
tax_all <-
  tax_all %>%
  mutate(scientificName = recode(species, 
                                 'Pinus sylvestris' = 'Pinus sylvestris L.',
                                 'Larix kaempferi' = 'Larix kaempferi (Lamb.) Carr.',
                                 'Betula pendula' = 'Betula pendula Roth',
                                 'Quercus rubra' = 'Quercus rubra L.',
                                 'Quercus robur' = 'Quercus robur L.',
                                 .missing = 'Tracheophyta')) %>%
  select(!species)

# bind taxonomic information to each observation
t_sp_1 <- left_join(t_sp, tax_all, by = c("TreeSpeciesName" = "rank"))


## 2. Create occurrence IDs ####

# check whether there is any occasion in which more than one tree was sampled at a sampling event
#(should not be the case here as we know that one measurement is only one tree at a time)
d_bb %>% count(eventID) %>% filter(n > 1)

# create occurenceID by extending eventID with '_1' 
occID <- 
  d_ev3 %>% 
  arrange(eventDate) %>%
  mutate('occurrenceID' = paste(eventID, 1, sep = '_')) 

# create occurrence file
occurrence <- 
  t_sp_1 %>%
  select(eventID, ObserverName, kingdom, phylum, class, order, family, genus, specificEpithet, scientificName, TreeID) %>%
  mutate('individualCount' = 1,
         'basisOfRecord' = 'HumanObservation',
         'occurrenceStatus' = 'present',
         'occurrenceRemarks' = NA,
         'occurrenceID' = occID$occurrenceID[match(.$eventID, occID$eventID)]) %>%
  rename('recordedBy' = 'ObserverName',
         'organismID' = 'TreeID') %>%
  select(eventID, occurrenceID, recordedBy, individualCount, occurrenceStatus,
         occurrenceRemarks, organismID, scientificName, kingdom, phylum, class, order, 
         family, genus, specificEpithet)

# save file as text file
write.table(occurrence, file = "occurrence.txt", sep = "\t", row.names = F)


# Part IV: Create Measurement or fact file --------------------------------

## 1. Create measurement or fact file ####
MOF <- 
  t_sp_1 %>% 
  pivot_longer(col = c('TreeTopScore', 'TreeAllScore'),
               names_to = "measurementType",
               values_to = 'measurementValue')  %>%
  select(eventID, measurementType, measurementValue)%>%
  mutate('measurementUnit' = NA,
         'measurementMethod' = 'https://doi.org/10.1098/rspb.2000.1363',
         'measurementRemarks' = NA)  

## 2. Create measurementID ####

# number the measurements per occurrence
MOF <-
  MOF %>%
  group_by(eventID) %>%
  mutate('ID' = c(1:n()))

# add occurrenceID
MOF$occurrenceID <- occurrence$occurrenceID[match(MOF$eventID, occurrence$eventID)]

# create measurementID by extending occurrenceID by number of measurement         
MOF <- 
  MOF %>% 
  mutate('measurementID' = paste(occurrenceID, ID, sep = '_')) %>%
  select(!c(ID, occurrenceID))

# rename measurement types to fit more controlled vocabulary
MOF$measurementType[MOF$measurementType == 'TreeTopScore'] <- 'bud burst stage (PO:0025532) of the tree crown'
MOF$measurementType[MOF$measurementType == 'TreeAllScore'] <- 'bud burst stage (PO:0025532) of the whole tree' 

  
# reorder columns according to GBIF list
eMOF <-
  MOF %>%
  select(measurementID, eventID, measurementType, measurementValue, 
         measurementUnit, measurementMethod, measurementRemarks)

# save file as text file
write.table(eMOF, file = "extendedmeasurementorfact.txt", sep = "\t", row.names = F)


# Part V: Create DwC-A as zip folder -----------------------------------------

# requires the creation of the meta.xml and EML.xml files first 

library(zip)

files = c('event.txt', 'occurrence.txt', 'extendedmeasurementorfact.txt' , "meta.xml", 'EML.xml')
zip("DwC-A_budburst.zip", files, root = ".", mode = "mirror")

zip_list("DwC-A_budburst.zip")
