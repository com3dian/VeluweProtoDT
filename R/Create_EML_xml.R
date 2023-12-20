# Pipeline for creation of EML metadata file of the DwC-A of bud burst data ####

# Author: Cherine Jantzen
# Created: 04/12/2023


# load packages
library(emld)
library(xml2)
library(EML)
library(ids)

# 1. Fill in meta data ----------------------------------------------------

# title of the data set
title <- "Bud burst data of department of Animal Ecology/NIOO-KNAW"

# information on the creator of the data set
creator <- list(organizationName = "Department of Animal Ecology - Netherlands institute of ecology (NIOO-KNAW)",
                positionName = "Databank manager",
                address = list(country = "NL", 
                               city = "Wageningen"),
                electronicMailAddress = "AnE_Database@nioo.knaw.nl",
                onlineUrl = "https://nioo.knaw.nl/en/facilities/hole-breeding-passerines-monitoring-hoge-veluwe ")

# information on the provider of the meta data
metadataProvider <- list(individualName = list(givenName = "Cherine", surName = "Jantzen"),
                         organizationName = "Netherlands institute of ecology (NIOO-KNAW)",
                         address = list(country = "NL", city = "Wageningen"),
                         electronicMailAddress = "C.Jantzen@nioo.knaw.nl")

# information on the contact person
contact_person <- list(organizationName = "Department of Animal Ecology - Netherlands institute of ecology (NIOO-KNAW)",
                       address = list(country = "NL", city = "Wageningen"),
                       positionName = "Databank manager",
                       electronicMailAddress = "AnE_Database@nioo.knaw.nl")

# date of publication of the data set
publication_date <- "2024-12-15"

# language of the data
language <- "en"

# abstract describing the data set
abstract <- list(para = "The stage of bud burst has been measured from 1988 onwards on several tree species in different areas across the Netherlands to record the phenology of the trees.")

# List of keywords and the thesaurus they are listed in 
keywords <- list(list(keyword = list("bud burst", "trees", "ecology", "plant phenology"), 
                      keywordThesaurus = "envThes"),
                 list(keyword = "oak", 
                      keywordThesaurus = "GEMET"))

# License for the work 
licensed <- list(licenseName = "CC BY 4.0 DEED Attribution 4.0 International",
                 url = "https://creativecommons.org/licenses/by/4.0/")

# geographic coverage of the data
geographic_coverage <- list(geographicDescription = "Several sites across the Netherlands have been sampled: The Nationalpark Hoge Veluwe, Oosterhout, Warnsborn, Doorwerth, Bennekom, Buunderkamp, Wolfheze, Rhene, Heveadorp, Goffert, Kernhem, Loenen",
                            boundingCoordinates = list(westBoundingCoordinate = "5.574453", 
                                                       eastBoundingCoordinate = "6.019378",
                                                       northBoundingCoordinate = "52.116720", 
                                                       southBoundingCoordinate = "51.821770"))

# temporal coverage of the data
temporal_coverage <- list(rangeOfDates = list(beginDate = list(calendarDate = "1988-04-21"), 
                                              endDate = list(calendarDate = "2023-05-21")))

# taxonomic coverage of the data
taxonomic_coverage <- list(generalTaxonomicCoverage = "Data covers a range of tree species that are either unknown or identified to species level.",
                           taxonomicClassification = list(list(taxonRankName = "Species",
                                                               taxonRankValue = "Quercus robur",
                                                               taxonId = "2878688",
                                                               commonName = "Pedunculate oak"),
                                                          list(taxonRankName = "Species",
                                                               taxonRankValue = "Quercus rubra",
                                                               taxonId = "2880539", 
                                                               commonName = "Northern red oak"),
                                                          list(taxonRankName = "Species",
                                                               taxonRankValue = "Betula pendula",
                                                               taxonId = "5331916", 
                                                               commonName = "Silver birch"),
                                                          list(taxonRankName = "Species",
                                                               taxonRankValue = "Larix kaempferi",
                                                               taxonId = "2686157", 
                                                               commonName = "Japanese larch"),
                                                          list(taxonRankName = "Species",
                                                               taxonRankValue = "Pinus sylvestris",
                                                               taxonId = "5285637", 
                                                               commonName = "Scots pine"),
                                                          list(taxonRankName = "Phylum",
                                                               taxonRankValue = "Tracheophyta",
                                                               taxonId = "7707728", 
                                                               commonName = "Vascular plants")))

# combine all three types of coverage                          
coverage <- list(geographicCoverage = geographic_coverage,
                 temporalCoverage = temporal_coverage,
                 taxonomicCoverage = taxonomic_coverage) 

# maintenance: frequency of updates
maintenance <- list(maintenanceUpdateFrequency = "annually", 
                    description = list(para = "Data is updated after annual data collection."))


# methods for data collection
methods <- list(methodStep = list(list(description = list(para = "Bud burst state is visually assessed twice a week from the beginning of April onwards.")), 
                                  list(description = list(para = "In the years 1989, 1990, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, bud burst was scored on a scale from 0 to 3 with increments of 0.25. In all other years, only increments of 0.5 were allowed. On this scale, 0 means that buds are not yet green, 0.5 means buds are swollen, 1 means buds are green and swollen, 2 means small leaves are sticking out and 3 means that leaves are bended at the leafbase. This score is assigned to both the lower part of the tree and the top of the tree."))))


# 2. Create the EML.xml file ----------------------------------------------

# create a UUID
packageId <- ids::uuid(n = 1, drop_hyphens = F, use_time = F)

# combine all components in one list
eml <- list(dataset = 
              list(title = title,
                   creator = creator,
                   metadataProvider = metadataProvider,
                   pubDate = publication_date,
                   language = language,
                   abstract = abstract,
                   keywordSet = keywords,
                   licensed = licensed,
                   coverage = coverage,
                   maintenance = maintenance,
                   contact = contact_person,
                   methods = methods),
            system = "uuid",
            packageId = packageId)

# write EMl file
EML::write_eml(eml, file = "eml.xml")


# 3. Add attributes for specific nodes ------------------------------------

# read EML file as XML file
EML <- xml2::read_xml("EML.xml")

# identify all taxonId nodes for which attribute shall be set
taxonId_node <- xml2::xml_find_all(EML, xpath = "//taxonId")

# set "provider" attribute for taxonId nodes
xml2::xml_set_attr(taxonId_node, attr = "provider", value = "https://www.gbif.org/")

# identify title node
title_node <- xml2::xml_find_all(EML, xpath = "//title")

# set title attribute
xml2::xml_set_attr(title_node, attr = "xml:lang", value = "en")


# 4. Validate EML file ----------------------------------------------------
emld::eml_validate(EML)

# write final EML file
xml2::write_xml(EML, file = "eml.xml")
