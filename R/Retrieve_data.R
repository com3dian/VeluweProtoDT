# Retrieve data from data base and prepare data for Dataverse ####

# Authors: Cherine Jantzen, Stefan Vriend
# Created: 12/12/2023
# Last updated: 19/12/2023

# Load packages
library(DBI)
library(here)
library(odbc)
library(rstudioapi)
library(tidyverse)

# Connect to NIOO AnE SQL server to retrieve data
con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = {"ODBC Driver 17 for SQL Server"},
                      UID = rstudioapi::askForPassword("Uid"),
                      Pwd = rstudioapi::askForPassword("Pwd"),
                      Server = rstudioapi::askForPassword("Server"))

# Get input data

## tbl_Area
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Area")) %>%
  dplyr::collect() %>%
  dplyr::select(!c("Remarks", "SysUser", "SysDate")) %>%
  write.csv(here::here("data", "tbl_area.csv"), row.names = FALSE)

## tbl_Budburst
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Budburst")) %>%
  dplyr::collect() %>%
  dplyr::select(!c("Observer", "ObserverName", "Remarks", "SysUser", "SysDate")) %>%
  write.csv(here::here("data", "tbl_budburst.csv"), row.names = FALSE)

## tbl_Tree
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Tree")) %>%
  dplyr::collect() %>%
  dplyr::select(!c("ExtraCode", "Remarks", "SysUser", "SysDate", "Budburst", "Frass", "Wintermoth_Selection")) %>%
  write.csv(here::here("data", "tbl_tree.csv"), row.names = FALSE)

## tbl_TreeSpecies
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_TreeSpecies")) %>%
  dplyr::collect() %>%
  dplyr::select(!c("Remarks", "SysUser", "SysDate")) %>%
  write.csv(here::here("data", "tbl_treeSpecies.csv"), row.names = FALSE)


# Write README
writeLines(text = c("# Bud burst data - Animal Ecology/NIOO-KNAW\n",
                    "### Authors\n",
                    " - Cherine C. Jantzen (https://orcid.org/0009-0006-0723-2682)",
                    " - Stefan J.G. Vriend (https://orcid.org/0000-0002-9006-5988)",
                    " - Marcel E. Visser (http://orcid.org/0000-0002-1456-1939)",
                    "\n",
                    "### Description",
                    "The stage of bud burst has been measured from 1988 onwards on several tree species in different areas across the Netherlands to record the phenology of the trees.",
                    "\n",
                    "### File description",
                    "`tbl_area.csv`\n",
                    "| Variable | Description |  ",
                    "|:----|:----------- |  ",
                    "| AreaID | Unique ID of study area (integer) |  ",
                    "| AreaShortName | Abbreviated name or code of study area (character) |  ",
                    "| AreaName | Full name of study area (character) |  ",
                    "| AreaID_GT | ID of study area in other database, where available (integer) |  ",
                    "\n",
                    "`tbl_budburst.csv`\n",
                    "| Variable | Description |  ",
                    "|:----|:----------- |  ",
                    "| BudburstID | Unique ID of bud burst observation (integer) |  ",
                    "| Year | Calendar year of bud burst observation (integer) |  ",
                    "| TreeID | ID of tree for which bud burst was recorded (integer) |  ",
                    "| AprilDate | Date of bud burst observation in number of days since 1 April (integer) |  ",
                    "| Day | Calendar day of bud burst observation (integer) |  ",
                    "| Month | Calendar month of bud burst observation (integer) |  ",
                    "| TreeTopScore | Score indicating bud burst stage of tree crown (float) |  ",
                    "| TreeAllScore | Score indicating bud burst stage of total tree (float) |  ",
                    "\n",
                    "`tbl_tree.csv`\n",
                    "| Variable | Description |  ",
                    "|:----|:----------- |  ",
                    "| TreeID | Unique ID of tree (integer) |  ",
                    "| AreaID | ID of study area (integer) |  ",
                    "| SiteNumber | Number of site within AreaID (integer) |  ",
                    "| TreeNumber | Number of tree within SiteNumber (integer) |  ",
                    "| TreeSpeciesID | ID of species of tree (integer) |  ",
                    "| Elevation | Elevation of tree location in metres above standard mean sea level (float) |  ",
                    "| Latitude | Latitude (north-south position) of tree location in decimal degrees (float) |  ",
                    "| Longitude | Longitude (east-west position) of tree location in decimal degrees (float) |  ",
                    "\n",
                    "`tbl_treeSpecies.csv`\n",
                    "| Variable | Description |  ",
                    "|:----|:----------- |  ",
                    "| TreeSpeciesID | Unique ID of species of tree (integer) |  ",
                    "| TreeSpeciesName | Vernacular name of species of tree (character) |  "),
           con = "data/README.md")
