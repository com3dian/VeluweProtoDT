# Retrieve data from data base ####

# Author: Cherine Jantzen
# Created: 12/12/2023
# Last updated: 18/12/2023

# load packages
library(DBI)
library(here)
library(odbc)
library(rstudioapi)
library(tidyverse)

# connect to NIOO AnE SQL server to retrieve data
con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = {"ODBC Driver 17 for SQL Server"},
                      UID = rstudioapi::askForPassword("Uid"),
                      Pwd = rstudioapi::askForPassword("Pwd"),
                      Server = rstudioapi::askForPassword("Server"))

# get input data

## tbl_Area
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Area")) %>%
  dplyr::collect() %>%
  write.csv(here::here("data", "tbl_area.csv"), row.names = FALSE)

## tbl_Budburst
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Budburst")) %>%
  dplyr::collect() %>%
  write.csv(here::here("data", "tbl_budburst.csv"), row.names = FALSE)

## tbl_Tree
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Tree")) %>%
  dplyr::collect() %>%
  write.csv(here::here("data", "tbl_Tree.csv"), row.names = FALSE)

## tbl_TreeSpecies
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_TreeSpecies")) %>%
  dplyr::collect() %>%
  write.csv(here::here("data", "tbl_TreeSpecies.csv"), row.names = FALSE)
