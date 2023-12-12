# Retrieve data from data base ####

# Author: Cherine Jantzen
# Date: 12/12/2023

# load packages
library(DBI)
library(odbc)
library(rstudioapi)
library(tidyverse)




# connect to NIOO AnE SQL server to retrieve data
con <- DBI::dbConnect(drv = odbc::odbc(),
                      Driver = {'SQL Server'},
                      UID = rstudioapi::askForPassword('Uid'),
                      Pwd = rstudioapi::askForPassword('Pwd'),
                      Server = 'ws.ane.nioo.int')

# get input data

## tbl_Area
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Area")) %>%
  dplyr::collect() %>% 
  write.csv('data/tbl_Area.csv')

## tbl_Budburst
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Budburst")) %>%
  dplyr::collect() %>% 
  write.csv('data/tbl_budburst.csv')

## tbl_Tree
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_Tree")) %>%
  dplyr::collect() %>% 
  write.csv('data/tbl_Tree.csv')

## tbl_TreeSpecies
dplyr::tbl(con, dbplyr::in_catalog(catalog = "AnE_Budburst", schema = "dbo", table = "tbl_TreeSpecies")) %>%
  dplyr::collect() %>% 
  write.csv('data/tbl_TreeSpecies.csv')
