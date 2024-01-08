# Bud burst data - Animal Ecology/NIOO-KNAW

### Authors

 - Cherine C. Jantzen (https://orcid.org/0009-0006-0723-2682)
 - Stefan J.G. Vriend (https://orcid.org/0000-0002-9006-5988)
 - Marcel E. Visser (http://orcid.org/0000-0002-1456-1939)


### Description
The stage of bud burst has been measured from 1988 onwards on several tree species in different areas across the Netherlands to record the phenology of the trees.


### File description
`tbl_area.csv`

| Variable | Description |  
|:----|:----------- |  
| AreaID | Unique ID of study area (integer) |  
| AreaShortName | Abbreviated name or code of study area (character) |  
| AreaName | Full name of study area (character) |  
| AreaID_GT | ID of study area in other database, where available (integer) |  


`tbl_budburst.csv`

| Variable | Description |  
|:----|:----------- |  
| BudburstID | Unique ID of bud burst observation (integer) |  
| Year | Calendar year of bud burst observation (integer) |  
| TreeID | ID of tree for which bud burst was recorded (integer) |  
| AprilDate | Date of bud burst observation in number of days since 1 April (integer) |  
| Day | Calendar day of bud burst observation (integer) |  
| Month | Calendar month of bud burst observation (integer) |  
| TreeTopScore | Score indicating bud burst stage of tree crown (float) |  
| TreeAllScore | Score indicating bud burst stage of total tree (float) |  


`tbl_tree.csv`

| Variable | Description |  
|:----|:----------- |  
| TreeID | Unique ID of tree (integer) |  
| AreaID | ID of study area (integer) |  
| SiteNumber | Number of site within AreaID (integer) |  
| TreeNumber | Number of tree within SiteNumber (integer) |  
| TreeSpeciesID | ID of species of tree (integer) |  
| Elevation | Elevation of tree location in metres above standard mean sea level (float) |  
| Latitude | Latitude (north-south position) of tree location in decimal degrees (float) |  
| Longitude | Longitude (east-west position) of tree location in decimal degrees (float) |  


`tbl_treeSpecies.csv`

| Variable | Description |  
|:----|:----------- |  
| TreeSpeciesID | Unique ID of species of tree (integer) |  
| TreeSpeciesName | Vernacular name of species of tree (character) |  
