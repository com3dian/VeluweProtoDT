# Create Darwin Core archive for bud burst data ####

# Author: Cherine Jantzen
# Created: 12/12/2023

# load package
library(zip)

# gather all files that shall be included in the archive
files = c('event.txt', 'occurrence.txt', 'extendedmeasurementorfact.txt' , "meta.xml", 'EML.xml')

# create a zip file and save archive
zip::zip("DwC-A_budburst.zip", files, root = ".", mode = "mirror")

# show content of newly created archive
zip::zip_list("DwC-A_budburst.zip")
