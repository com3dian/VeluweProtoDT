# Pipeline for creation of meta.xml file of the DwC-A of bud burst data
# The meta.xml is a descriptor file that informs how the Darwin Core tables are organised

# Author: Stefan Vriend
# Created: 2023-12-20

# Load packages
library(xml2)


# Function to create meta.xml from Darwin Core tables ---------------------

# Arguments:
# - core: Character string specifying the file location of the core table.
# - extension: (optional) Character string specifyin the file locations of the extension tables.

create_meta_xml <- function(core,
                            extensions = NULL) {

  # A DwC-Archive must contain exactly one <core> element, representing the core table.

  # A DwC-Archive may contain zero or more <extension> elements, each representing an individual extension table.








}

meta <- xml2::xml_new_root(.value = "archive",
                           .version = "1.0",
                           .encoding = "UTF8")



xml2::write_xml(meta, "meta.xml")
