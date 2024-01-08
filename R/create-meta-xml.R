# Pipeline for creation of meta.xml file of the DwC-A of bud burst data
# The meta.xml is a descriptor file that informs how the Darwin Core tables are organised

# Author: Stefan Vriend
# Created: 2023-12-20
# Last updated: 2024-01-04

# Load packages
library(xml2)
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)

# Function to create meta.xml from Darwin Core tables ---------------------

# Arguments:
# - core: Named vector specifying the DwC name of the core table and the file path of the core table.
#   For example, c("Event" = "C:\\Users\\event.csv").
# - extension: (optional) Named vector specifying the names and the file locations of the extension tables.
#   For example, c("Occurrence" = "C:\\Users\\occurrence.csv", "ExtendedMeasurementOrFact" = "C:\\Users\\emof.csv")
# - file: Character string specifying the file path and file name of the output xml

create_meta_xml <- function(core,
                            extensions = NULL,
                            file) {

  # Check if supplied name(s) of the table(s) exist(s) in DwC namespace
  dwc_namespace_classes <- httr::GET("https://rs.gbif.org/extensions.html") |>
    httr::content() |>
    rvest::html_table() |>
    purrr::map(.f = ~{

      .x |>
        tidyr::pivot_wider(names_from = "X1", values_from = "X2")

    }) |>
    dplyr::bind_rows() |>
    dplyr::select("Name", "Namespace", "RowType")

  core_names <- dwc_namespace_classes |>
    dplyr::filter(Name %in% c("Event", "Occurrence", "Taxon")) |>
    dplyr::pull("Name")

  if(!(names(core) %in% core_names)) {

    stop("Please provide a core table name that exists in the Darwin Core namespace. Check 'https://rs.gbif.org/extensions.html' for all options.")

  }

  if(!is.null(extensions) && any(!(names(extensions) %in% dwc_namespace_classes$Name))) {

    stop("Please provide an extension table name that exists in the Darwin Core namespace. Check 'https://rs.gbif.org/extensions.html' for all options.")

  }

  # Create a new XML document with root <archive>
  meta <- xml2::xml_new_root(.value = "archive",
                             .version = "1.0",
                             .encoding = "UTF-8",
                             # Set namespace arguments
                             "xmlns" = "http://rs.tdwg.org/dwc/text/",
                             "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                             "xmlns:xs" = "http://www.w3.org/2001/XMLSchema",
                             "xsi:schemaLocation" = "http://rs.tdwg.org/dwc/text/ http://rs.tdwg.org/dwc/text/tdwg_dwc_text.xsd")

  # A DwC-Archive must contain exactly one <core> element, representing the core table
  xml2::xml_add_child(.x = meta,
                      .value = "core",
                      # Set arguments
                      "encoding" = "UTF-8",
                      "fieldsTerminatedBy" = ",",
                      "linesTerminatedBy" = "\\n",
                      "fieldsEnclosedBy" = "",
                      "ignoreHeaderLines" = "1",
                      "rowType" = dwc_namespace_classes |>
                        dplyr::filter(Name == names(core)) |>
                        dplyr::pull("RowType")) |>
    # Add file name
    xml2::xml_add_child(.value = "files") |>
    xml2::xml_add_child(.value = "location",
                        basename(core)) # Retain name and extension of file without path

  # Read in core table
  core_file <- read.csv(core)

  # Get core ID, i.e., the identifier of a record in the core table
  core_id <- paste0(tolower(names(core)), "ID")

  # Add header, and indicate which column indicates the coreID
  xml2::xml_children(meta) |>
    xml2::xml_add_child(.value = "id",
                        "index" = paste(which(names(core_file) == core_id) - 1))

  # Add namespace of each variable in core
  purrr::walk2(.x = rev(names(core_file)),
               .y = rev(seq_len(length(names(core_file)))),
               .f = ~{

                 xml2::xml_add_sibling(.x = xml2::xml_children(xml2::xml_children(meta))[2],
                                       .value = "field",
                                       "index" = .y,
                                       "term" = assign_uri(.x))

               })

  # A DwC-Archive may contain zero or more <extension> elements, each representing an individual extension table.
  if(!is.null(extensions)) {

    # Loop through extensions
    purrr::walk2(.x = names(extensions),
                 .y = extensions,
                 .f = ~{

                   extension <- .x

                   xml2::xml_add_child(.x = meta,
                                       .value = "extension",
                                       # Set arguments
                                       "encoding" = "UTF-8",
                                       "fieldsTerminatedBy" = ",",
                                       "linesTerminatedBy" = "\\n",
                                       "fieldsEnclosedBy" = "",
                                       "ignoreHeaderLines" = "1",
                                       "rowType" = dwc_namespace_classes |>
                                         dplyr::filter(Name == extension) |>
                                         dplyr::pull("RowType")) |>
                     # Add file name
                     xml2::xml_add_child(.value = "files") |>
                     xml2::xml_add_child(.value = "location",
                                         basename(.y)) # Retain name and extension of file without path

                 })

    # Loop through extensions oncemore to add header and variable namespace
    purrr::pwalk(.l = list(names(extensions),
                           extensions,
                           seq_len(length(extensions))),
                 .f = ~{

                   # Read in extension table
                   extension_file <- read.csv(..2)
                   extension_number <- ..3

                   # Add header, and indicate which column indicates the coreID
                   xml2::xml_children(meta)[extension_number + 1] |>
                     xml2::xml_add_child(.value = "coreid",
                                         "index" = paste(which(names(extension_file) == core_id) - 1))

                   # Add namespace of each variable in extension
                   purrr::walk2(.x = names(extension_file),
                                .y = seq_len(length(names(extension_file))),
                                .f = ~{

                                  xml2::xml_add_child(.x = xml2::xml_children(meta)[extension_number + 1],
                                                      .value = "field",
                                                      "index" = .y,
                                                      "term" = assign_uri(.x))

                                })

                 })

  }

  # Save xml
  xml2::write_xml(meta, file)

  # Validate xml
  valid <- suppressWarnings({
    xml2::xml_validate(x = xml2::read_xml(file),
                       schema = xml2::read_xml("https://dwc.tdwg.org/text/tdwg_dwc_text.xsd"))
  })

  if(!valid) {

    file.remove(file)
    stop("The generated XML is not schema-valid.")

  }

}

# Functions to find a variable's namespace and assign its URI -------------

# Function to validate url, wrapped in purrr::safely() to capture errors easily
# This function is typically only used inside `assign_uri()`
try_url <- purrr::safely(.f = ~{

  url_head <- httr::HEAD(.x)

  if(url_head$status_code == 404) {

    stop("HTTP 404: Page not found.")

  } else if(url_head$status_code == 200) {

    if(stringr::str_detect(string = url_head$url, pattern = .x)) {

      return("Requested URL seems to be the valid URI for this term.")

    } else {

      stop("Request for URL does not reciprocate URL.")

    }

  }

}, otherwise = NULL)

# Assign uri to term. Either Darwin Core, Dublin Core, or self-assigned.
# Argument
# - term: Character specifying the name of the variable for which we find the associated namespace and resource identifier

assign_uri <- function(term) {

  # Try Darwin Core
  url <- paste0("http://rs.tdwg.org/dwc/terms/", term)
  try_dwc <- try_url(url)

  if(is.null(try_dwc$error) & !is.null(try_dwc$result)) {

    uri <- httr::GET(url) |>
      httr::content(as = "text", encoding = "UTF-8") |>
      jsonlite::fromJSON() |>
      purrr::pluck("@graph") |>
      dplyr::filter(stringr::str_detect(string = `@id`, pattern = ".json")) |>
      tidyr::unnest("dcterms:references", names_sep = "_") |>
      dplyr::pull("dcterms:references_@id")

    # Try Dublin Core
    # Note: the URI in the "/elements/1.1/" namespace of Dublin Core starts with "http://purl.org/dc/elements/1.1/X"
    # but redirects to "https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/X",
    # where X is the name of the term

  } else {

    url <- paste0("https://www.dublincore.org/specifications/dublin-core/dcmi-terms/elements11/", term)
    try_dc <- try_url(url)

    if(is.null(try_dc$error) & !is.null(try_dc$result)) {

      uri <- httr::GET(url) |>
        httr::content(encoding = "UTF-8") |>
        rvest::html_table() |>
        purrr::list_c() |>
        dplyr::filter(X1 == "URI") |>
        dplyr::pull("X2")

      # Ask user for URI
    } else {

      cat(paste0("The term `", term, "` does not match one of the expected metadata schema."))
      domain <- readline("Please provide the URI of this term (without quotation marks): ")

      uri <- paste0(domain,
                    dplyr::if_else(stringr::str_ends(string = domain,
                                                     pattern = "/"), "", "/"),
                    term)

    }

  }

  return(uri)

}


# Create meta.xml for bud burst DwC-A -------------------------------------

create_meta_xml(core = c("Event" = here::here("data", "event.csv")),
                extensions = c("ExtendedMeasurementOrFact" = here::here("data", "extendedmeasurementorfact.csv"),
                               "Occurrence" = here::here("data", "occurrence.csv")),
                file = here::here("data", "meta.xml"))
