# Pipeline for creation of meta.xml file of the DwC-A of bud burst data
# The meta.xml is a descriptor file that informs how the Darwin Core tables are organised

# Author: Stefan Vriend
# Created: 2023-12-20
# Last updated: 2023-12-21

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


# Function to verify whether a url exists ---------------------------------

# Function to validate url, wrapped in purrr::safely() to capture errors easily
# Typically only used within `assign_uri()`
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

      uri <- readline(paste0("The term `", term, "` does not match one of the expected metadata schema, please provide the URI of this term:"))

    }

  }

  return(uri)

}
