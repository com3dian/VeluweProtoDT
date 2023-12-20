# Get bud burst data through Dataverse API

# Author: Stefan Vriend
# Created: 2023-12-12
# Last updated: 2023-12-20


# Load packages -----------------------------------------------------------

library(httr)
library(jsonlite)
library(tidyverse)
library(stringr)


# Function to retrieve data from Dataverse API ----------------------------

# Arguments
# dataset: Character specifying the persistent identifier for a dataset. For example, "doi:10.80227/test-QMGPSW".
# version: Character specifying a version of the dataset. This can be one of ":draft" (for current draft), ":latest" (for latest draft or latest publication), ":latest-published" (for latest publication), "x" (where 'x' is a major version), or "x.y" (where 'x' is a major version and 'y' a minor version).
# server: Character specifying a Dataverse server. For example, "demo.dataverse.nl" or "dataverse.nl".
# key: Character specifying a Dataverse server API key. Note: API keys are unique to servers.

retrieve_dataverse_data <- function(dataset,
                                    version = ":latest",
                                    server = "demo.dataverse.nl",
                                    key = rstudioapi::askForSecret("Dataverse API key")) {

  # Check if dataset is provided in right format (i.e., starting with "doi:")
  if(!stringr::str_starts(string = dataset, pattern = "doi:")) {

    dataset_doi <- paste0("doi:", stringr::str_remove(string = dataset, pattern = "DOI:|https://doi.org/"))

  } else {

    dataset_doi <- dataset

  }

  # Retrieve ID that belongs to the data set of interest
  dataset_id <- httr::GET(url = paste0("https://", server, "/api/",
                                       "datasets/:persistentId?persistentId=", dataset_doi),
                          httr::add_headers("X-Dataverse-key" = key)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data") |>
    purrr::pluck("id")

  # Retrieve list of data files that are part of the data set
  dataset_files <- httr::GET(url = paste0("https://", server, "/api/",
                                          "datasets/", dataset_id, "/",
                                          "versions/", version, "/",
                                          "files"),
                             httr::add_headers("X-Dataverse-key" = key)) |>
    httr::content(as = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data") |>
    purrr::pluck("dataFile")

  # Retrieve each data file in list using their unique IDs
  data <- purrr::map(.x = dataset_files$id,
                     .f = ~{

                       httr::GET(url = paste0("https://", server, "/api/",
                                              "access/datafile/", .x),
                                 httr::add_headers("X-Dataverse-key" = key)) |>
                         httr::content(encoding = "UTF-8")

                     }) |>
    purrr::set_names(stringr::str_remove_all(string = dataset_files$filename, "\\..*"))

  # If API is unsuccessful, prompt message to check DOI, version and/or server
  if(purrr::is_empty(data)) {

    stop("Dataverse API failed to fulfill the request. Check whether the provided dataset DOI, version, and/or server are correct.")

  } else {

    return(data)

  }

}

# Retrieve bud burst data files as list (including README)
data <- retrieve_dataverse_data(dataset = "doi:10.80227/test-QMGPSW")

# Store each file as separate R objects for easier use in subsequent scripts
for(i in 1:length(data)) {

  assign(names(data)[i], data[[i]])

}
