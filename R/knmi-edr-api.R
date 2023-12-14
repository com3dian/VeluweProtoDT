# Get daily mean temperature data through KNMI EDR API
# Created: 2023/11/20
# Last updated: 2023/12/14

# Enviromental Data Retrieval (EDR)
# https://developer.dataplatform.knmi.nl/edr-api
# The goal of an EDR API is to query environmental data in a spatio-temporal manner. This data can be queried using different query patterns. You can, for example, query the data at a position or in a spatial cube. The available query endpoints depend on the Collection you are querying.


# Load packages -----------------------------------------------------------

library(tidyverse)
library(stringr)
library(httr)
library(jsonlite)

# Function to retrieve data from the EDR API ------------------------------

# The API has a number of restrictions:
# - Users can send up to 200 requests per second (this is only an issue for high-performance computation)
# - Users can send up to 1000 requests per hour (this may be an issue for long time series)
# - The number of data points in a single request is limited to Nx * Ny * Nz * Nt <= 1,000,000 and Nz * Nt <= 100, where Nx is number of longitudinal grid cells, Ny is number of latitudinal grid cells, Nz is number of altitudinal grid cells, Nt is number of time points. Most often, the second rule is the limiting factor.

get_edr <- function(year,
                    bbox,
                    start_time = "00:00:00",
                    end_time = "23:59:59") {

  # year: year (four-digit integer) for which you want to retrieve data
  # bbox: spatial bounding box for which you want to retrieve data. Vector of four numeric values, indicating western-most, southern-most, eastern-most and northern-most point of the bounding box (in decimal degrees).
  # start_time: start time of period for which you want to retrieve data. Character string of format "hh:mm:ss". Default: "00:00:00".
  # end_time: end time of period for which you want to retrieve data. Character string of format "hh:mm:ss". Default: "23:59:59".

  # Get data for period 1
  edr_period1 <- httr::GET(paste0("https://api.dataplatform.knmi.nl/edr/collections/Tg1/",
                                  "cube?bbox=", paste(bbox, collapse = "%2C"),
                                  "&z=0",
                                  "&parameter-name=temperature",
                                  "&datetime=", paste(year - 1, "12", "01", sep = "-"), "T",
                                  stringr::str_replace_all(start_time, pattern = ":", replacement = "%3A"), "Z%2F",
                                  paste(year, "03", "01", sep = "-"), "T",
                                  stringr::str_replace_all(end_time, pattern = ":", replacement = "%3A"), "Z"),
                           httr::add_headers(Authorization = Sys.getenv("KNMI_EDR_KEY")))

  # Convert from JSON to R readable format
  tg_period1 <- jsonlite::fromJSON(rawToChar(edr_period1$content))



  # Get data for period 2
  edr_period2 <- httr::GET(paste0("https://api.dataplatform.knmi.nl/edr/collections/Tg1/",
                                  "cube?bbox=", paste(bbox, collapse = "%2C"),
                                  "&z=0",
                                  "&parameter-name=temperature",
                                  "&datetime=", paste(year, "03", "02", sep = "-"), "T",
                                  stringr::str_replace_all(start_time, pattern = ":", replacement = "%3A"), "Z%2F",
                                  paste(year, "05", "31", sep = "-"), "T",
                                  stringr::str_replace_all(end_time, pattern = ":", replacement = "%3A"), "Z"),
                           httr::add_headers(Authorization = Sys.getenv("KNMI_EDR_KEY")))

  # Convert from JSON to R readable format
  tg_period2 <- jsonlite::fromJSON(rawToChar(edr_period2$content))

  # Check whether data retrieval is successful or not


  # Match temperature to coordinates
  data_period1 <- tidyr::expand_grid(date = tg_period1$domain$axes$t$values,
                                     y = tg_period1$domain$axes$y$values,
                                     x = tg_period1$domain$axes$x$values) |>
    dplyr::mutate(temperature = tg_period1$ranges$temperature$values)

  data_period2 <- tidyr::expand_grid(date = tg_period2$domain$axes$t$values,
                                     y = tg_period2$domain$axes$y$values,
                                     x = tg_period2$domain$axes$x$values) |>
    dplyr::mutate(temperature = tg_period2$ranges$temperature$values)

  # Output
  data <- dplyr::bind_rows(data_period1, data_period2)

  return(data)

}

#bbox <- c(5.8170137, 52.0240608, 5.8755853, 52.0507969)
bbox <- c(5.824436777442551, 52.032393019069225, 5.870356194968013, 52.046647934312794)

# Retrieve data for 1988 to 2023
temp_data <- purrr::map(.x = 1988:2023,
                        .f = ~{

                          get_edr(year = .x,
                                  bbox = bbox)

                        },
                        .progress = TRUE) |>
  purrr::list_c()
