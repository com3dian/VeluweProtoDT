# Get daily weather data through KNMI EDR API
# Author: Stefan Vriend
# Created: 2023/11/20
# Last updated: 2024/01/22

# Environmental Data Retrieval (EDR)
# https://developer.dataplatform.knmi.nl/edr-api
# The goal of an EDR API is to query environmental data in a spatiotemporal manner. This data can be queried using different query patterns. You can, for example, query the data at a position or in a spatial cube. The available query endpoints depend on the Collection you are querying.

# The API has a number of restrictions:
# - Users can send up to 200 requests per second (this is only an issue for high-performance computation)
# - Users can send up to 1000 requests per hour (this may be an issue for long time series)
# - The number of data points in a single request is limited to Nx * Ny * Nz * Nt <= 1,000,000 and Nz * Nt <= 100, where Nx is number of longitudinal grid cells, Ny is number of latitudinal grid cells, Nz is number of altitudinal grid cells, Nt is number of time points. Most often, the second rule is the limiting factor.

# Load packages -----------------------------------------------------------

library(tidyverse)
library(here)

# I. Function to retrieve data from the EDR API ------------------------------

# Arguments:
# bbox: spatial bounding box for which to retrieve data. Vector of four numeric values, indicating western-most, southern-most, eastern-most and northern-most point of the bounding box (in decimal degrees).
# variable: weather variable of interest. Either "mean temperature", "max temperature", "min temperature" or "precipitation".
# start_date: start date of period for which to retrieve data. Date format "yyyy-mm-dd". Use e.g., `lubridate::make_date()`.
# start_time: start time of period for which to retrieve data. Character string of format "hh:mm:ss". Default: "00:00:00".
# end_date: end date of period for which to retrieve data. Date format "yyyy-mm-dd". Use e.g., `lubridate::make_date()`.
# end_time: end time of period for which to retrieve data. Character string of format "hh:mm:ss". Default: "23:59:59".
# knmi_edr_key: user-specific KNMI EDR API key (request here: https://developer.dataplatform.knmi.nl/edr-api#token). Character string.

retrieve_knmi_edr_data <- function(bbox,
                                   variable = c("mean temperature", "max temperature",
                                                "min temperature", "precipitation"),
                                   start_date,
                                   start_time = "00:00:00",
                                   end_date,
                                   end_time = "23:59:59",
                                   knmi_edr_key = Sys.getenv("KNMI_EDR_KEY")) {

  # Create KNMI variable lookup table to match variable inputs to KNMI collection & parameter names
  knmi_var_lookup <- tibble::tibble(
    collection = c("Tg1", "Tx1", "Tn1", "Rd1"),
    parameter = c("temperature", "temperature", "temperature", "precipitation"),
    var_name = c("mean temperature", "max temperature", "min temperature", "precipitation")
  )

  if(!(variable %in% knmi_var_lookup$var_name)) {

    stop("The weather variable you provided does not exist. Select one of: 'mean temperature', 'max temperature', 'min temperature', or 'precipitation'.")

  }

  # Check that start_date and end_date are of class 'Date' to ensure that we can retrieve
  if(!any(class(start_date) == "Date", class(end_date) == "Date")) {

    stop("Please provide dates as 'yyyy-mm-dd'.")

  }

  repeat({ # If retrieving data from KNMI EDR API fails, try again

    # Send GET request to KNMI EDR API
    edr_get <- httr::GET(url = paste0("https://api.dataplatform.knmi.nl/edr/collections/",
                                      knmi_var_lookup |> dplyr::filter(var_name == variable) |> dplyr::pull("collection"),
                                      "/cube?bbox=", paste(bbox, collapse = "%2C"),
                                      "&z=0",
                                      "&parameter-name=",
                                      knmi_var_lookup |> dplyr::filter(var_name == variable) |> dplyr::pull("parameter"),
                                      "&datetime=",
                                      start_date, "T",
                                      stringr::str_replace_all(string = start_time,
                                                               pattern = ":",
                                                               replacement = "%3A"), "Z%2F",
                                      end_date, "T",
                                      stringr::str_replace_all(string = end_time,
                                                               pattern = ":",
                                                               replacement = "%3A"), "Z"),
                         httr::add_headers(Authorization = knmi_edr_key))

    # Convert from JSON to R readable format
    edr_data <- jsonlite::fromJSON(txt = rawToChar(x = edr_get$content))

    # If unsuccessful, print message (and try again)
    if(is.null(edr_data$domain)) message(paste0("KNMI EDR API failed to fulfill request with starting date ",
                                                start_date, ". Will try again."))

    # If successful, end
    if(!is.null(edr_data$domain)) break()

  })

  # Output
  return(edr_data)

}



# II. Get temperature data for desired spatiotemporal parameters ---------

# Define bounding box for Veluwe site
bbox <- c(5.824436777442551, 52.032393019069225, 5.870356194968013, 52.046647934312794)

# Retrieve data for 1988 to 2023
temp <- purrr::map(.x = 1988:2023,
                        .f = ~{

                          # Retrieve data for period 1 (1 Dec to 1 March)
                          period1 <- retrieve_knmi_edr_data(bbox = bbox,
                                                            variable = "mean temperature",
                                                            start_date = lubridate::make_date(.x - 1, 12, 1),
                                                            start_time = "00:00:00",
                                                            end_date = lubridate::make_date(.x, 3, 1),
                                                            end_time = "23:59:59")

                          # Retrieve data for period 2 (2 March to 31 May)
                          period2 <- retrieve_knmi_edr_data(bbox = bbox,
                                                            variable = "mean temperature",
                                                            start_date = lubridate::make_date(.x, 3, 2),
                                                            start_time = "00:00:00",
                                                            end_date = lubridate::make_date(.x, 5, 31),
                                                            end_time = "23:59:59")

                          # Match temperature data to dates and coordinates
                          data1 <- tidyr::expand_grid(date = period1$domain$axes$t$values,
                                                      y = period1$domain$axes$y$values,
                                                      x = period1$domain$axes$x$values) |>
                            dplyr::mutate(temperature = period1$ranges$temperature$values)

                          data2 <- tidyr::expand_grid(date = period2$domain$axes$t$values,
                                                      y = period2$domain$axes$y$values,
                                                      x = period2$domain$axes$x$values) |>
                            dplyr::mutate(temperature = period2$ranges$temperature$values)

                          # Bind output of two periods
                          data <- dplyr::bind_rows(data1, data2)

                          return(data)

                        },
                        .progress = TRUE) |>
  purrr::list_c()


# save temperature data 
write.csv(temp, here::here("data", "Tg1_seasonalTemperature_Dec1987_to_June2023.csv"), row.names = FALSE)
