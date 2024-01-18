# Access temperature scenario data data through GitHub ####

# Author: Cherine Jantzen, Stefan Vriend
# Created: 11/01/2024
# Updated: 18/01/2024


# I. Load packages --------------------------------------------------------

library(tidyverse)
library(httr)


# II. GitHUb access -------------------------------------------------------

# get master branch of repository where scenario data is strored in
master_branch <- httr::GET("https://api.github.com/repos/matt-long/bio-pop-ToE/git/trees/master?recursive=1")

# get the file paths of each file that needs to be downloaded
file_path <- tibble::tibble(path = purrr::map_chr(httr::content(master_branch)$tree, "path")) %>%
  tidyr::separate_wider_delim(path, delim = '/', names = c('base', 'folder', 'filename'),
                              too_few = "align_end", too_many = "drop") %>%
  dplyr::filter(folder == 'data', stringr::str_detect(filename, '.csv'))

# retrieve data
scenario_data_all <- tibble::tibble()

for (i in seq(1, nrow(file_path))){

  # get path name for each file
  path <- paste0('https://raw.githubusercontent.com/matt-long/bio-pop-ToE/master/notebooks/data/', file_path$filename[i])

  # read each file as CSV
  df <- readr::read_csv(httr::content(httr::GET(path)))

  # assign file name to data
  file_name <- file_path$filename[i]

  # extract scenario name from file name
  df1 <- df %>%
    dplyr::mutate(scenario_name = stringr::str_extract(file_name, pattern = "1pt5degC(?=\\.)|1pt5degC_OS|2pt0degC|RCP85|RCP45"),
                  member_id = as.character(member_id))

  scenario_data_all <- bind_rows(scenario_data_all, df1)

}

# discard data before 1985-01-01 to limit file size
scenario_data_all <- scenario_data_all %>%
  dplyr::filter(time >= as.POSIXct("1985-01-01", tz = "UTC"))

# save scenario data as CSV locally
write.csv(scenario_data_all, file = here::here("data", "scenario_temperatures.csv"), row.names = FALSE)

# zip csv to reduce file size
zip::zip(zipfile = here::here("data", "scenario_temperatures.zip"), files = here::here("data", "scenario_temperatures.csv"))
file.remove(here::here("data", "scenario_temperatures.csv"))
