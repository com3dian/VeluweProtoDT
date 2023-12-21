# Climwin model: relation of bud burst and temperature #### 

# Authors: Cherine Jantzen & Stefan Vriend
# Created: 28/11/2023
# Last updated: 21/12/2023


# I. Retrieve data & load packages ----------------------------------------

# load packages
library(climwin)
library(dplyr)
library(lubridate)

# retrieve data
temp <- read.csv(here::here("data", "temp_climwin_input.csv"))
avg_annual_budburst_dates <- read.csv(here::here("data", "budburst_climwin_input.csv"))


# II. Date conversion to fit climwin  -------------------------------------

# climwin can only handle dates in the format "dd/mm/yyyy" (as a character), so dates
# have to be converted before modelling. However, there need to be numeric dates for the baseline models too. 

# convert dates of temperature file
temp <- temp %>% mutate(date = lubridate::as_date(date),
                        year = lubridate::year(date),
                        month = lubridate::month(date),
                        day = lubridate::day(date),
                        doy = lubridate::yday(date), 
                        dummy = month * 100 + day)

temp$factor_date <- as.factor(paste(temp$day, temp$month, temp$year, sep = "/"))

# convert bud burst dates
avg_annual_budburst_dates$date_info <- paste(avg_annual_budburst_dates$year, floor(avg_annual_budburst_dates$avg_bud_burst_DOY))
avg_annual_budburst_dates$date <- strptime(avg_annual_budburst_dates$date_info, "%Y %j")
avg_annual_budburst_dates$date <- as.factor(format(as.Date(avg_annual_budburst_dates$date), "%d/%m/%Y"))

# create numeric dates to be used in the baseline model &
# and exclude location 7, as this labels trees without coordinates 
avg_annual_budburst_dates <- avg_annual_budburst_dates %>% 
  dplyr::mutate(formatted_date = as.Date(as.character(date), format = "%d/%m/%Y"),
                DOY = lubridate::yday(formatted_date)) %>%
  dplyr::filter(date != "NA", 
                locID != "loc7")



# III. Climwin: Sliding window model --------------------------------------------

## 1. Function to calculate the window in which bud burst data is best explained by temperature ####

# Arguments
# biological_data: Tibble specifying the biological input data for the climate model, containing biological dates that are tested in format 'dd/mm/yyyy'. Only necessary when the first window is calculated.
# reference_day: Numeric vector of 2 values specifying the day and month of the reference day before which climate windows are tested. For example, c(31, 5) for the 31st of March.
# range: Numeric vector of 2 values specifying the range of days before the reference day in which climate windows are tested. For example, c(181, 0), meaning that windows between 181 days and 0 days before the reference day are tested.
# window_number: Choice between "first" and "second", specifies whether the first best window should be calculated or a second window based on the first one.
# first_window: Tibble containing the best model data of the first window/iteration of the function. Used as input data when second window should be calculated. 

find_climate_window <- function(biological_data = NULL,
                                range,
                                reference_day,
                                window_number = c("first", "second"),
                                first_window = NULL) {
  
  # find 'first' or 'second' climate window
  if(window_number == "first") {
    
    # error if biological data is not provided when searching for first window
    if(is.null(biological_data)) {
      
      stop("If you want to find a first climate window, provide the biological data as `biological_data`.")
      
    }
    
    # define baseline
    baseline <- lm(DOY ~ year, data = biological_data)
    
  } else if(window_number == "second") {
    
    # error if first window output is not provided when searching for second window
    if(is.null(first_window)) {
      
      stop("If you want to find a second climate window, provide the output of the first iteration of `find_climate_window()` as `first_window`.")
      
    }
    
    biological_data <- first_window$biological_data
    
    # the first window is added as an explanatory variable to the baseline model
    baseline_data <- first_window$best_window[[1]]$BestModelData %>% 
      dplyr::rename("first_window" = "climate",
                    "DOY" = "yvar")
    
    # define baseline
    baseline <- lm(DOY ~ year + first_window, data = baseline_data)
    
  }
  
  # climwin analysis: Find best window 
  best_window <- climwin::slidingwin(baseline = baseline, 
                                     xvar = list(Temp = temp$temperature),
                                     cdate = temp$factor_date, 
                                     bdate = biological_data$date,
                                     type = "absolute", 
                                     refday = reference_day,
                                     spatial = list(biological_data$locID, temp$locID),
                                     range = range, 
                                     func = "lin", 
                                     stat = "mean")  
  
  # Back calculation of the opening and closing day of the calculated window to calender dates
  
  # create a reference year for calculation of start and end date (can be any year that is not a leap year, as dates should be calculated on the basis of regular years)
  reference_year <- dplyr::if_else(condition = lubridate::leap_year(max(temp$year)), 
                                   true = max(temp$year) - 1, 
                                   false = max(temp$year))
  
  # calculate calender date when window opens
  start_date <- lubridate::make_date(year = reference_year, 
                                     month = reference_day[2], 
                                     day = reference_day[1]) - best_window$combos[1,]$WindowOpen
  
  # calculate calender date when window closes
  end_date <- lubridate::make_date(year = reference_year, 
                                   month = reference_day[2], 
                                   day = reference_day[1]) - best_window$combos[1,]$WindowClose
  
  return(tibble::lst(best_window, biological_data, baseline, range, reference_day, start_date, end_date))
  
}


## 2. Calculate windows for each species of interest ####
first_window_Qrobur <- find_climate_window(biological_data = avg_annual_budburst_dates %>% 
                                             dplyr::filter(stringr::str_detect(scientificName, "Quercus robur")) ,
                                           window_number = "first",
                                           reference_day = c(31, 5),
                                           range = c(181, 0))

first_window_Qrubra <- find_climate_window(biological_data = avg_annual_budburst_dates %>%
                                             dplyr::filter(stringr::str_detect(scientificName, "Quercus rubra")),
                                           reference_day = c(31, 5),
                                           range = c(181, 0),
                                           window_number = "first")

## Check for a second window
## There sometimes can be a second climate window that fits the data. To test for that, the data of the 
## best window (i.e., first window) is taken as the input data for the same model as before.
second_window_Qrobur <- find_climate_window(window_number = "second",
                                           reference_day = c(31, 5),
                                           range = c(181, 0),
                                           first_window = first_window_Qrobur)

second_window_Qrubra <- find_climate_window(window_number = "second",
                                            reference_day = c(31, 5),
                                            range = c(181, 0),
                                            first_window = first_window_Qrubra)

# VI. Check significance of model -----------------------------------------

# To account for over fitting and to check whether the selected window has been selected by chance, 
# a randomization test on that window is performed. 

## 1. Function for climwin randomization ####

# Arguments
# x: Output of find_first_climate_window() 
# repeats: Numeric specifying the number of repeats used in the randomization model of climwin. Default is set to 20. 

randomization_climate_window <- function(x,
                                         repeats = 20) {
  
  # get components of x
  baseline <- x$baseline
  biological_data <- x$biological_data
  reference_day <- x$reference_day
  range <- x$range
  dataset <- x$best_window[[1]]$Dataset
  
  # climate window analysis for randomised data
  window_rand <- climwin::randwin(repeats = repeats, 
                                  baseline = baseline,
                                  xvar = list(Temp = temp$temperature),
                                  cdate = temp$factor_date, 
                                  bdate = biological_data$date,
                                  type = "absolute", 
                                  refday = reference_day,
                                  spatial = list(biological_data$locID, temp$locID),
                                  range = range, 
                                  func = "lin", 
                                  stat = "mean")
  
  # get the p-value of the randomization test
  climwin_p <- climwin::pvalue(dataset = dataset,
                               datasetrand = window_rand[[1]],
                               metric = "C", 
                               sample.size = length(unique(biological_data$year)))
  
  return(climwin_p)
  
}


## 2. Calculate p-values for each window ####

# get p-values for both windows of Quercus robur
p_Qrobur_firstwin <- randomization_climate_window(x = first_window_Qrobur)
p_Qrobur_secondwin <- randomization_climate_window(x = second_window_Qrobur)

# get p-values for both windows Quercus rubra
p_Qrubra_firstwin <- randomization_climate_window(x = first_window_Qrubra)
p_Qrubra_secondwin <- randomization_climate_window(x = second_window_Qrubra)
