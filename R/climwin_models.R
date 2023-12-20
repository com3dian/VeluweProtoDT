# Climwin model: relation of bud burst and temperature #### 

# Author: Cherine Jantzen
# Created: 28/11/2023
# Last updated: 19/12/2023


# I. Retrieve data & load packages ----------------------------------------

# load packages
library(climwin)
library(dplyr)
library(lubridate)

# retrieve data
temp <- read.csv(here::here("data", "temp_climwin_input.csv"))
budburst <- read.csv(here::here("data", "budburst_climwin_input.csv"))


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
budburst$date_info <- paste(budburst$year, floor(budburst$avg_bud_burst_DOY))
budburst$date <- strptime(budburst$date_info, "%Y %j")
budburst$date <- as.factor(format(as.Date(budburst$date), "%d/%m/%Y"))

# create numeric dates to be used in the baseline model &
# and exclude location 7, as this labels trees without coordinates 
budburst <- budburst %>% 
  dplyr::mutate(formatted_date = as.Date(as.character(date), format = "%d/%m/%Y"),
                DOY = lubridate::yday(formatted_date)) %>%
  dplyr::filter(date != "NA", 
                locID != "loc7")



# III. Climwin: Sliding window model --------------------------------------------

# 1. Function to calculate the window in which bud burst data is best explained by temperature ####

# Arguments
# species: Character specifying the tree species by its scientific Name (with author information) as in the input data. For example, "Quercus robur L." 
# reference_day: Numeric vector of 2 values specifying the day and month of the reference day before which climate windows are tested. For example, c(31, 5) for the 31st of March.
# range: Numeric vector of 2 values specifying the range of days before the reference day in which climate windows are tested. For example, c(181, 0), meaning that windows between 181 days and 0 days before the reference day are tested.

find_first_climate_window <- function(species,
                                      reference_day,
                                      range) {
  
  
  # filter input data for species  
  model_input  <- budburst %>% dplyr::filter(scientificName == species)
  
  # Climwin analysis: Find first window 
  first_window <- climwin::slidingwin(baseline = lm(DOY ~ year, data = model_input), 
                                      xvar = list(Temp = temp$temperature),
                                      cdate = temp$factor_date, 
                                      bdate = model_input$date,
                                      type = "absolute", 
                                      refday = reference_day,
                                      spatial = list(model_input$locID, temp$locID),
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
                                     day = reference_day[1]) - first_window$combos[1,]$WindowOpen
  
  # calculate calender date when window closes
  end_date <- lubridate::make_date(year = reference_year, 
                                   month = reference_day[2], 
                                   day = reference_day[1]) - first_window$combos[1,]$WindowClose
  
  
  return(list(first_window, model_input, range, reference_day, start_date, end_date))
}


# 2. Ccalculate windows for each species of interest ####
first_window_Qrobur <- find_first_climate_window(species = 'Quercus robur L.',
                                                 reference_day = c(31, 5),
                                                 range = c(181, 0))

first_window_Qrubra <- find_first_climate_window(species = 'Quercus rubra L.',
                                                 reference_day = c(31, 5),
                                                 range = c(181, 0))


# VI. Check significance of model -----------------------------------------
# To account for over fitting and to check whether the selected window has been selected by chance, 
# a randomization test on that window is performed. 

# 1. Function for climwin randomization ####

# Arguments
# x: Output of find_first_climate_window() 
# repeats: Numeric specifying the number of repeats used in the randomization model of climwin. Default is set to 20. 

randomization_climate_window <- function(x,
                                         repeats = 20) {
  
  # Get components of x
  model_input <- x$model_input
  reference_day <- x$reference_day
  range <- x$range
  dataset <- x$first_window[[1]]$Dataset
  
  # Climate window analysis for randomised data
  first_window_rand <- climwin::randwin(repeats = repeats, 
                                        baseline = lm(DOY ~ year, data = model_input),
                                        xvar = list(Temp = temp$temperature),
                                        cdate = temp$factor_date, 
                                        bdate = model_input$date,
                                        type = "absolute", 
                                        refday = reference_day,
                                        spatial = list(model_input$locID, temp$locID),
                                        range = range, 
                                        func = "lin", 
                                        stat = "mean")
  
  # get the p-value of the randomization test
  climwin_p <- climwin::pvalue(dataset = dataset,
                               datasetrand = first_window_rand[[1]],
                               metric = "C", 
                               sample.size = length(unique(model_input$year)))
  
  return(climwin_p)
  
}

# V. Check for a second window ---------------------------------------------------------
# There sometimes can be a second climate window that fits the data. To test for that, the data of the 
# best window (i.e., first window) is taken as the input data for the same model as before.

# save data of best model of first window for use as new data for the second window
new_model_data <- first_window_Qrobur[[1]]$BestModelData 

# Rename column 
new_model_data$first_window <- new_model_data$climate 

## 1. slidingwin to find second window on data of first window ####
second_window_Qrobur <- climwin::slidingwin(baseline = lm(yvar ~ first_window + year, data = new_model_data), 
                                     xvar = list(Temp = temp$temperature),
                                     cdate = temp$factor_date, 
                                     bdate = budburst_Qrobur$date,
                                     type = "absolute", 
                                     refday = refday,
                                     spatial = list(budburst_Qrobur$locID, temp$locID),
                                     range = range, 
                                     func = "lin", 
                                     stat = "mean")

# give model results
second_window_Qrobur$combos

## 2. Randomization ####
# slidingwin to find second window on data of first window
second_window_Qrobur_rand <- climwin::randwin(repeats = 20,
                                              baseline = lm(yvar ~ first_window + year, data = new_model_data), 
                                              xvar = list(Temp = temp$temperature),
                                              cdate = temp$factor_date, 
                                              bdate = budburst_Qrobur$date,
                                              type = "absolute", 
                                              refday = refday,
                                              spatial = list(budburst_Qrobur$locID, temp$locID),
                                              range = range, 
                                              func = "lin", 
                                              stat = "mean")

# p-value of randomization
climwin::pvalue(dataset = second_window_Qrobur[[1]]$Dataset, 
                datasetrand = second_window_Qrobur_rand[[1]],
                metric = "C", 
                sample.size = length(unique(budburst_Qrobur$year)))


# VI. Climate windows for second species: Quercus rubra  ------------------

# select tree species: Quercus rubra
budburst_Qrubra <- budburst %>% filter(scientificName == 'Quercus rubra L.')


## 2. Climwin analysis: Find first window ####
first_window_Qrubra <- climwin::slidingwin(baseline = lm(DOY ~ year, data = budburst_Qrubra), 
                                           xvar = list(Temp = temp$temperature),
                                           cdate = temp$factor_date, 
                                           bdate = budburst_Qrubra$date,
                                           type = "absolute", 
                                           refday = refday,
                                           spatial = list(budburst_Qrubra$locID, temp$locID),
                                           range = range, 
                                           func = "lin", 
                                           stat = "mean")


# best model window results
first_window_Qrubra$combos


## 3. Calculate calender dates of the first window ####

## back-calculate start day of year of window
start_win <- range[1] - first_window_Qrubra$combos[1,7]

## transform result to calender date (year is set randomly to a year within  the data that is not a leap year)
start_date <- temp %>% 
  dplyr::filter(doy == start_win & year == max(year)) %>% # max(year) shouldn't be leap year -> how to specify that?
  dplyr::select("date") %>% 
  unique() 

start_date

## back-calculate end day of year of window
end_win <- range[1] - first_window_Qrubra$combos[1,8]  

## transform result to calender date (year is set to a year within the data that is not a leap year)
end_date <- temp %>% 
  dplyr::filter(doy == end_win & year == max(year)) %>% # see question above (l. 90)
  dplyr::select("date") %>% 
  unique()

end_date

# VI. Check significance of model: Quercus rubra -----------------------------------------
# To account for over fitting and to check whether the selected window has been selected by chance, 
# a randomization test on that window is performed. 

first_window_Qrubra_rand <- climwin::randwin(repeats = 20, 
                                      baseline = lm(DOY ~ year, data = budburst_Qrubra),
                                      xvar = list(Temp = temp$temperature),
                                      cdate = temp$factor_date, 
                                      bdate = budburst_Qrubra$date,
                                      type = "absolute", 
                                      refday = refday,
                                      spatial = list(budburst_Qrubra$locID, temp$locID),
                                      range = range, 
                                      func = "lin", 
                                      stat = "mean")

# get the p-value of the randomization test
climwin::pvalue(dataset = first_window_Qrubra[[1]]$Dataset, 
                datasetrand = first_window_Qrubra_rand[[1]],
                metric = "C", 
                sample.size = length(unique(budburst_Qrubra$year)))
