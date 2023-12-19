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

# create numeric dates to be used in the baseline model
budburst <- budburst %>% 
  dplyr::mutate(formatted_date = as.Date(as.character(date), format = "%d/%m/%Y"),
                DOY = lubridate::yday(formatted_date))

# exclude location 7, as this labels trees without coordinates 
# and filter for species: Quercus robur
budburst_2 <- budburst %>% filter(date != "NA", 
                                  locID != "loc7", 
                                  scientificName == 'Quercus robur L.')


# III. Sliding window analysis --------------------------------------------

## 1. set parameters for sliding window model ####

# refday = reference day, before which windows are tested
refday <- c(31, 5)

# range = the range of days before the reference day, in which windows are tested
range <-  c(181, 0)


## 2. Climwin analysis: Find first window ####
first_window <- climwin::slidingwin(baseline = lm(DOY ~ year, data = budburst_2), 
                                    xvar = list(Temp = temp$temperature),
                                    cdate = temp$factor_date, 
                                    bdate = budburst_2$date,
                                    type = "absolute", 
                                    refday = refday,
                                    spatial = list(budburst_2$locID, temp$locID),
                                    range = range, 
                                    func = "lin", 
                                    stat = "mean")


# best model window results
first_window$combos


## 3. Calculate calender dates of the first window ####

## back-calculate start day of year of window
start_win <- range[1] - first_window$combos[1,7]

## transform result to calender date (year is set randomly to a year within  the data that is not a leap year)
start_date <- temp %>% 
  dplyr::filter(doy == start_win & year == max(year)) %>% # max(year) shouldn't be leap year -> how to specify that?
  dplyr::select("date") %>% 
  unique() 

start_date

## back-calculate end day of year of window (subtract 31 days of December for correct calender date)
end_win <- range[1] - first_window$combos[1,8] - 31 

## transform result to calender date (year is set to a year within the data that is not a leap year)
end_date <- temp %>% 
  dplyr::filter(doy == end_win & year == max(year)) %>% # see question above (l. 90)
  dplyr::select("date") %>% 
  unique()

end_date

# VI. Check significance of model -----------------------------------------
# To account for over fitting and to check whether the selected window has been selected by chance, 
# a randomization test on that window is performed. 

first_window_rand <- climwin::randwin(repeats = 20, 
                                      baseline = lm(DOY ~ year, data = budburst_2),
                                      xvar = list(Temp = temp$temperature),
                                      cdate = temp$factor_date, 
                                      bdate = budburst_2$date,
                                      type = "absolute", 
                                      refday = refday,
                                      spatial = list(budburst_2$locID, temp$locID),
                                      range = range, 
                                      func = "lin", 
                                      stat = "mean")

# get the p-value of the randomization test
climwin::pvalue(dataset = first_window[[1]]$Dataset, 
                datasetrand = first_window_rand[[1]],
                metric = "C", 
                sample.size = length(unique(budburst_2$year)))


# V. Check for a second window ---------------------------------------------------------
# There sometimes can be a second climate window that fits the data. To test for that, the data of the 
# best window (i.e., first window) is taken as the input data for the same model as before.

# save data of best model of first window for use as new data for the second window
new_model_data <- first_window[[1]]$BestModelData 

# Rename column 
new_model_data$first_window <- new_model_data$climate 

## 1. slidingwin to find second window on data of first window ####
second_window <- climwin::slidingwin(baseline = lm(yvar ~ first_window + year, data = new_model_data), 
                                     xvar = list(Temp = temp$temperature),
                                     cdate = temp$factor_date, 
                                     bdate = budburst_2$date,
                                     type = "absolute", 
                                     refday = refday,
                                     spatial = list(budburst_2$locID, temp$locID),
                                     range = range, 
                                     func = "lin", 
                                     stat = "mean")

# give model results
second_window$combos

## 2. Randomization ####
# slidingwin to find second window on data of first window
second_window_rand <- climwin::randwin(repeats = 20,
                                       baseline = lm(yvar ~ first_window + year, data = new_model_data), 
                                       xvar = list(Temp = temp$temperature),
                                       cdate = temp$factor_date, 
                                       bdate = budburst_2$date,
                                       type = "absolute", 
                                       refday = refday,
                                       spatial = list(budburst_2$locID, temp$locID),
                                       range = range, 
                                       func = "lin", 
                                       stat = "mean")

# p-value of randomization
climwin::pvalue(dataset = second_window[[1]]$Dataset, 
                datasetrand = second_window_rand[[1]],
                metric = "C", 
                sample.size = length(unique(budburst_2$year)))
