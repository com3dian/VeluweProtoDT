find_first_climate_window <- function(biological_data,
                                      #species,
                                      range,
                                      reference_day) {
  
  
  # filter input data for species  
  #model_input  <- budburst %>% dplyr::filter(scientificName == species)
  
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
  
  # Ta
  reference_year <- dplyr::if_else(condition = lubridate::leap_year(max(temp$year)), 
                                   true = max(temp$year) - 1, 
                                   false = max(temp$year))
  
  start_date <- lubridate::make_date(year = reference_year, 
                                     month = reference_day[2], 
                                     day = reference_day[1]) - first_window$combos[1,]$WindowOpen
  
  end_date <- lubridate::make_date(year = reference_year, 
                                   month = reference_day[2], 
                                   day = reference_day[1]) - first_window$combos[1,]$WindowClose
  
  return(tibble::lst(first_window, model_input, range, reference_day, start_date, end_date))
  
}


# calculate windows for both species
first_window_Qrobur <- find_first_climate_window(species = 'Quercus robur L.',
                                                 reference_day = c(31, 5),
                                                 range = c(181, 0))

first_window_Qrubra <- find_first_climate_window(species = 'Quercus rubra L.',
                                                 reference_day = c(31, 5),
                                                 range = c(181, 0))




# III. Function for climwin randomization  --------------------------------

# Arguments
# - x: Output of find_first_climate_window() 

randomization_climate_window <- function(x,
                                         repeats = 20) {
  
  # Get components of x
  model_input <- x$model_input
  reference_day <- x$reference_day
  range <- x$range
  dataset <- x$first_window[[1]]$Dataset
  
  # 
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

randomization_climate_window(x = first_window_Qrobur)

# Function to find second window ------------------------------------------

function(x,
         repeats = 20) {
  # Get components of x 
  model_input <- x$model_input
  new_model_data <- x$first_window[[1]]$BestModelData 
  
  # Rename column 
  new_model_data$first_window <- new_model_data$climate 
  
  ## Slidingwin to find second window on data of first window ####
  second_window <- climwin::slidingwin(baseline = lm(yvar ~ first_window + year, data = new_model_data), 
                                       xvar = list(Temp = temp$temperature),
                                       cdate = temp$factor_date, 
                                       bdate = model_input$date,
                                       type = "absolute", 
                                       refday = refday,
                                       spatial = list(model_input$locID, temp$locID),
                                       range = range, 
                                       func = "lin", 
                                       stat = "mean")
  
  ## Randomization ####
  second_window_rand <- climwin::randwin(repeats = repeats,
                                         baseline = lm(yvar ~ first_window + year, data = new_model_data), 
                                         xvar = list(Temp = temp$temperature),
                                         cdate = temp$factor_date, 
                                         bdate = model_input$date,
                                         type = "absolute", 
                                         refday = refday,
                                         spatial = list(model_input$locID, temp$locID),
                                         range = range, 
                                         func = "lin", 
                                         stat = "mean")
  
  # p-value of randomization
  climwin::pvalue(dataset = second_window[[1]]$Dataset, 
                  datasetrand = second_window_rand[[1]],
                  metric = "C", 
                  sample.size = length(unique(model_input$year)))
  
}

# New functions -----------------------------------------------------------

find_climate_window <- function(biological_data = NULL,
                                range,
                                reference_day,
                                window_number = c("first", "second"),
                                first_window = NULL) {
  
  # Find 'first' or 'second' climate window
  if(window_number == "first") {
    
    if(is.null(biological_data)) {
      
      stop("If you want to find a first climate window, provide the biological data as `biological_data`.")
      
    }
    
    # Define baseline
    baseline <- lm(DOY ~ year, data = biological_data)
    
  } else if(window_number == "second") {
    
    # Error if first window output is not provided when searching for second window
    if(is.null(first_window)) {
      
      stop("If you want to find a second climate window, provide the output of the first iteration of `find_climate_window()` as `first_window`.")
      
    }
    
    biological_data <- first_window$biological_data
    
    # The first window is added as an explanatory variable to the baseline model
    baseline_data <- first_window$best_window[[1]]$BestModelData %>% 
      dplyr::rename("first_window" = "climate",
                    "DOY" = "yvar")
    
    # Define baseline
    baseline <- lm(DOY ~ year + first_window, data = baseline_data)
    
  }
  
  # Climwin analysis: Find window 
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
  
  # Ta
  reference_year <- dplyr::if_else(condition = lubridate::leap_year(max(temp$year)), 
                                   true = max(temp$year) - 1, 
                                   false = max(temp$year))
  
  start_date <- lubridate::make_date(year = reference_year, 
                                     month = reference_day[2], 
                                     day = reference_day[1]) - best_window$combos[1,]$WindowOpen
  
  end_date <- lubridate::make_date(year = reference_year, 
                                   month = reference_day[2], 
                                   day = reference_day[1]) - best_window$combos[1,]$WindowClose
  
  return(tibble::lst(best_window, biological_data, baseline, range, reference_day, start_date, end_date))
  
}

test <- find_climate_window(biological_data = budburst %>% dplyr::filter(scientificName == 'Quercus robur L.'),
                            reference_day = c(31, 5),
                            range = c(181, 0),
                            type = "first")

test2 <- find_climate_window(first_window = test,
                             reference_day = c(31, 5),
                             range = c(181, 0),
                             type = "second")

randomization_climate_window <- function(x,
                                         repeats = 20) {
  
  # Get components of x
  baseline <- x$baseline
  biological_data <- x$biological_data
  reference_day <- x$reference_day
  range <- x$range
  dataset <- x$best_window[[1]]$Dataset
  
  # 
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

randomization_climate_window(test, 1)
randomization_climate_window(test2, 1)
