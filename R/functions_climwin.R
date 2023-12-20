find_first_climate_window <- function(species,
                                      range,
                                      reference_day) {
  
  
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
  
  return(list(first_window, model_input, range, reference_day, start_date, end_date))
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