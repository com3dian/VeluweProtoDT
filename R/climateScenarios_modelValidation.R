# Hindcasting and validation of climate scenarios ####

# Author: Cherine Jantzen
# Created: 05/01/2024
# Updated: 10/01/2024

# I. Preparation ----------------------------------------------------------

# load packages
library(lubridate)
library(dplyr)
library(ggplot2)

# load data
temp <- read.csv(here::here("data", "temp_climwin_input.csv"))
avg_annual_budburst_dates <- read.csv(here::here("data","budburst_climwin_input.csv"))
climwin_QRobur <- load(here::here("data", "climwin_outputs_Qrobur.rda"))


# II. Function 1: Format measured temperature data & model bud burst --------

# Arguments
# temperature_data: data frame/tibble specifying the measured temperatures that were used as an input for the climate window analysis
# climwin_output: output from the climate window analysis of the sensitivity window .....?
# biological_data: data frame/tibble specifying the biological input data (as used for the climate window analysis)
# use_zScores: ....? specifying whether the zScores of temperatures should be used to model the biological variable or the actual yearly mean temperatures 

model_budburst_measuredTemp <- function(temperature_data,
                                        climwin_output,
                                        biological_data,
                                        use_zScores,
                                        number_simulations) {
  
  ## format temperature data and filter for bud burst sensitive period
  measured_temp <- temperature_data %>%
    dplyr::mutate(date = lubridate::as_date(date),
                  year = lubridate::year(date),
                  month = lubridate::month(date),
                  day = lubridate::day(date),
                  doy = lubridate::yday(date),
                  # Create dummy for filtering window later. Format: 312 = March 12, 401 = April 1
                  dummy = month * 100 + day) %>%
    ### filter for the calculated window of climwin
    dplyr::filter(dummy > (lubridate::month(climwin_output$start_date) * 100 + lubridate::day(climwin_output$start_date)) &
                    dummy < (lubridate::month(climwin_output$end_date) * 100 + lubridate::day(climwin_output$end_date))) %>%
    ### get mean temperature per day and year over all locations
    dplyr::summarise("mean_temperature" = mean(temperature),
                     "sd_KNMI_temp" = sd(temperature, na.rm = TRUE),
                     .by = "year") %>% 
    dplyr::mutate(overall_mean = mean(mean_temperature),
                  overall_sd = sd(mean_temperature),
                  # annotate data
                  type = rep("measured", nrow(.)),
                  scenario = rep(NA, nrow(.)),
                  run = rep(NA, nrow(.)),
                  # z-scores as (mean of year x - mean over years)/sd of yearly means
                  zScore = (mean_temperature - overall_mean) / overall_sd) %>%
    dplyr::select(!c("overall_mean", "overall_sd"))
  
  
  ## get annual mean bud burst date
  avg_budburst <- biological_data  %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::summarise(avg_budburst_DOY_allLoc = mean(avg_bud_burst_DOY, na.rm = TRUE), 
                     .by = "year")
  
  ## add bud burst to temperature
  budburst_temp <- measured_temp %>%
    dplyr::left_join(avg_budburst, by = "year")
  
  ## model bud burst with measured KNMI temperatures
  if(!(use_zScores == "yes")) {
    
    ### model observed bud burst with mean measured temperatures 
    model_bb_temp <- lm(avg_budburst_DOY_allLoc ~ mean_temperature, data = budburst_temp)
    intercept_bb_temp <- as.numeric(model_bb_temp$coefficients[1])
    slope_bb_temp <- as.numeric(model_bb_temp$coefficients[2])
    sigma_bb_temp <- sigma(model_bb_temp)
    model_coefficients_bb_temp <- model_bb_temp$coefficients
    vcov_bb_temp <- vcov(model_bb_temp)
    
    # simulations
    sim_scenario_slope_pred_year <- NULL
    
    for (s in 1:number_simulations) {
      
        # get data to predict on 
        new_data <- data.frame(mean_temperature = budburst_temp$mean_temperature)
        
        Error <- rnorm(n = nrow(new_data), sd = sigma_bb_temp)
        predicted_bb_date <- intercept_bb_temp + slope_bb_temp * new_data$mean_temperature + Error
        predicted_budburst <- data.frame(budburst_temp, predicted_bb_date)
        
        # get slope
        model_pred_year <- lm(predicted_bb_date ~ year, data = predicted_budburst)
        slope_pred_year <- as.numeric(model_pred_year$coefficients[2])
        df_slope_pred_year <- data.frame(scenario = "measured", run = NA, sim = paste0("sim_", s), slope = slope_pred_year)
        
       sim_scenario_slope_pred_year <- rbind(sim_scenario_slope_pred_year, df_slope_pred_year)
    }
    
    
    
    # ### residuals of model
    # residual_fitted <- dplyr::bind_cols(model_bb_temp$residuals, model_bb_temp$fitted.values) %>%
    #   dplyr::rename("residuals" = "...1", "fitted" = "...2") %>% 
    #   ggplot2::ggplot(aes(x = fitted, y = residuals)) + 
    #   geom_point() + 
    #   theme_classic() 
    
    ### plot observed bud burst dates against mean temperatures
    plot <-  ggplot2::ggplot() +
      ggplot2::geom_abline(intercept = intercept_bb_temp, 
                           slope = slope_bb_temp, 
                           linetype = 1, linewidth = 1, color = "black") +
      ggplot2::geom_point(data = budburst_temp, aes(x = mean_temperature, 
                                                    y = avg_budburst_DOY_allLoc), 
                          color = "black", size = 3) +
      ggplot2::ylab("Bud burst date (DOY)") +
      ggplot2::xlab("mean measured temperature") +
      ggplot2::theme_classic()
    
    return(tibble::lst(avg_budburst, budburst_temp, model_bb_temp, sim_measured_slope_pred_year, plot))
    
  } else {
    
    ### model observed bud burst with zScores of measured temperatures
    model_bb_temp_zScore <- lm(avg_budburst_DOY_allLoc ~ zScore, data = budburst_temp)
    intercept_bb_temp_zScore <- as.numeric(model_bb_temp_zScore$coefficients[1])
    slope_bb_temp_zScore <- as.numeric(model_bb_temp_zScore$coefficients[2])
    sigma_bb_temp_zScore <- sigma(model_bb_temp_zScore)
    model_coefficients_bb_temp_zScore <- model_bb_temp_zScore$coefficients
    vcov_bb_temp_zScore <- vcov(model_bb_temp_zScore)
    
    sim_measured_slope_pred_year <- NULL
    
    for (s in 1:number_simulations) {
      
        # get data to predict on 
        new_data <- data.frame(zScore = budburst_temp$zScore)
        
        Error <- rnorm(n = nrow(new_data), sd = sigma_bb_temp_zScore)
        predicted_bb_date <- intercept_bb_temp_zScore + slope_bb_temp_zScore * new_data$zScore + Error
        predicted_budburst <- data.frame(budburst_temp, predicted_bb_date)
        
        # get slope
        model_pred_year <- lm(predicted_bb_date ~ year, data = predicted_budburst)
        slope_pred_year <- as.numeric(model_pred_year$coefficients[2])
        df_slope_pred_year <- data.frame(scenario = "measured", run = NA, sim = paste0("sim_", s), slope = slope_pred_year)
        
       
      sim_measured_slope_pred_year <- rbind(sim_measured_slope_pred_year, df_slope_pred_year)
    }
    
    
    # ### residuals of model
    # residual_fitted <- dplyr::bind_cols(model_bb_temp_zScore$residuals, model_bb_temp_zScore$fitted.values) %>%
    #   dplyr::rename("residuals" = "...1", "fitted" = "...2") %>% 
    #   ggplot2::ggplot(aes(x = fitted, y = residuals)) + 
    #   geom_point() + 
    #   theme_classic() 
    
    ### plot observed bud burst dates against zScores of temperature
    plot_zScore <-  ggplot2::ggplot() +
      ggplot2::geom_abline(intercept = intercept_bb_temp_zScore, slope = slope_bb_temp_zScore, 
                           linetype = 1, linewidth = 1, color = "black") +
      ggplot2::geom_point(data = budburst_temp, aes(x = zScore, y = avg_budburst_DOY_allLoc), color = "black", size = 3) +
      ggplot2::ylab("Bud burst date (DOY)") +
      ggplot2::xlab("zScore measured temperatures") +
      ggplot2::theme_classic()
    
    return(tibble::lst(avg_budburst, budburst_temp, model_bb_temp_zScore, sim_measured_slope_pred_year, plot_zScore))
  }
}


# III. Function 2:  -------------------------------------------------------

# Arguments
# climwin_output: Output from the climate window analysis of the sensitivity window .....?
# scenario_name: Character specifying the name of the scenario the function is used on to annotate the data (e.g., "RCP 8.5")
# scenario_data: List of data frames, each containing one run of a scenario and providing temperature values per day  
# avg_biol_data: Data frame/tibble containing the yearly mean bud burst date and year
# linear_model: Linear model used in function 'model_budburst_measuredTemp' to model bud burst against the measured temperatures. Used as the basis for the predictions of bud burst dates. 
# use_zScores: ....? specifying whether the zScores of temperatures should be used to model the biological variable or the actual yearly mean temperatures


scenario_hindcast_budburst <- function(climwin_output,
                                       scenario_name,
                                       scenario_data,
                                       avg_biol_data,
                                       linear_model,
                                       use_zScores){
  
  ## loop through all files to format data
  scenario_temp <- NULL
  
  for(f in 1:length(scenario_data)) {
    
    df <- scenario_data[[f]]
    
    ### add year and day of year
    df <- df %>%
      dplyr::mutate(date = lubridate::as_date(time),
                    year = lubridate::year(date),
                    dummy = lubridate::month(date) * 100 + lubridate::day(date),
                    doy = lubridate::yday(date),
                    # add mean temperatures in degree Celsius
                    temp_degreesC = TREFHT - 273.15,
                    run = f) %>%
      dplyr::filter(dummy > (lubridate::month(climwin_output$start_date) * 100 + lubridate::day(climwin_output$start_date)) &
                      dummy < (lubridate::month(climwin_output$end_date) * 100 + lubridate::day(climwin_output$end_date))) %>%
      
      ### summarise and annotate data per run
      dplyr::summarise("mean_temperature" = mean(temp_degreesC, na.rm = TRUE), 
                       "sd_temperature" = sd(temp_degreesC, na.rm = TRUE),
                       .by = "year") %>%
      dplyr::mutate(type = rep("model", nrow(.)),
                    scenario = rep(scenario_name, nrow(.)),
                    run = rep(f, nrow(.)),
                    # get mean and standard deviation over all years
                    overall_mean = mean(mean_temperature),
                    overall_sd = sd(mean_temperature),
                    # z-scores as (mean of year x - mean over years)/sd of yearly means
                    zScore = (mean_temperature - overall_mean) / overall_sd)
    
    
    scenario_temp <- rbind(scenario_temp, df)
  }
  
  
  ## hindcasting
  scenario_temp_bbPeriod <- scenario_temp %>% 
    dplyr::filter(year > (min(avg_biol_data$year) - 1), year < (max(avg_biol_data$year) + 1))
  
  
  hindcast_budburst <- NULL
  
  for (i in 1:length(unique(scenario_temp_bbPeriod$run))) {
    
    data <- scenario_temp_bbPeriod %>% 
      dplyr::filter(run == i)
    
    if(!(use_zScores == "yes")) {
      
      new <- data.frame(mean_temperature = data$mean_temperature)
      
    } else {
      new <- data.frame(zScore = data$zScore)
    }
    
    ### predict bud burst dates 
    prediction <- predict(linear_model, newdata = new, se.fit = TRUE, interval = "prediction")
    df <- data.frame(data, prediction$fit)
    
    ### add data to data frames
    hindcast_budburst <- rbind(hindcast_budburst, df)
  }
  
  ## combine predicted bud burst dates with observed dates
  pred_obs_bb <- hindcast_budburst %>% 
    dplyr::select("predicted_bb_doy" = "fit", "year", "run") %>%
    left_join(avg_biol_data, by = "year")
  
  ## plot predicted against observed dates per run
  plot <- ggplot2::ggplot(pred_obs_bb, aes(avg_budburst_DOY_allLoc, predicted_bb_doy)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::labs(x = "Observed mean bud burst date [day of year]", 
                  y = "Predicted mean bud burst date [day of year]", 
                  title = "Predicted vs. observed bud burst dates per scenario run") +
    ggplot2::facet_wrap(~ run, labeller = label_both) +
    ggplot2::xlim((min(pred_obs_bb$avg_budburst_DOY_allLoc) - 2), (max(pred_obs_bb$avg_budburst_DOY_allLoc) + 2))
  
  
  return(tibble::lst(scenario_temp, pred_obs_bb, plot, scenario_name))
}


# IV. Use functions -------------------------------------------------------

## 1. Model bud burst based on measured temperatures ####
# with zScores of temperature
KNMI_temp_zScore <- model_budburst_measuredTemp(temperature_data = temp,
                                                biological_data = avg_annual_budburst_dates %>%
                                                  dplyr::filter(stringr::str_detect(scientificName, "Quercus robur")),
                                                climwin_output = first_window_Qrobur,
                                                use_zScores = "yes",
                                                number_simulations = 1000)
KNMI_temp_zScore$plot_zScore

# with mean temperatures
KNMI_temp <- model_budburst_measuredTemp(temperature_data = temp,
                                         biological_data = avg_annual_budburst_dates %>%
                                           dplyr::filter(stringr::str_detect(scientificName, "Quercus robur")),
                                         climwin_output = first_window_Qrobur,
                                         use_zScores = "no")
KNMI_temp$plot


## 2. Use scenario temperatures to hind-cast bud burst dates and compare with measured bud burst dates ####

## Get scenario data
# load and format scenario data
FileNames = list.files("P:/Fair data for DT/Temperature scenarios/Scenario_data") # to be changed to GitHub repo


# load each file
FileList <- lapply(paste("P:/Fair data for DT/Temperature scenarios/Scenario_data", FileNames, sep = "/"), # to be changed to GitHub repo
                   function(x) read.table(x, header = TRUE, sep = ","))

# use function 2
hindcast_bb_zScore <- scenario_hindcast_budburst(climwin_output = first_window_Qrobur,
                                                 scenario_name = "RCP_8.5",
                                                 scenario_data = FileList,
                                                 linear_model = KNMI_temp_zScore$model_bb_temp_zScore,
                                                 avg_biol_data = KNMI_temp_zScore$avg_budburst,
                                                 use_zScores = "yes")
# get plot
hindcast_bb_zScore$plot

# with mean temperatures
hindcast_bb <- scenario_hindcast_budburst(climwin_output = first_window_Qrobur,
                                          scenario_name = "RCP_8.5",
                                          scenario_data = FileList,
                                          linear_model = KNMI_temp$model_bb_temp,
                                          avg_biol_data = KNMI_temp$avg_budburst,
                                          use_zScores = "no")
# get plot
hindcast_bb$plot


# save output for forecasting script
# save(hindcast_bb_zScore, file = here::here("data", "hindcast_budburst_zScore.rda"))
# save(hindcast_bb, file = here::here("data", "hindcast_budburst.rda"))
# save(KNMI_temp_zScore, file = here::here("data", "KNMI_temp_zScore.rda"))
# save(KNMI_temp, file = here::here("data", "KNMI_temp.rda"))