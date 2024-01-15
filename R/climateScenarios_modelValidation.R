# Hindcasting and validation of climate scenarios ####

# Author: Cherine Jantzen
# Created: 05/01/2024
# Updated: 15/01/2024

# I. Preparation ----------------------------------------------------------

# load packages
library(lubridate)
library(dplyr)
library(ggplot2)

# load data
temp <- read.csv(here::here("data", "temp_climwin_input.csv"))
avg_annual_budburst_dates <- read.csv(here::here("data","budburst_climwin_input.csv"))
climwin_QRobur <- load(here::here("data", "climwin_outputs_Qrobur.rda"))
scenario_data_all <- read.csv(here::here("data", "scenario_temperatures.csv"))

# set seed 
set.seed(2804)

# II. Function 1: Format measured temperature data & model bud burst ------

# Arguments
# temperature_data: data frame/tibble specifying the measured temperatures that were used as an input for the climate window analysis
# climwin_output: output from the climate window analysis of the sensitivity window .....?
# biological_data: data frame/tibble specifying the biological input data (as used for the climate window analysis)
# use_zScores: ....? specifying whether the zScores of temperatures should be used to model the biological variable or the actual yearly mean temperatures 

model_validation <- function(measured_temperatures,
                             climwin_output,
                             biological_data,
                             use_zScores = c("yes", "no"),
                             number_simulations,
                             scenario_data,
                             scenario = c("1pt5degC, 1pt5degC_OS, 2pt0degC, RCP85, RCP45")) {
  
  ## 1. Measured temperatures: Formatting ####
  
  ## convert dates and filter for bud burst sensitive period
  measured_temp <- measured_temperatures %>%
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
  
  
  
  ## 2. Scenario temperatures: Formatting ####
  df <- scenario_data %>% dplyr::filter(scenario_name == scenario)
  
  ## add year and day of year
  scenario_temp <- df %>%
    dplyr::mutate(date = lubridate::as_date(time),
                  year = lubridate::year(date),
                  dummy = lubridate::month(date) * 100 + lubridate::day(date),
                  doy = lubridate::yday(date),
                  # add mean temperatures in degree Celsius
                  temp_degreesC = as.numeric(TREFHT) - 273.15) %>%
    dplyr::filter(dummy > (lubridate::month(climwin_output$start_date) * 100 + lubridate::day(climwin_output$start_date)) &
                    dummy < (lubridate::month(climwin_output$end_date) * 100 + lubridate::day(climwin_output$end_date))) %>%
    
    ## summarise and annotate data per run
    dplyr::mutate("mean_temperature" = mean(temp_degreesC, na.rm = TRUE), 
                  "sd_temperature" = sd(temp_degreesC, na.rm = TRUE),
                  .by = "year") %>%
    dplyr::mutate(type = rep("model", nrow(.)),
                  run = member_id)
  
  ## standardize scenario temperatures for historic and future period
  ### historic period
  scenario_temp_hist <- scenario_temp %>% 
    dplyr::filter(year >= min(biological_data$year), year <= max(biological_data$year)) %>%
    # get mean and standard deviation over all years
    dplyr::mutate(overall_mean_hist = mean(mean_temperature),
                  overall_sd_hist = sd(mean_temperature),
                  # z-scores as (mean of year x - mean over years)/sd of yearly means
                  zScore = (mean_temperature - overall_mean_hist) / overall_sd_hist)
  
  overall_mean_hist <- unique(scenario_temp_hist$overall_mean_hist)
  
  ### future period
  scenario_temp_fut <- scenario_temp %>%
    dplyr::filter(year > max(biological_data$year)) %>%
    # get mean and standard deviation over all years
    dplyr::mutate(overall_mean_fut = mean(mean_temperature),
                  overall_sd_fut = sd(mean_temperature),
                  # z-scores as (mean of year x - mean over years)/sd of yearly means
                  zScore = (mean_temperature - overall_mean_hist) / overall_sd_fut)
  
  
  ## 3. Biological data ####
  
  ## get annual mean bud burst date
  avg_budburst <- biological_data  %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::summarise(avg_budburst_DOY_allLoc = mean(avg_bud_burst_DOY, na.rm = TRUE), 
                     .by = "year")
  
  ## add bud burst to temperature
  budburst_temp <- measured_temp %>%
    dplyr::left_join(avg_budburst, by = "year")
  
  
  ## 4. Modelling ####
  
  # get slope of observed biological data ~ year
  m1 <- lm(avg_budburst_DOY_allLoc ~ year, data = budburst_temp)
  slope_bb_year <- m1$coefficients[2]
  
  ## 4.1 Modelling with mean temperatures ####
  
  if(!(use_zScores == "yes")) {
    
    ### model observed bud burst with mean measured temperatures 
    model_for_prediction <- lm(avg_budburst_DOY_allLoc ~ mean_temperature, data = budburst_temp)
    
    ### get model parameters
    intercept_bb_temp <- as.numeric(model_for_prediction$coefficients[1])
    slope_bb_temp <- as.numeric(model_for_prediction$coefficients[2])
    sigma_bb_temp <- sigma(model_for_prediction)
    model_coefficients_bb_temp <- model_for_prediction$coefficients
    vcov_bb_temp <- vcov(model_for_prediction)
    
    # Simulate prediction of biological data based on model_for_prediction for measured temperatures 
    sim_measured_slope_pred_year <- NULL
    
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
      
      sim_measured_slope_pred_year <- rbind(sim_measured_slope_pred_year, df_slope_pred_year)
    }
    
    # Simulate predictions of biological data based on model_for_prediction for scenario temperatures
    sim_scenario_slope_pred_year <- NULL
    
    for (s in 1:number_simulations) {
      
      scenario_slopes_pred_year <- NULL
      
      for (r in unique(scenario_temp_hist$run)) {
        
        # filter per run
        df <- scenario_temp_hist %>%
          dplyr::filter(run == r)
        
        # get data to predict on 
        new_data <- data.frame(mean_temperature = df$mean_temperature)
        
        Error <- rnorm(n = nrow(new_data), sd = sigma_bb_temp)
        predicted_bb_date <- intercept_bb_temp + slope_bb_temp * new_data$mean_temperature + Error
        predicted_budburst <- data.frame(df, predicted_bb_date)
        
        # get slope
        model_pred_year <- lm(predicted_bb_date ~ year, data = predicted_budburst)
        slope_pred_year <- as.numeric(model_pred_year$coefficients[2])
        df_slope_pred_year <- data.frame(scenario = scenario, run = r, sim = paste0("sim_", s), slope = slope_pred_year)
        
        # add data to data frames
        scenario_slopes_pred_year <- rbind(scenario_slopes_pred_year, df_slope_pred_year) 
      }
      
      sim_scenario_slope_pred_year <- rbind(sim_scenario_slope_pred_year, scenario_slopes_pred_year)
    }
    
    # combine simulated slopes for measured temperatures and scenario temperatures
    slopes_combined <- rbind(sim_measured_slope_pred_year, sim_scenario_slope_pred_year)
    
    # visually compare slopes
    plot_validation <- ggplot2::ggplot(slopes_combined) +
      ggplot2::geom_histogram(aes(y = ..density.., x = slope, fill = scenario), binwidth = 0.01) +
      scale_fill_brewer(palette = "Dark2") +  
      ggplot2::geom_vline(xintercept = slope_bb_year, linewidth = 2) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = "Slope (predicted bud burst ~ year)", y = "Density")
    
    ### plot observed bud burst dates against mean temperatures
    plot_obs_temp <-  ggplot2::ggplot() +
      ggplot2::geom_abline(intercept = intercept_bb_temp,
                           slope = slope_bb_temp,
                           linetype = 1, linewidth = 1, color = "black") +
      ggplot2::geom_point(data = budburst_temp, aes(x = mean_temperature,
                                                    y = avg_budburst_DOY_allLoc),
                          color = "black", size = 3) +
      ggplot2::ylab("Bud burst date (DOY)") +
      ggplot2::xlab("mean measured temperature") +
      ggplot2::theme_classic()

    return(tibble::lst(budburst_temp, scenario_temp_hist, scenario_temp_fut, model_for_prediction, plot_validation, plot_obs_temp))
    
  } else {
    
    ## 4.2. Modelling with zScores of temperature ####
    
    ### model observed bud burst with zScores of measured temperatures
    model_for_prediction_zScore <- lm(avg_budburst_DOY_allLoc ~ zScore, data = budburst_temp)
    
    ### get model parameter
    intercept_bb_temp_zScore <- as.numeric(model_for_prediction_zScore$coefficients[1])
    slope_bb_temp_zScore <- as.numeric(model_for_prediction_zScore$coefficients[2])
    sigma_bb_temp_zScore <- sigma(model_for_prediction_zScore)
    model_coefficients_bb_temp_zScore <- model_for_prediction_zScore$coefficients
    vcov_bb_temp_zScore <- vcov(model_for_prediction_zScore)
    
    # Simulate prediction of biological data based on model_for_prediction for measured temperatures 
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
    
    # Simulate predictions of biological data based on model_for_prediction for scenario temperatures
    sim_scenario_slope_pred_year <- NULL
    
    for (s in 1:number_simulations) {
      
      scenario_slopes_pred_year <- NULL
      
      for (r in unique(scenario_temp_hist$run)) {
        
        # filter per run
        df <- scenario_temp_hist %>%
          dplyr::filter(run == r)
        
        # get data to predict on 
        new_data <- data.frame(zScore = df$zScore)
        
        Error <- rnorm(n = nrow(new_data), sd = sigma_bb_temp_zScore)
        predicted_bb_date <- intercept_bb_temp_zScore + slope_bb_temp_zScore * new_data$zScore + Error
        predicted_budburst <- data.frame(df, predicted_bb_date)
        
        # get slope
        model_pred_year <- lm(predicted_bb_date ~ year, data = predicted_budburst)
        slope_pred_year <- as.numeric(model_pred_year$coefficients[2])
        df_slope_pred_year <- data.frame(scenario = scenario, run = r, sim = paste0("sim_", s), slope = slope_pred_year)
        
        # add data to data frames
        scenario_slopes_pred_year <- rbind(scenario_slopes_pred_year, df_slope_pred_year) 
      }
      
      sim_scenario_slope_pred_year <- rbind(sim_scenario_slope_pred_year, scenario_slopes_pred_year)
    }
    
    # combine simulated slopes for measured temperatures and scenario temperatures
    slopes_combined <- rbind(sim_measured_slope_pred_year, sim_scenario_slope_pred_year)
    
    # visually compare slopes
    plot_validation <- ggplot2::ggplot(slopes_combined) +
      ggplot2::geom_histogram(aes(y = ..density.., x = slope, fill = scenario), binwidth = 0.01) +
      scale_fill_brewer(palette = "Dark2") +  
      ggplot2::geom_vline(xintercept = slope_bb_year, linewidth = 2) +
      ggplot2::theme_classic() +
      ggplot2::labs(x = "Slope (predicted bud burst ~ year)", y = "Density")
    
    
    ### plot observed bud burst dates against zScores of temperature
    plot_zScore <-  ggplot2::ggplot() +
      ggplot2::geom_abline(intercept = intercept_bb_temp_zScore, slope = slope_bb_temp_zScore,
                           linetype = 1, linewidth = 1, color = "black") +
      ggplot2::geom_point(data = budburst_temp, aes(x = zScore, y = avg_budburst_DOY_allLoc), color = "black", size = 3) +
      ggplot2::ylab("Bud burst date (DOY)") +
      ggplot2::xlab("zScore measured temperatures") +
      ggplot2::theme_classic()
    
    return(tibble::lst(budburst_temp, scenario_temp_hist, scenario_temp_fut, model_for_prediction_zScore, plot_validation, plot_zScore))
  }
}

# IV. Use functions -------------------------------------------------------
# with zScores
Validation_RCP8.5_zScores <- model_validation(measured_temperatures = temp,
                                              climwin_output = first_window_Qrobur,
                                              biological_data = avg_annual_budburst_dates %>%
                                                dplyr::filter(stringr::str_detect(scientificName, "Quercus robur")),
                                              scenario = "RCP85",
                                              scenario_data = scenario_data_all,
                                              use_zScores = "yes",
                                              number_simulations = 20)


# Validation_all_scenarios <- purrr::map(.x = c("1pt5degC", "1pt5degC_OS", "2pt0degC", "RCP85", "RCP45"),
#                                        .f = ~{
#                                          output <- model_validation(measured_temperatures = temp,
#                                                                     climwin_output = first_window_Qrobur,
#                                                                     biological_data = avg_annual_budburst_dates %>%
#                                                                       dplyr::filter(stringr::str_detect(scientificName, "Quercus robur")),
#                                                                     scenario = .x,
#                                                                     scenario_data = scenario_data_all,
#                                                                     use_zScores = "yes",
#                                                                     number_simulations = 20)
#                                          return(output)
#                                        },
#                                        .progress = TRUE) %>%
#   purrr::list_c()

# with mean temperatures
Validation_RCP8.5 <- model_validation(measured_temperatures = temp,
                              climwin_output = first_window_Qrobur,
                              biological_data = avg_annual_budburst_dates %>%
                                dplyr::filter(stringr::str_detect(scientificName, "Quercus robur")),
                              scenario = "RCP85",
                              scenario_data = scenario_data_all,
                              use_zScores = "no",
                              number_simulations = 20)




# save output for forecasting script
# save(hindcast_bb_zScore, file = here::here("data", "hindcast_budburst_zScore.rda"))
# save(hindcast_bb, file = here::here("data", "hindcast_budburst.rda"))
# save(KNMI_temp_zScore, file = here::here("data", "KNMI_temp_zScore.rda"))
# save(KNMI_temp, file = here::here("data", "KNMI_temp.rda"))