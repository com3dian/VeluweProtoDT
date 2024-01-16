
# Forecasting of bud burst dates based on climate scenarios ####

# Author: Cherine Jantzen
# Created: 08/01/2024
# Updated: 16/01/2024

# I. Preparation ----------------------------------------------------------

# load(here::here("data", "Validation_RCP4.5_zScores.rda"))
# load(file = here::here("data", "Validation_RCP8.5_zScores.rda"))
# load(file = here::here("data", "Validation_1pt5degC_OS_zScores.rda"))
# load(file = here::here("data", "Validation_2pt0degC_zScores.rda"))
# load(file = here::here("data", "Validation_1pt5degC_zScores.rda"))

# set colour palette
scenario_colours <- c("RCP45" = "darkblue", "RCP85" = "orange", "1pt5degC" = "green", 
                      "1pt5degC_OS" = "blue", "2pt0degC" = "#294C60", "measured" = "#820933")

# II. Function: Forecasting ---------------------------------------------------------

# Arguments
# scenario_temp: Formatted scenario temperature data as given by in the output list of function "scenario_hindcast_budburst".
# linear_model: Linear model used in function 'model_budburst_measuredTemp' to model bud burst against the measured temperatures. Used as the basis for the predictions of future bud burst dates. 
# use_zScores: yes or no, specifying whether the zScores of temperatures should be used to model the biological variable or the actual yearly mean temperatures


forecasting_budburst <- function(scenario_temp,
                                 linear_model,
                                 use_zScores = c("yes", "no")){
  
  if(use_zScores == "yes"){
    
    model_coefficients_bb_temp_zScore <- linear_model$coefficients
    vcov_bb_temp_zScore <- vcov(linear_model)
    
    forecasted_budburst_perRun <- NULL
    forecasted_budburst_smoothend <- NULL
    
    for (r in unique(scenario_temp$run)) {
      
      df <- scenario_temp %>%
        dplyr::filter(run == r)
      
      X <- model.matrix(~ zScore, data = df)
      predicted <- X %*% model_coefficients_bb_temp_zScore
      varPred <- diag(X %*% vcov_bb_temp_zScore %*% t(X))
      sePred <- sqrt(varPred)
      
      df1 <- data.frame(df, 
                        predicted_bb_date = predicted, 
                        varPred = varPred,
                        sePred = sePred)
      
      df1$Con95Pred <- 1.96 * df1$sePred
      
      forecasted_budburst_perRun <- rbind(forecasted_budburst_perRun, df1)
      
      
      # 11 year sliding-window to smooth forecasting  
      df2 <- df1 %>% dplyr::distinct(year, .keep_all = TRUE)
      
      forecasted_budburst_smoothend_window <- NULL
      
      for (s in (min(df2$year) + 5) : (max(df2$year) - 5)) {
        
        Data_s <- df2 %>% dplyr::filter(year >= (s - 5) & year <= (s + 5))
        
        Data_new <- data.frame(year = s, 
                               scenario_name = unique(df2$scenario_name), 
                               run = r, 
                               mean_pred_bb_window = mean(Data_s$predicted_bb_date), 
                               sd = sd(Data_s$predicted_bb_date),
                               error = (qnorm(0.95) * sd(Data_s$predicted_bb_date))/sqrt(nrow(Data_s)))
        
        Data_new <- Data_new[complete.cases(Data_new), ]
        
        forecasted_budburst_smoothend_window <- rbind(forecasted_budburst_smoothend_window, Data_new)
      }
      
      forecasted_budburst_smoothend <- rbind(forecasted_budburst_smoothend, forecasted_budburst_smoothend_window) 
  }
  
    return(tibble::lst(forecasted_budburst_perRun, forecasted_budburst_smoothend))
    
  } else {
    
    # use mean temperatures
    
    model_coefficients_bb_temp <- linear_model$coefficients
    vcov_bb_temp <- vcov(linear_model)
    
    forecasted_budburst_perRun <- NULL
    forecasted_budburst_smoothend <- NULL
    
    for (r in unique(scenario_temp$run)) {
      
      df <- scenario_temp %>%
        dplyr::filter(run == r)
      
      X <- model.matrix(~ mean_temperature, data = df)
      predicted <- X %*% model_coefficients_bb_temp
      varPred <- diag(X %*% vcov_bb_temp %*% t(X))
      sePred <- sqrt(varPred)
      
      df1 <- data.frame(df, 
                        predicted_bb_date = predicted, 
                        varPred = varPred,
                        sePred = sePred)
      
      df1$Con95Pred <- 1.96 * df1$sePred
      
      forecasted_budburst_perRun <- rbind(forecasted_budburst_perRun, df1)
      
      
      # 11 year sliding-window to smooth forecasting  
      df2 <- df1 %>% dplyr::distinct(year, .keep_all = TRUE)
      
      forecasted_budburst_smoothend_window <- NULL
      
      for (s in (min(df2$year) + 5) : (max(df2$year) - 5)) {
        
        Data_s <- df2 %>% dplyr::filter(year >= (s - 5) & year <= (s + 5))
        
        Data_new <- data.frame(year = s, 
                               scenario_name = unique(df2$scenario_name), 
                               run = r, 
                               mean_pred_bb_window = mean(Data_s$predicted_bb_date), 
                               sd = sd(Data_s$predicted_bb_date),
                               error = (qnorm(0.95) * sd(Data_s$predicted_bb_date))/sqrt(nrow(Data_s)))
        
        Data_new <- Data_new[complete.cases(Data_new), ]
        
        forecasted_budburst_smoothend_window <- rbind(forecasted_budburst_smoothend_window, Data_new)
      }
      
      forecasted_budburst_smoothend <- rbind(forecasted_budburst_smoothend, forecasted_budburst_smoothend_window) 
    }
    
    return(tibble::lst(forecasted_budburst_perRun, forecasted_budburst_smoothend))
    
  }

}


# III. Forecast bud burst dates -------------------------------------------

# use zScores

# RCP 4.5
future_budburst_RCP4.5_zScores <- forecasting_budburst(scenario_temp = Validation_RCP4.5_zScores$scenario_temp_fut,
                                                       linear_model = Validation_RCP4.5_zScores$model_for_prediction,
                                                       use_zScore = "yes")

# RCP 8.5
future_budburst_RCP8.5_zScores <- forecasting_budburst(scenario_temp = Validation_RCP8.5_zScores$scenario_temp_fut,
                                                       linear_model = Validation_RCP8.5_zScores$model_for_prediction,
                                                       use_zScore = "yes")
# 1.5°C
future_budburst_1pt5degC_zScores <- forecasting_budburst(scenario_temp = Validation_1pt5degC_zScores$scenario_temp_fut,
                                                       linear_model = Validation_1pt5degC_zScores$model_for_prediction,
                                                       use_zScore = "yes")
# 1.5°C OS
future_budburst_1pt5degC_OS_zScores <- forecasting_budburst(scenario_temp = Validation_1pt5degC_OS_zScores$scenario_temp_fut,
                                                         linear_model = Validation_1pt5degC_OS_zScores$model_for_prediction,
                                                         use_zScore = "yes")

# 2.0°C
future_budburst_2pt0degC_zScores <- forecasting_budburst(scenario_temp = Validation_2pt0degC_zScores$scenario_temp_fut,
                                                         linear_model = Validation_2pt0degC_zScores$model_for_prediction,
                                                         use_zScore = "yes")

# combine smoothend predictions of all scenarios
Forecasting_all <- rbind(future_budburst_RCP4.5_zScores$forecasted_budburst_smoothend,
                         future_budburst_RCP8.5_zScores$forecasted_budburst_smoothend,
                         future_budburst_1pt5degC_zScores$forecasted_budburst_smoothend,
                         future_budburst_1pt5degC_OS_zScores$forecasted_budburst_smoothend,
                         future_budburst_2pt0degC_zScores$forecasted_budburst_smoothend)

# plot predicted bud burst of each scenario 
Forecasting_all %>%
  group_by(year, scenario_name) %>%
  summarise(mean = mean(mean_pred_bb_window, na.rm = TRUE),
            sd = sd(mean_pred_bb_window, na.rm = TRUE),
            error = (qnorm(0.95) * sd)/sqrt(n()),
            CI_lower = mean - error,
            CI_upper = mean + error) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(aes(y = mean, x = year, colour = scenario_name), linewidth = 2) +
  ggplot2::geom_ribbon(aes(x = year, 
                           ymin = CI_lower, 
                           ymax = CI_upper, fill = scenario_name), 
                       alpha = 0.1) +
  scale_color_manual(values = scenario_colours) + 
  scale_fill_manual(values = scenario_colours) +
  ggplot2::theme_classic() +
  ggplot2::labs(x = "Year",
                y = "Predicted bud burst date (mean over scenario runs)", 
                title = "Predicted bud burst dates of the future", 
                colour = "Scenario", 
                fill = "Scenario") +
  ggplot2::theme(legend.position = "bottom")


