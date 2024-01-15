
# Forecasting of bud burst dates based on climate scenarios ####

# Author: Cherine Jantzen
# Created: 08/01/2024
# Updated: 15/01/2024

# I. Preparation ----------------------------------------------------------

# load(hindcast_bb_zScore, file = here::here("data", "hindcast_budburst_zScore.rda"))
# load(hindcast_bb, file = here::here("data", "hindcast_budburst.rda"))
# load(KNMI_temp_zScore, file = here::here("data", "KNMI_temp_zScore.rda"))
# load(KNMI_temp, file = here::here("data", "KNMI_temp.rda"))

# II. Function: Forecasting ---------------------------------------------------------

# Arguments
# scenario_temp: Formatted scenario temperature data as given by in the output list of function "scenario_hindcast_budburst".
# start_year: Year as numeric specifying the year in which to start forecasting.
# linear_model: Linear model used in function 'model_budburst_measuredTemp' to model bud burst against the measured temperatures. Used as the basis for the predictions of future bud burst dates. 
# scenario_name: Character specifying the name of the scenario the function is used on to annotate the data (e.g., "RCP 8.5")
# use_zScores: ...?


forecasting_budburst <- function(scenario_temp,
                                 linear_model,
                                 use_zScores = c("yes", "no")){
  
  model_coefficients_bb_temp_zScore <- linear_model$coefficients
  vcov_bb_temp_zScore <- vcov(linear_model)
  
  forecasted_budburst_smoothend <- NULL
  forecasted_budburst <- NULL
  
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
    
    forecasted_budburst <- rbind(forecasted_budburst, df1)
    
  
  # 11 year sliding-window to smooth forecasting  
    df2 <- df1 %>% dplyr::distinct(year, .keep_all = TRUE)
    
    forecasted_budburst_smoothend_run <- NULL
  
    for (s in (min(df2$year) + 5) : (max(df2$year) - 5)) {
      
      Data_s <- df2 %>% dplyr::filter(year >= (s - 5) & year <= (s + 5))
      
      Data_new <- data.frame(year = s, 
                             #scenario_name = unique(df2$scenario_name), 
                             run = r, 
                             mean_pred_bb_window = mean(Data_s$predicted_bb_date), 
                             sd = sd(Data_s$predicted_bb_date),
                             error = (qnorm(0.95) * sd(Data_s$predicted_bb_date))/sqrt(nrow(Data_s)))
      
      Data_new <- Data_new[complete.cases(Data_new), ]
      
      forecasted_budburst_smoothend_run <- rbind(forecasted_budburst_smoothend_run, Data_new)
    }
    
    forecasted_budburst_smoothend <- rbind(forecasted_budburst_smoothend, forecasted_budburst_smoothend_run)
  
  }
  
  # ADD BIND
  
  return(tibble::lst(forecasted_budburst, forecasted_budburst_smoothend))
  
} 



# III. Forecast bud burst dates -------------------------------------------

# use zScores
future_budburst_RCP8.5_zScores <- forecasting_budburst(scenario_temp = Validation_RCP8.5_zScores$scenario_temp_fut,
                                                       linear_model = Validation_RCP8.5_zScores$model_for_prediction_zScore,
                                                       use_zScore = "yes")

future_budburst_RCP8.5_zScores$forecasted_budburst_smoothend %>%
  group_by(year) %>%
  summarise(mean = mean(mean_pred_bb_window, na.rm = TRUE),
            sd = sd(mean_pred_bb_window, na.rm = TRUE),
            error = (qnorm(0.95) * sd)/sqrt(n()),
            CI_lower = mean - error,
            CI_upper = mean + error) %>%
  ggplot2::ggplot() +
  ggplot2::geom_line(aes(y = mean, x = year), linewidth = 1) +
  ggplot2::geom_ribbon(aes(x = year, 
                           ymin = CI_lower, 
                           ymax = CI_upper), 
                       alpha = 0.2) +
  ggplot2::theme_classic()

# observed_predicted_bb <- Validation_RCP8.5_zScores$budburst_temp %>% 
#   dplyr::select("avg_budburst_date" = "avg_budburst_DOY_allLoc", "year") %>% 
#   dplyr::mutate(type = "measured",
#                 Con95Pred = NA) %>%
#   rbind(., future_budburst_RCP8.5_zScores$forecasted_budburst %>%
#           dplyr::select("predicted_bb_date", "Con95Pred", "year") %>%
#           dplyr::mutate("avg_budburst_date" = mean(predicted_bb_date), .by = "year") %>%
#           dplyr::mutate(type = "predicted") %>%
#           dplyr::select(!"predicted_bb_date"))
# 
# ggplot2::ggplot(observed_predicted_bb) +
#   ggplot2::geom_line(data = observed_predicted_bb %>% 
#                        dplyr::filter(year > 2023), 
#                      aes(y = avg_budburst_date, x = year), linewidth = 2) +
#   geom_point(data = observed_predicted_bb %>% 
#                dplyr::filter(year < 2024), 
#              aes(y = avg_budburst_date, x = year), size = 2) +
#   ggplot2::geom_ribbon(data = observed_predicted_bb %>% dplyr::filter(year > 2023), aes(x = year, 
#                            ymin = avg_budburst_date - Con95Pred, 
#                            ymax = avg_budburst_date + Con95Pred), 
#                        alpha = 0.2) +
#   ggplot2::theme_classic()




future_budburst_RCP8.5_zScores$forecasted_budburst %>% 
  summarise(mean_fit = mean(fit), .by = "year") %>%
  mutate(group = "predicted") %>% 
  rbind(., Validation_RCP8.5_zScores$budburst_temp %>% 
          select("mean_fit" = "avg_budburst_DOY_allLoc", "year") %>% 
          mutate(group = "measured")) %>% 
  ggplot(aes(y = mean_fit, x = year, colour = group)) +
  geom_point(size = 2)


# use mean temperatures
future_budburst_RCP8.5 <- forecasting_budburst(start_year = 2024,
                                               scenario_temp = hindcast_bb$scenario_temp,
                                               linear_model = KNMI_temp$model_bb_temp,
                                               scenario_name = hindcast_bb$scenario_name,
                                               use_zScore = "no")

future_budburst_RCP8.5$plot


