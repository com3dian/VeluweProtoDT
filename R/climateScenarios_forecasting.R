
# Forecasting of bud burst dates based on climate scenarios ####

# Author: Cherine Jantzen
# Created: 08/01/2024


# I. Preparation ----------------------------------------------------------

# What data do we need from the previous script?
# load(hindcast_bb_zScore, file = here::here("data", "hindcast_budburst_zScore.rda"))
# load(hindcast_bb, file = here::here("data", "hindcast_budburst.rda"))
# load(KNMI_temp_zScore, file = here::here("data", "KNMI_temp_zScore.rda"))
# load(KNMI_temp, file = here::here("data", "KNMI_temp.rda"))

# II. Function: Forecasting ---------------------------------------------------------

# The prediction intervals are for a single observation at each case in newdata
# (or by default, the data used for the fit) with error variance(s) pred.var. This can be a multiple
# of res.var, the estimated value of σ^2: the default is to assume that future observations have the
# same error variance as those used for fitting.

forecasting_budburst <- function(scenario_temp,
                                 start_year,
                                 linear_model,
                                 scenario_name,
                                 use_zScores){
  
  future_temp <- scenario_temp %>% 
    dplyr::filter(year > (start_year - 1))
  
  future_budburst <- NULL
  
  for (i in 1:length(unique(future_temp$run))) {
    
    # filter per run
    df <- future_temp %>% dplyr::filter(run == i)
    
    if(!(use_zScores == "yes")) {
      
      # specify input data for predictions
      new <- data.frame(mean_temperature = df$mean)
      
    } else {
      # specify input data for predictions
      new <- data.frame(zScore = df$zScore)
    }
    
    # predict bud burst dates based on linear model
    prediction <- predict(linear_model, newdata = new, se.fit = TRUE, interval = "prediction")
    
    df1 <- data.frame(df, prediction$fit)
    future_budburst <- rbind(future_budburst, df1)
  }
  
  # get mean of predicted bud burst date per year 
  Summary_future_budburst <- future_budburst %>% 
    dplyr::mutate(mean_predicted_bbdate = mean(fit, na.rm = TRUE), 
                  sd_predicted_bbdate = sd(fit, na.rm = TRUE),
                  min_lwr = min(lwr), # how to handle errors?
                  max_upr = max(upr),
                  .by = "year")
  
  # plot
  plot <- ggplot2::ggplot(Summary_future_budburst) + 
    ggplot2::geom_errorbar(aes(x = year, 
                               ymin = min_lwr, 
                               ymax = max_upr), width = 1) + 
    ggplot2::geom_point(aes(x = year, 
                            y = mean_predicted_bbdate), 
                        shape = 21, color = "black", fill = "black", size = 5, alpha = 1) + 
    ggplot2::labs(y = "Predicted bud burst date over all runs (DOY)", 
                  x = "Year", 
                  title = paste("Scenario", scenario_name, sep = " ")) + 
    ggplot2::theme_classic() + 
    ggplot2::scale_y_continuous(limits = c(min(Summary_future_budburst$lwr), max(Summary_future_budburst$upr))) + 
    ggplot2::scale_x_continuous(limits = c((min(Summary_future_budburst$year - 1)), (max(Summary_future_budburst$year) - 1)))
  
  
  return(tibble::lst(Summary_future_budburst, plot))
  
}


# III. Forecast bud burst dates -------------------------------------------

# use zScores
future_budburst_RCP8.5_zScores <- forecasting_budburst(start_year = 2024,
                                                       scenario_temp = hindcast_bb_zScore$scenario_temp,
                                                       linear_model = KNMI_temp_zScore$model_bb_temp_zScore,
                                                       scenario_name = hindcast_bb_zScore$scenario_name,
                                                       use_zScore = "yes")

future_budburst_RCP8.5_zScores$plot

# use mean temperatures
future_budburst_RCP8.5 <- forecasting_budburst(start_year = 2024,
                                               scenario_temp = hindcast_bb$scenario_temp,
                                               linear_model = KNMI_temp$model_bb_temp_zScore,
                                               scenario_name = hindcast_bb$scenario_name,
                                               use_zScore = "yes")

future_budburst_RCP8.5$plot



###############################################################################

## Alternative plot

# future_budburst_RCP8.5 %>% 
#   purrr::pluck("Summary_future_budburst") %>%
#   dplyr::summarise(the_real_mean = mean(fit),
#                    the_real_upr = mean(upr),
#                    the_real_lwr = mean(lwr),
#                    .by = "year") %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_line(aes(x = year, y = the_real_upr), color = "red") +
#   ggplot2::geom_line(aes(x = year, y = the_real_lwr), color = "blue") +
#   ggplot2::geom_line(aes(x = year, y = the_real_mean)) +
#   #ggplot2::geom_point(aes(x = year, y = fit)) +
#   ggplot2::theme_classic()

