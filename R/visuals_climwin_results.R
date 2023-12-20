# Visualization of climwin results ####

# Author: Cherine Jantzen
# Created: 19/12/2023
# Last updated: 20/12/2023


# load packages
library(dplyr)
library(ggplot2)


# I. Plots integrated in climwin ------------------------------------------

# First window analysis Quercus robur: plot AIC heat map
climwin::plotdelta(first_window_Qrobur[[1]]$Dataset)
climwin::plotall(first_window_Qrobur[[1]]$Dataset)

# Second window analysis Quercus robur: plot AIC heat map
climwin::plotdelta(second_window_Qrobur[[1]]$Dataset)
climwin::plotall(second_window_Qrobur[[1]]$Dataset)

# First window analysis Quercus rubra: plot AIC heat map
climwin::plotdelta(first_window_Qrubra[[1]]$Dataset)
climwin::plotall(first_window_Qrubra[[1]]$Dataset)


# II. Compare model output with observed values --------------------------

# get temperatures for selected window
mean_temp_in_window <- temp %>% 
  dplyr::filter(dummy > (lubridate::month(start_date$date) * 100 + lubridate::day(start_date$date)) & 
                  dummy < (lubridate::month(end_date$date) * 100 + lubridate::day(end_date$date))) %>%
  dplyr::summarise(mean_temp = mean(temperature, na.rm = TRUE), 
                   .by = c("year", "locID"))

# get the model coefficients
wind_coef_Qrobur <- coef(summary(first_window_Qrobur[[1]]$BestModel)) 

# calculate predicted bud burst dates based on model coefficients
mean_temp_in_window$pred.bb_date <- (wind_coef_Qrobur[1,1] + wind_coef_Qrobur[2,1] * mean_temp_in_window$year) + (wind_coef_Qrobur[3,1] * mean_temp_in_window$mean_temp)

# calculate mean annual bud burst date 
obs_pred_bb_Qrobur <- budburst_Qrobur %>% 
  summarise(mean_annual_bb_date = mean(avg_bud_burst_DOY), 
            .by = c("year", "locID")) %>% 
  right_join(mean_temp_in_window, by = c("year", "locID"))

# plot predicted against observed annual bud burst dates 
ggplot2::ggplot(obs_pred_bb_Qrobur, aes(mean_annual_bb_date, pred.bb_date,  colour = locID)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_smooth(aes(colour = locID), method = "lm", se = FALSE) +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Predicted & observed bud burst dates of Q. robur", 
                subtitle = "Climate window: 04/04 to 08/05",
                y = "predicted bud burst date [day of the year]",
                x = "observed annual bud burst date [day of the year]") +
  ggplot2::xlim(90, 130) +
  ggplot2::ylim(90, 130) 

