# Visualization of climwin results ####

# Author: Cherine Jantzen
# Created: 19/12/2023


# load packages
library(dplyr)
library(ggplot2)


# I. Plots integrated in climwin ------------------------------------------

# First window analysis: plot AIC heat map
climwin::plotdelta(first_window[[1]]$Dataset)

# Second window analysis: plot AIC heat map
climwin::plotall(second_window[[1]]$Dataset)


# II. Compare model output with observed values --------------------------

# get temperatures for selected window
TempBB <- temp %>% 
  dplyr::filter(dummy > (lubridate::month(start_date$date) * 100 + lubridate::day(start_date$date)) & 
                  dummy < (lubridate::month(end_date$date) * 100 + lubridate::day(end_date$date)))

MeanTempBB <- TempBB %>% 
  dplyr::summarise(mean_temp = mean(temperature, na.rm = TRUE), 
                   .by = c("year", "locID"))

# get the model coefficients
wind_coef <- coef(summary(first_window[[1]]$BestModel)) 

# get equation describing the model & predict bud burst dates with that
MeanTempBB$pred.bb_date <- (wind_coef[1,1] + wind_coef[2,1] * MeanTempBB$year) + (wind_coef[3,1] * MeanTempBB$mean_temp)

# plot predicted against observed values 
obs_pred_bb <- db_2 %>% 
  summarise(mean_annual_bb_date = mean(avg_budburst_DOY), 
            .by = c("year", "locID")) %>% 
  right_join(MeanTempBB, by = c("year", "locID"))

ggplot2::ggplot(obs_pred_bb, aes(mean_annual_bb_date, pred.bb_date,  colour = locID)) +
  ggplot2::geom_point(size = 2) +
  ggplot2::geom_smooth(aes(colour = locID), method = "lm", se = FALSE) +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Predicted & observed bud burst dates", 
                subtitle = "Climate window: 04/04 to 08/05",
                y = "predicted bud burst date [day of the year]",
                x = "observed annual bud burst date [day of the year]") +
  ggplot2::xlim(90, 130) +
  ggplot2::ylim(90, 130) 

