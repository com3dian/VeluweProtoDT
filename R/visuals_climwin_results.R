# Visualization of climwin results ####

# Author: Cherine Jantzen
# Created: 19/12/2023
# Last updated: 20/12/2023



# I. Load packages  -------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)


# II. Visualize output of climwin -----------------------------------------

## 1. Function to create two plots for the output of the climwin model ####
# Arguments
# biological_data: Tibble specifying the biological input data that was used for the climate model in function "find_climate_window()" . 
# window_output: Output list of function "find_climate_window()".


plot_climwin_output <- function(biological_data,
                       window_output){
 
  # create a heat map of model delta AICc values
  AIC_heatmap <- climwin::plotdelta(dataset = window_output$best_window[[1]]$Dataset, arrow = TRUE) +
    ggplot2::theme_classic() +
    ggplot2::labs(subtitle = biological_data$scientificName %>% unique())
  
  
  # get annual mean temperatures of the best window
  mean_temp_in_window <- temp %>% 
    dplyr::filter(dummy > (lubridate::month(window_output$start_date) * 100 + lubridate::day(window_output$start_date)) & 
                    dummy < (lubridate::month(window_output$end_date) * 100 + lubridate::day(window_output$end_date))) %>%
    dplyr::summarise(mean_temp = mean(temperature, na.rm = TRUE), 
                     .by = c("year", "locID"))
  
  # add mean temperatures to annual bud burst data
  annual_budburst_and_temp <- dplyr::left_join(biological_data,
                                               mean_temp_in_window %>%
                                                 dplyr::select("year", "mean_temp", "locID"), 
                                               by = c("year", "locID"))
  
  # plot annual mean temperatures of the best window against annual average bud burst dates
  plot_budburst_temperature<-  ggplot2::ggplot(annual_budburst_and_temp, aes(mean_temp, avg_bud_burst_DOY, colour = locID)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_smooth(method = "lm", se = F) +
    ggplot2::theme_classic() +
    ggplot2::labs(subtitle = biological_data$scientificName %>% unique(),
                  title = "Annual bud burst date ~ annual mean temperature in best window",
                  x = "Mean temperature [°C]",
                  y = "Annual mean bud burst date")

  # arrange both plots in one figure  
  ggpubr::ggarrange(AIC_heatmap,  plot_budburst_temperature)
  
}

## 2. Call function and create plots ####

Fig_Qrubra <- plot_climwin_output(biological_data = avg_annual_budburst_dates %>%
                           dplyr::filter(stringr::str_detect(scientificName, "Quercus robur")),
                         window_output = first_window_Qrobur)

Fig_Qrubra <- plot_climwin_output(biological_data = avg_annual_budburst_dates %>%
                           dplyr::filter(stringr::str_detect(scientificName, "Quercus rubra")),
                         window_output = first_window_Qrubra)


