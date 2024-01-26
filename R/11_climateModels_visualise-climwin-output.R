# Visualization of climwin results ####

# Author: Cherine Jantzen
# Created: 2023-12-19
# Last updated: 2024-01-26


# I. Load packages and data  ----------------------------------------------

library(dplyr)
library(ggplot2)
library(ggpubr)

# Load model output data 
load(here::here("data", "climwin_outputs_Qrobur.rda"))
load(here::here("data", "climwin_outputs_Qrubra.rda"))

colour_pal <- c("#48d3d3", "#FC8D59", "#D53E4F", "#FFD560", "#3288BD")

# II. Visualize output of climwin -----------------------------------------

## 1. Function to create two plots for the output of the climwin model ####
# Arguments
# - x: Output list of function "find_climate_window()".

plot_climwin_output <- function(x){

  # create a heat map of model delta AICc values
  AIC_heatmap <- climwin::plotdelta(dataset = x$best_window[[1]]$Dataset,
                                    arrow = TRUE) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.title.x = element_text(size = 15),
                   axis.title.y = element_text(size = 15),
                   axis.text.x = element_text(size = 15),
                   axis.text.y = element_text(size = 15),
                   title = element_text(size = 16),
                   legend.position = "bottom")

  # get annual mean temperatures of the best window
  mean_temp_in_window <- x$climate_data %>%
    dplyr::filter(dummy > (lubridate::month(x$start_date) * 100 + lubridate::day(x$start_date)) &
                    dummy < (lubridate::month(x$end_date) * 100 + lubridate::day(x$end_date))) %>%
    dplyr::summarise(mean_temp = mean(temperature, na.rm = TRUE),
                     .by = c("year", "locID"))

  # add mean temperatures to annual bud burst data
  annual_budburst_and_temp <- dplyr::left_join(x$biological_data,
                                               mean_temp_in_window %>%
                                                 dplyr::select("year", "mean_temp", "locID"),
                                               by = c("year", "locID"))

  # plot annual mean temperatures of the best window against annual average bud burst dates
  plot_budburst_temperature<-  ggplot2::ggplot(data = annual_budburst_and_temp,
                                               mapping = aes(x = mean_temp, y = avg_bud_burst_DOY, colour = locID)) +
    ggplot2::geom_point(size = 2, alpha = 0.4) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    ggplot2::theme_classic() +
    ggplot2::scale_colour_manual(values = colour_pal) +
    ggplot2::labs(title = "Bud burst date ~ mean temperature in best window",
                  x = "Annual mean temperature [°C]",
                  y = "Annual mean bud burst date",
                  colour = "Location (grid cell)") +
    ggplot2::theme(title = element_text(size = 16),
                   axis.title.x = element_text(size = 15),
                   axis.title.y = element_text(size = 15),
                   legend.title = element_text(size = 15),
                   axis.text.x = element_text(size = 15),
                   axis.text.y = element_text(size = 15),
                   legend.text = element_text(size = 13),
                   legend.position = "bottom")

  # arrange both plots in one figure
  ggpubr::ggarrange(AIC_heatmap,  plot_budburst_temperature, align = "hv")

}

## 2. Call function and create plots ####

Fig_Qrobur <- plot_climwin_output(first_window_Qrobur)
Fig_Qrobur
ggsave(Fig_Qrobur, filename = "P:/LTER-LIFE days/plot_climwin.png", units = "cm", width = 35, height = 20)
Fig_Qrubra <- plot_climwin_output(first_window_Qrubra)
Fig_Qrubra