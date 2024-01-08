
# Forecasting of bud burst dates based on climate scenarios ####

# Author: Cherine Jantzen
# Created: 08/01/2024


# I. Preparation ----------------------------------------------------------

# What data do we need from the previous script?
# Summary_scenarios <- read.csv(here::here("data", "Summary_scenarios.csv"))
# load(here::here("data", "model_budburst_temp.rda"))
# load(here::here("data", "model_budburst_zScore_temp.rda"))

# II. Forecasting ---------------------------------------------------------

# The prediction intervals are for a single observation at each case in newdata
# (or by default, the data used for the fit) with error variance(s) pred.var. This can be a multiple
# of res.var, the estimated value of σ^2: the default is to assume that future observations have the
# same error variance as those used for fitting.

## 1. Predictions based on zScore temperatures ####

future_temp <- Summary_scenarios %>% 
  dplyr::filter(year > 2023)

future_budburst_zScore <- NULL

for (f in 1:length(unique(future_temp$run))) {
  
  # filter per run
  df <- future_temp %>% dplyr::filter(run == f)
  
  # specify input data for predictions
  new <- data.frame(zScore = df$zScore)
  
  # predict bud burst dates based on linear model
  prediction <- predict(model_bb_KNMI_zScore, newdata = new, se.fit = TRUE, interval = "prediction")
  
  df1 <- data.frame(df, prediction$fit)
  future_budburst_zScore <- rbind(future_budburst_zScore, df1)
}


# get mean of predicted bud burst date per year 
Summary_future_budburst_zScore <- future_budburst_zScore %>% 
  dplyr::summarise(mean = mean(fit, na.rm = TRUE), 
                   sd = sd(fit, na.rm = TRUE), 
                   .by = "year")

# plot
Summary_future_budburst_zScore %>% 
  select(year, mean, sd) %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_errorbar(aes(x = year, ymin = mean - sd, ymax = mean + sd), width = 1) + 
  ggplot2::geom_point(aes(x = year, y = mean), shape = 21, color = "black", fill = "black", size = 5, alpha = 1) + 
  ggplot2::ylab("Predicted bud burst date over all runs (DOY)") + 
  ggplot2::xlab("Year") + 
  ggplot2::theme_classic() + 
  ggplot2::scale_y_continuous(limits = c(91, 130), breaks = seq(95, 130, by = 4), expand = c(0, 0)) + 
  ggplot2::scale_x_continuous(limits = c(2022, 2102), breaks = seq(2025, 2095, by = 10), expand = c(0, 0))


## 2. Predictions based on actual temperatures ####

future_budburst <- NULL

for (f in 1:length(unique(future_temp$run))) {
  
  # filter per run
  df <- future_temp %>% dplyr::filter(run == f)
  
  # specify input data for predictions
  new <- data.frame(mean_temperature = df$mean)
  
  # predictions of bud burst date based on actual temperatures
  prediction <- predict(model_bb_KNMI, newdata = new, se.fit = TRUE, interval = "prediction")
  
  df2 <- data.frame(df, prediction$fit)
  future_budburst <- rbind(future_budburst, df2)
}


# get mean predicted bud burst date
Summary_future_budburst <- future_budburst %>% 
  dplyr::summarise(mean = mean(fit, na.rm = TRUE), 
                   sd = sd(fit, na.rm = TRUE), 
                   .by = "year")

# plot
Summary_future_budburst %>% 
  select(year, mean, sd) %>% 
  ggplot2::ggplot() + 
  ggplot2::geom_errorbar(aes(x = year, ymin = mean - sd, ymax = mean + sd), width = 1) + 
  ggplot2::geom_point(aes(x = year, y = mean), shape = 21, color = "black", fill = "black", size = 5, alpha = 1) + 
  ggplot2::ylab("Predicted bud burst date over all runs (DOY)") + 
  ggplot2::xlab("Year") + 
  ggplot2::theme_classic() + 
  ggplot2::scale_y_continuous(limits = c(91, 130), breaks = seq(95, 130, by = 4), expand = c(0, 0)) + 
  ggplot2::scale_x_continuous(limits = c(2022, 2102), breaks = seq(2025, 2095, by = 10), expand = c(0, 0))

future_budburst %>% 
  dplyr::summarise(the_real_mean = mean(fit),
                   the_real_upr = mean(upr),
                   the_real_lwr = mean(lwr),
                   .by = "year") %>% 
  ggplot2::ggplot() +
  ggplot2::geom_line(aes(x = year, y = the_real_upr), color = "red") +
  ggplot2::geom_line(aes(x = year, y = the_real_lwr), color = "blue") +
  ggplot2::geom_line(aes(x = year, y = the_real_mean)) +
  #ggplot2::geom_point(aes(x = year, y = fit)) +
  ggplot2::theme_classic()

### 2.1. Plot predicted bud burst dates for all runs separately ####
# Plots <- NULL
# for (f in 1:length(unique(future_budburst$run))) {
#   
#   Data_for_plotting <- future_budburst %>% filter(run == f)
#   
#   Plot <- Data_for_plotting %>% 
#     select(year, fit, lwr, upr) %>% 
#     ggplot() + 
#     geom_errorbar(aes(x = year, 
#                       ymin = lwr, ymax = upr), width = 1) +
#     geom_point(aes(x = year, y = fit), fill = "black", 
#                shape = 21, color = "black", size = 1, alpha = 1) + ylab(paste("Predicted laying dates from run", f, sep = "")) + 
#     xlab("Year") + 
#     theme_classic() +
#     scale_y_continuous(limits = c(64, 135), breaks = seq(70, 130, 10), expand = c(0, 0)) + 
#     scale_x_continuous(limits = c(2019, 
#                                   2102), breaks = seq(2030, 2090, by = 20), expand = c(0, 0))
#   
#   
#   run <- paste("Run", f, sep = "")
#   assign(run, Plot)
#   Plots <- append(Plots, run, after = length(Plots))
# }
# 
# All_Plots <- cowplot::plot_grid(get(Plots[1]), get(Plots[2]), get(Plots[3]), get(Plots[4]), get(Plots[5]), get(Plots[6]), 
#                        get(Plots[7]), get(Plots[8]), get(Plots[9]), get(Plots[10]), get(Plots[11]), get(Plots[12]), get(Plots[13]), 
#                        get(Plots[14]), get(Plots[15]), get(Plots[16]), get(Plots[17]), get(Plots[18]), get(Plots[19]), get(Plots[20]), 
#                        get(Plots[21]), get(Plots[22]), get(Plots[23]), get(Plots[24]), get(Plots[25]), get(Plots[26]), get(Plots[27]), 
#                        get(Plots[28]), get(Plots[29]), get(Plots[30]), get(Plots[31]), get(Plots[32]), get(Plots[33]), get(Plots[34]), 
#                        get(Plots[35]), get(Plots[36]), get(Plots[37]), get(Plots[38]), get(Plots[39]), get(Plots[40]), scale = 0.9, 
#                        ncol = 6)
