
# Forecasting of bud burst dates based on climate scenarios ####

# Author: Cherine Jantzen
# Created: 08/01/2024


# I. Preparation ----------------------------------------------------------

# What data do we need from the previous script?
# Summary_scenarios
# model_bb_KNMI_zScore
# model_bb_KNMI


# II. Forecasting ---------------------------------------------------------

# The prediction intervals are for a single observation at each case in newdata
# (or by default, the data used for the fit) with error variance(s) pred.var. This can be a multiple
# of res.var, the estimated value of σ^2: the default is to assume that future observations have the
# same error variance as those used for fitting.

# 1. Predictions based on zScore temperatures ####

future_temp <- Summary_scenarios %>% 
  dplyr::filter(year > 2023)

future_budburst_zScore <- NULL

for (f in 1:length(unique(future_temp$run))) {
  
  # filter per run
  df <- future_temp %>% dplyr::filter(run == f)
  
  new <- data.frame(zScore = df$zScore)
  
  prediction <- predict(model_bb_KNMI_zScore, newdata = new, se.fit = TRUE, interval = "prediction")
  df1 <- data.frame(df, prediction$fit)
  future_budburst_zScore <- rbind(future_budburst_zScore, df1)
}


# get mean prediction
Summary_future_budburst_zScore <- future_budburst_zScore %>% 
  dplyr::summarise(mean = mean(fit, na.rm = TRUE), 
                   sd = sd(fit, na.rm = TRUE), 
                   .by = "year")

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


# 2. Predictions based on actual temperatures ####

future_budburst <- NULL

for (f in 1:length(unique(future_temp$run))) {
  
  # filter per run
  df <- future_temp %>% dplyr::filter(run == f)
  
  new <- data.frame(mean_temperature = df$mean)
  
  # predictions of bud burst date based on actual temperatures
  prediction <- predict(model_bb_KNMI, newdata = new, se.fit = TRUE, interval = "prediction")
  df2 <- data.frame(df, prediction$fit)
  future_budburst <- rbind(future_budburst, df2)
}


# get mean prediction
Summary_future_budburst <- future_budburst %>% 
  dplyr::summarise(mean = mean(fit, na.rm = TRUE), 
                   sd = sd(fit, na.rm = TRUE), 
                   .by = "year")

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

