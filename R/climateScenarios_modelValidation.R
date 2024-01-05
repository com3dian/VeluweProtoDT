# Hindcasting and validation of climate scenarios ####

# Created: 05/01/2024
# Author: Cherine Jantzen


# I. Preparation ----------------------------------------------------------

# load packages
library(lubridate)
library(dplyr)
library(ggplot2)

# load data
temp <- read.csv(here::here("data", "temp_climwin_input.csv"))
budburst <- read.csv(here::here("data","budburst_climwin_input.csv"))
climwin_QRobur <- load(here::here("data", "climwin_outputs_Qrobur.rda"))


# II. Format temperature data ---------------------------------------------

# format temperature data and filter for bud burst sensitive period
temp <- temp %>%
  dplyr::mutate(date = lubridate::as_date(date),
                year = lubridate::year(date),
                month = lubridate::month(date),
                day = lubridate::day(date),
                doy = lubridate::yday(date),
                # Create dummy for filtering window later. Format: 312 = March 12, 401 = April 1
                dummy = month * 100 + day) %>%
  # filter for the calculated window of climwin
  dplyr::filter(dummy > (lubridate::month(first_window_Qrobur$start_date) * 100 + lubridate::day(first_window_Qrobur$start_date)) &
                  dummy < (lubridate::month(first_window_Qrobur$end_date) * 100 + lubridate::day(first_window_Qrobur$end_date)))

# get mean temperature per day and year over all locations
mean_KNMI_temp <- temp %>%
  dplyr::summarise("mean_temperature_KNMI" = mean(temperature),
                   "sd_KNMI_temp" = sd(temperature, na.rm = TRUE),
                   .by = "year")


# III. Scenario data ------------------------------------------------------

## 1. Format scenario data ####
# load and format scenario data
FileNames = list.files("P:/Fair data for DT/Temperature scenarios/Scenario_data") # to be changed
# FileNames = list.files(here::here("data"), pattern = "cesm"))

# load each file
FileList <- lapply(paste("P:/Fair data for DT/Temperature scenarios/Scenario_data", FileNames, sep = "/"), # to be changed
                   function(x) read.table(x, header = TRUE, sep = ","))

# loop through all files to format data
climateScenarios <- NULL

for(f in seq(1, length(FileNames), by = 1)) {
  
  Data <- FileList[[f]]
  
  # add year and day of year
  Data <- Data %>%
    dplyr::mutate(date = lubridate::as_date(time, "%Y-%m-%d"),
                  year = lubridate::year(date),
                  dummy = lubridate::month(date) * 100 + lubridate::day(date),
                  doy = lubridate::yday(date),
                  # add mean temperatures in degree celcius
                  temp_degreesC = TREFHT - 273.15)
  
  # save data in vector
  run <- paste0("Run", f)
  climateScenarios <- append(climateScenarios, run, after = length(climateScenarios))
  assign(run, Data)
}


# III. Summarize data for model validation --------------------------------

## 1. Climate scenarios ####
Summary_scenarios <- NULL

for (f in seq(1, length(climateScenarios), by = 1)) {
  
  # get data for each run & filter scenario for temperature sensitive window of bud burst
  Data <- get(climateScenarios[f]) %>%
    dplyr::filter(dummy > (lubridate::month(first_window_Qrobur$start_date) * 100 + lubridate::day(first_window_Qrobur$start_date)) &
                    dummy < (lubridate::month(first_window_Qrobur$end_date) * 100 + lubridate::day(first_window_Qrobur$end_date)))
  
 # summarise and annotate data per run
  Data_MeanTemp <- Data %>% 
    dplyr::summarise(mean = mean(temp_degreesC, na.rm = TRUE), 
                     sd = sd(temp_degreesC, na.rm = TRUE),
                     .by = "year") %>%
    dplyr::mutate(type = rep("Model", nrow(.)),
                  scenario = rep("RCP_8.5", nrow(.)),
                  run = rep(f, nrow(.)))
  
  # standardize data 
  Data_for_standardization <- Data_MeanTemp %>% 
    dplyr::filter(year > (min(budburst$year) - 1) & year < (max(budburst$year) + 1))
  
  # get mean and standard deviation over all years
  overall_mean <- mean(Data_for_standardization$mean)
  overall_sd <- sd(Data_for_standardization$mean)
  
  df <- Data_MeanTemp %>% 
    # anomalies as (mean of year x - mean over years)/mean over years
    dplyr::mutate(anomaly = (mean - overall_mean) / overall_mean, 
                  # z-scores as (mean of year x - mean over years)/sd of yearly means
                  zScore = (mean - overall_mean) / overall_sd) 
  
  Summary_scenarios <- rbind(Summary_scenarios, df)
}

## 2. KNMI temperatures ####
KNMI_temp_summary <- 
  mean_KNMI_temp %>% 
  dplyr::mutate(overall_mean = mean(mean_temperature_KNMI),
                overall_sd = sd(mean_temperature_KNMI),
                type = rep("KNMI", nrow(.)),
                scenario = rep(NA, nrow(.)),
                run = rep(NA, nrow(.)),
                # anomalies as (mean of year x - mean over years)/mean over years
                anomaly = (mean_temperature_KNMI - overall_mean) / overall_mean,
                # z-scores as (mean of year x - mean over years)/sd of yearly means
                zScore = (mean_temperature_KNMI - overall_mean) / overall_sd) %>%
  dplyr::select(!c("overall_mean", "overall_sd"))

# combine summarized data of measured KNMI values and scenarios
# temp_KNMI_scenario <- rbind(Summary_scenarios, KNMI_temp_summary)


# IV. Model bud burst and temperature -------------------------------------

## 1. Model bud burst with measured KNMI temperatures ####
budburst_QR <- budburst %>%
  dplyr::filter(scientificName == "Quercus robur L.") %>%
  dplyr::mutate(year = as.numeric(year)) %>%
  dplyr::summarise(avg_budburst_DOY_allLoc = mean(avg_bud_burst_DOY), .by = "year")

# add bud burst to temperature
budburst_temp <- KNMI_temp_summary %>%
    dplyr::left_join(budburst_QR, by = "year")

# linear model
model_bb_KNMI_zScore <- lm(avg_budburst_DOY_allLoc ~ zScore, data = budburst_temp)

intercept_bb_KNMI_zScore <- as.numeric(model_bb_KNMI_zScore$coefficients[1])
slope_bb_KNMI_zScore <- as.numeric(model_bb_KNMI_zScore$coefficients[2])

summary(model_bb_KNMI_zScore)

ggplot() +
  geom_abline(intercept = intercept_bb_KNMI_zScore, slope = slope_bb_KNMI_zScore, linetype = 1, linewidth = 1, color = "black") +
  geom_point(data = budburst_temp, aes(x = zScore, y = avg_budburst_DOY_allLoc), color = "black", size = 3) +
  ylab("Bud burst date (DOY)") +
  xlab("zScore KNMI temperatures") +
  theme_classic()

# ggsave(filename = "P:/Fair data for DT/Temperature scenarios/plot_observedBB_KNMItemp_zscore.png")

## 2. Hindcasting: Predict bud burst based on scenario temperatures ####

Summary_scenario_bbPeriod <- Summary_scenarios %>% 
  dplyr::filter(year > (min(budburst_QR$year) - 1), year < (max(budburst_QR$year) + 1))


Predicted_budburst <- NULL
# Predicted_budburst_slope <- NULL

for (f in 1:length(unique(Summary_scenario_bbPeriod$run))) {
  
  data <- Summary_scenario_bbPeriod %>% 
    dplyr::filter(run == f)
  
  new <- data.frame(zScore = data$zScore)
  
  # predict bud burst dates 
  prediction <- predict(model_bb_KNMI_zScore, newdata = new, se.fit = TRUE, interval = "prediction")
  df <- data.frame(data, prediction$fit)
  
  # # linear model of predicted value and year 
  # model <- lm(fit ~ year, data = df)
  # slope <- as.numeric(model$coefficients[2])
  # df_Slope <- data.frame(Scenario = 1, Run = f, Slope = slope)
  
  # add data to data frames
  Predicted_budburst <- rbind(Predicted_budburst, df)
  # Predicted_budburst_slope <- rbind(Predicted_budburst_slope, df_Slope)
}

pred_obs_bb <- Predicted_budburst %>% 
  dplyr::select("predicted_bb_doy" = "fit", "year", "run") %>%
  left_join(budburst_QR, by = "year")

ggplot(pred_obs_bb, aes(avg_budburst_DOY_allLoc, predicted_bb_doy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Observed mean bud burst date [day of year]", y = "Predicted mean bud burst date [day of year]", title = "Predicted vs. observed based on zScores of temperature per scenario run") +
  facet_wrap(~ run) +
  xlim(100, 130)

# ggsave(filename = "P:/Fair data for DT/Temperature scenarios/plot_zscore_observed_pred_bb.png", unit = "cm", width = 30, height = 20)

# corrrelation of observed and predicted bud burst dates
cor(pred_obs_bb$avg_budburst_DOY_allLoc, pred_obs_bb$predicted_bb_doy)


# V. Alternative approach: no zScores but real temperatures -------------------


# linear model
model_bb_KNMI <- lm(avg_budburst_DOY_allLoc ~ mean_temperature_KNMI, data = budburst_temp)

intercept_bb_KNMI <- as.numeric(model_bb_KNMI_zScore$coefficients[1])
slope_bb_KNMI <- as.numeric(model_bb_KNMI_zScore$coefficients[2])

summary(model_bb_KNMI)

ggplot() +
  geom_abline(intercept = intercept_bb_KNMI, slope = slope_bb_KNMI, linetype = 1, size = 1, color = "black") +
  geom_point(data = budburst_temp, aes(x = mean_temperature_KNMI, y = avg_budburst_DOY_allLoc), color = "black", size = 3) +
  ylab("Bud burst date (DOY)") +
  xlab("KNMI temperatures") +
  theme_classic()

## 2. Hindcasting: Predict bud burst based on scenario temperatures ####

Summary_scenario_bbPeriod <- Summary_scenarios %>% 
  dplyr::filter(year > (min(budburst_QR$year) - 1), year < (max(budburst_QR$year) + 1))


Predicted_budburst <- NULL
# Predicted_budburst_slope <- NULL

for (f in 1:length(unique(Summary_scenario_bbPeriod$run))) {
  
  data <- Summary_scenario_bbPeriod %>% 
    dplyr::filter(run == f)
  
  new <- data.frame(mean_temperature_KNMI = data$mean) # not KNMI data just rename for now to make the model run
  
  # predict bud burst dates 
  prediction <- predict(model_bb_KNMI, newdata = new, se.fit = TRUE, interval = "prediction")
  df <- data.frame(data, prediction$fit)
  
  # # linear model of predicted value and year 
  # model <- lm(fit ~ year, data = df)
  # slope <- as.numeric(model$coefficients[2])
  # df_Slope <- data.frame(Scenario = 1, Run = f, Slope = slope)
  
  # add data to data frames
  Predicted_budburst <- rbind(Predicted_budburst, df)
  # Predicted_budburst_slope <- rbind(Predicted_budburst_slope, df_Slope)
}

pred_obs_bb <- Predicted_budburst %>% 
  dplyr::select("predicted_bb_doy" = "fit", "year", "run", "mean_temp" = "mean") %>%
  left_join(budburst_temp %>%
              dplyr::select("year", "mean_temperature_KNMI", "avg_budburst_DOY_allLoc"), by = "year")

ggplot(pred_obs_bb, aes(mean_temp, mean_temperature_KNMI)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~ run) +
  labs(x = "Measured KNMI mean temperature of window", y = "Mean scenario temperature in window", title = "KNMI vs. scenario temperatures per scneario run")

# ggsave(filename = "P:/Fair data for DT/Temperature scenarios/plot_KNMI_scenario_temp.png", unit = "cm", width = 30, height = 20)
# corrrelation of observed and predicted bud burst dates
cor(pred_obs_bb$mean_temp, pred_obs_bb$mean_temp_KNMI)

ggplot(pred_obs_bb, aes(avg_budburst_DOY_allLoc, predicted_bb_doy)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Observed mean bud burst date [day of year]", y = "Predicted mean bud burst date [day of year]", title = "Predicted vs. observed based on actual temperature per scenario run") +
  facet_wrap(~ run) +
  xlim(100, 130)

# ggsave(filename = "P:/Fair data for DT/Temperature scenarios/plot_pred_obs_bb.png", unit = "cm", width = 30, height = 20)
