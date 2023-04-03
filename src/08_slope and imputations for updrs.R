knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(ggpmisc)
library(cvms)
library(groupdata2)
library(performance)
library(lme4)
library(lmerTest)
library(dlookr)
library(corrr)
library(fpp3)

parent          = dirname(getwd())
data.path       = file.path(parent, "data")
train.data.path = file.path(data.path, "intermediate", "data from 07_rmd")

Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }

  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

all_data = readRDS(file = file.path(train.data.path, "alldata.rds"))

# The slope was computed between the first and the last visit in our time range. 
# This was done for each subject. The time variable was scaled to avoid shrinking the slope
## PD ON ----
temp = all_data$pd_on %>% 
  dplyr::select(PATNO, day_num, is_visit_day, updrs_tot) %>% 
  na.omit() %>% 
  group_by(PATNO) %>% 
  nest() %>% 
  mutate(lm = map(.x = data, .f = ~ lm(updrs_tot ~ day_num/max(day_num), data = .x)$coefficients[2])) %>% 
  unnest() %>% 
  dplyr::select(-c("day_num", "is_visit_day", "updrs_tot")) %>% 
  distinct() %>% 
  ungroup()

all_data$pd_on = left_join(all_data$pd_on,temp, by = "PATNO") %>% 
  rename(updrs_slope = lm)

## PD OFF ----
temp = all_data$pd_off %>% 
  dplyr::select(PATNO, day_num, is_visit_day, updrs_tot) %>% 
  na.omit() %>% 
  group_by(PATNO) %>% 
  nest() %>% 
  mutate(lm = map(.x = data, .f = ~ lm(updrs_tot ~ scale(day_num), data = .x)$coefficients[2])) %>% 
  unnest() %>% 
  dplyr::select(-c("day_num", "is_visit_day", "updrs_tot")) %>% 
  distinct() %>% 
  ungroup()

all_data$pd_off = left_join(all_data$pd_off,temp, by = "PATNO") %>% 
  rename(updrs_slope = lm)

# add week variable ----
all_data$pd_on = all_data$pd_on  %>% group_by(PATNO) %>%  mutate(week_num = 1:n() %/% 7, .after = day_num) %>% ungroup()
all_data$pd_off= all_data$pd_off %>% group_by(PATNO) %>% mutate(week_num = 1:n() %/% 7, .after = day_num) %>% ungroup()

rm(temp)

## Data reparing ----
pd_on_temp = all_data$pd_on %>% 
  mutate(across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ na_if(.x, 0)),
         across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ ifelse(.x < 0, NA, .x))) %>% 
  mutate(sleep_efficiency          = ifelse(sleep_efficiency > 1, NA, sleep_efficiency),
         inbed_time                = round(ifelse(inbed_time >= 24 | inbed_time <= 1, NA, inbed_time)),
         total_sleep_time_ms       = ifelse(total_sleep_time_ms > 43200000, NA, total_sleep_time_ms),
         wake_after_sleep_onset_ms = ifelse(wake_after_sleep_onset_ms > 43200000, NA, total_sleep_time_ms),
         num_awakenings            = round(ifelse(num_awakenings < 0, 0, num_awakenings))) %>% 
  mutate(total_nrem_time_ms        = dlookr::imputate_outlier(.data = ., xvar = total_nrem_time_ms, method = "capping", no_attrs = T), 
         total_rem_time_ms         = dlookr::imputate_outlier(.data = ., xvar = total_rem_time_ms,method = "capping", no_attrs = T), 
         total_deep_nrem_time_ms   = dlookr::imputate_outlier(.data = ., xvar = total_deep_nrem_time_ms,method = "capping", no_attrs = T),
         total_light_nrem_time_ms  = dlookr::imputate_outlier(.data = ., xvar = total_light_nrem_time_ms,method = "capping", no_attrs = T),
         sleep_efficiency          = dlookr::imputate_outlier(.data = ., xvar = sleep_efficiency,method = "capping", no_attrs = T),
         inbed_time                = dlookr::imputate_outlier(.data = ., xvar = inbed_time,method = "capping", no_attrs = T),
         total_sleep_time_ms       = dlookr::imputate_outlier(.data = ., xvar = total_sleep_time_ms,method = "capping", no_attrs = T),
         wake_after_sleep_onset_ms = dlookr::imputate_outlier(.data = ., xvar = wake_after_sleep_onset_ms, method = "capping", no_attrs = T),
         weigthed_mean_rmssd       = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_rmssd, method = "capping", no_attrs = T),
         weigthed_mean_cv_rmssd    = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_cv_rmssd,method = "capping", no_attrs = T),
         weigthed_mean_pulse_rate  = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_pulse_rate,method = "capping", no_attrs = T)) %>% 
  mutate(across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ zoo::na.aggregate(.x, by = week_num, FUN = mean, maxgap = 7)),
         across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ zoo::na.approx(.x)))

## Feature engineering ----
# Creating the cumulated dataframes (start from week 0 and add a week)
time_vec         = seq(0,52,1)
time_list        = vector(mode = "list", length = length(time_vec))
lambda_meta_list = vector(mode = "list", length = length(time_vec))
stl_list         = vector(mode = "list", length = length(time_vec))
names(time_list) = time_vec
names(lambda_meta_list) = time_vec
names(stl_list)  = time_vec
predictors       = names(pd_on_temp)[21:34]
predictors_box = paste0(predictors, "_box")
max_day_num_by_patno = pd_on_temp %>% 
  dplyr::select(PATNO, day_num) %>% 
  group_by(PATNO) %>% 
  filter(day_num == max(day_num)) %>% 
  rename(max_day_num=day_num) %>% 
  ungroup()
# i= 1
# j= 1
# Looping to subset week starting by 0 and adding up to 52 (the complete starting dataframe)
for (i in 1:length(time_vec)) {
  # print(paste(i, time_vec[i]))
  time_list[[i]] = pd_on_temp %>% 
    filter(week_num <= time_vec[[i]]) %>% 
    tsibble::as_tsibble(key   = PATNO, 
                        index = day_num) # Converting as time series dataframe
  
  ## Prepare for STL Decomposition
  # Finding lambda
  lambda_list = list()
  for (j in 1:length(predictors)) {
    lambda_list[[j]] = round(forecast::BoxCox.lambda(x = (time_list[[i]][[predictors[j] ]]+1), method = "loglik"), 1)
  }
  names(lambda_list) = predictors
  
  # Box Cox Transform
  for (j in 1:length(predictors)) {
    time_list[[i]] = time_list[[i]] %>% 
      mutate("{predictors[j]}_box" := fabletools::box_cox(.data[[predictors[j] ]], lambda = lambda_list[[j]]))
  }
  
  ## STL Decomposition
  df_stl = time_list[[i]] %>% 
    dplyr::select(PATNO, day_num, contains("_box")) %>% 
    pivot_longer(cols = contains("_box")) %>% 
    group_by(name) %>% 
    nest() %>% 
    mutate(dcmp = map(.x = data, .f = ~ .x %>% model(STL(value ~ trend(window = 7)))))
  
  # Extract decomposition
  df_decomp = df_stl$dcmp %>%
    lapply(
      function(x) x %>% 
        mutate(decomp = map(.x = `STL(value ~ trend(window = 7))`,
                            .f = ~ as_tibble(.x$fit$decomposition))) %>% 
        unnest(decomp) %>% 
        dplyr::select(-`STL(value ~ trend(window = 7))`)) %>% 
    `names<-`(predictors_box) %>% 
    bind_rows(.id = "var")
  
  # Pivot wider the decomposed trends to become our predictors
  df_predictors = df_decomp %>% 
    dplyr::select(c("var", "PATNO", "day_num", "trend")) %>% 
    mutate(var = paste0(var, "_trend")) %>% 
    pivot_wider(names_from = var,
                values_from = trend) %>% 
    arrange(PATNO, day_num) %>% 
    mutate(across(.cols = contains("trend"), .fns = ~ as.numeric(scale(.x)))) %>% # Here preds are scaled
    rename_with(.cols = contains("trend"), .fn = ~ paste0(.x, "_z"))
  
  # Merge with a subset of the original pd_on dataframe
  time_list[[i]] = left_join(time_list[[i]],
                             df_predictors, by = c("PATNO", "day_num")) %>% 
    as_tibble()
  
  # Now compute the slope for each predictor
  time_list[[i]] = left_join(time_list[[i]], 
            max_day_num_by_patno, by = "PATNO") %>% 
    group_by(PATNO) %>% 
    nest() %>% 
    mutate(inbed_time_slope           = map(.x = data, .f = ~ lm(inbed_time_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_sleep_time_slope     = map(.x = data, .f = ~ lm(total_sleep_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           wake_slope                 = map(.x = data, .f = ~ lm(wake_after_sleep_onset_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           sleep_efficiency_slope     = map(.x = data, .f = ~ lm(sleep_efficiency_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           num_awakenings_slope       = map(.x = data, .f = ~ lm(num_awakenings_box_trend_z   ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_nrem_time_slope      = map(.x = data, .f = ~ lm(total_nrem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_rem_time_slope       = map(.x = data, .f = ~ lm(total_rem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_deep_nrem_time_slope = map(.x = data, .f = ~ lm(total_deep_nrem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_light_nrem_time_slope= map(.x = data, .f = ~ lm(total_light_nrem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           daily_step_count_slope     = map(.x = data, .f = ~ lm(daily_step_count_sum_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_mean_rmssd_slope  = map(.x = data, .f = ~ lm(weigthed_mean_rmssd_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_median_rmssd_slope= map(.x = data, .f = ~ lm(weigthed_median_rmssd_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_mean_cv_rmssd_slope=map(.x = data, .f = ~ lm(weigthed_mean_cv_rmssd_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_mean_pulse_rate_slope=map(.x = data, .f = ~ lm(weigthed_mean_pulse_rate_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2])
    ) %>% 
    unnest(c(data, inbed_time_slope, total_sleep_time_slope, wake_slope, 
    sleep_efficiency_slope, num_awakenings_slope, total_nrem_time_slope, 
    total_rem_time_slope, total_deep_nrem_time_slope, total_light_nrem_time_slope, 
    daily_step_count_slope, weigthed_mean_rmssd_slope, weigthed_median_rmssd_slope, 
    weigthed_mean_cv_rmssd_slope, weigthed_mean_pulse_rate_slope))
  
  # Saving all the object for future reference and plots part 1
  lambda_meta_list[[i]] = lambda_list
  stl_list[[i]]         =  df_stl
  
}
# Saving all the object for future reference and plots part 2
stl_decomposition_pd_on_list = list(raw_pd_on        = pd_on_temp,
                                    decomposed_pd_on = time_list,
                                    lambda_meta_list = lambda_meta_list,
                                    stl_list         = stl_list)

rm(df_stl, lambda_list, df_predictors, stl_list)
# substituted by raw_pd_on
rm(pd_on_temp)

## Data reparing ----
pd_off_temp = all_data$pd_off %>% 
  mutate(across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ na_if(.x, 0)),
         across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ ifelse(.x < 0, NA, .x))) %>% 
  mutate(sleep_efficiency          = ifelse(sleep_efficiency > 1, NA, sleep_efficiency),
         inbed_time                = round(ifelse(inbed_time >= 24 | inbed_time <= 1, NA, inbed_time)),
         total_sleep_time_ms       = ifelse(total_sleep_time_ms > 43200000, NA, total_sleep_time_ms),
         wake_after_sleep_onset_ms = ifelse(wake_after_sleep_onset_ms > 43200000, NA, total_sleep_time_ms),
         num_awakenings            = round(ifelse(num_awakenings < 0, 0, num_awakenings))) %>% 
  mutate(total_nrem_time_ms        = dlookr::imputate_outlier(.data = ., xvar = total_nrem_time_ms, method = "capping", no_attrs = T), 
         total_rem_time_ms         = dlookr::imputate_outlier(.data = ., xvar = total_rem_time_ms,method = "capping", no_attrs = T), 
         total_deep_nrem_time_ms   = dlookr::imputate_outlier(.data = ., xvar = total_deep_nrem_time_ms,method = "capping", no_attrs = T),
         total_light_nrem_time_ms  = dlookr::imputate_outlier(.data = ., xvar = total_light_nrem_time_ms,method = "capping", no_attrs = T),
         sleep_efficiency          = dlookr::imputate_outlier(.data = ., xvar = sleep_efficiency,method = "capping", no_attrs = T),
         inbed_time                = dlookr::imputate_outlier(.data = ., xvar = inbed_time,method = "capping", no_attrs = T),
         total_sleep_time_ms       = dlookr::imputate_outlier(.data = ., xvar = total_sleep_time_ms,method = "capping", no_attrs = T),
         wake_after_sleep_onset_ms = dlookr::imputate_outlier(.data = ., xvar = wake_after_sleep_onset_ms, method = "capping", no_attrs = T),
         weigthed_mean_rmssd       = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_rmssd, method = "capping", no_attrs = T),
         weigthed_mean_cv_rmssd    = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_cv_rmssd,method = "capping", no_attrs = T),
         weigthed_mean_pulse_rate  = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_pulse_rate,method = "capping", no_attrs = T)) %>% 
  mutate(across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ zoo::na.aggregate(.x, by = week_num, FUN = mean, maxgap = 7)),
         across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ zoo::na.approx(.x)))

## Feature engineering ----
# Creating the cumulated dataframes (start from week 0 and add a week)
time_vec         = seq(0,52,1)
time_list        = vector(mode = "list", length = length(time_vec))
lambda_meta_list = vector(mode = "list", length = length(time_vec))
stl_list         = vector(mode = "list", length = length(time_vec))
names(time_list) = time_vec
names(lambda_meta_list) = time_vec
names(stl_list)  = time_vec
predictors       = names(pd_off_temp)[21:34]
predictors_box = paste0(predictors, "_box")
max_day_num_by_patno = pd_off_temp %>% 
  dplyr::select(PATNO, day_num) %>% 
  group_by(PATNO) %>% 
  filter(day_num == max(day_num)) %>% 
  rename(max_day_num=day_num) %>% 
  ungroup()
# i= 1
# j= 1

# Looping to subset week starting by 0 and adding up to 52 (the complete starting dataframe)
for (i in 1:length(time_vec)) {
  # print(paste(i, time_vec[i]))
  time_list[[i]] = pd_off_temp %>% 
    filter(week_num <= time_vec[[i]]) %>% 
    tsibble::as_tsibble(key   = PATNO, 
                        index = day_num) # Converting as time series dataframe
  
  ## Prepare for STL Decomposition
  # Finding lambda
  lambda_list = list()
  for (j in 1:length(predictors)) {
    lambda_list[[j]] = round(forecast::BoxCox.lambda(x = (time_list[[i]][[predictors[j] ]]+1), method = "loglik"), 1)
  }
  names(lambda_list) = predictors
  
  # Box Cox Transform
  for (j in 1:length(predictors)) {
    time_list[[i]] = time_list[[i]] %>% 
      mutate("{predictors[j]}_box" := fabletools::box_cox(.data[[predictors[j] ]], lambda = lambda_list[[j]]))
  }
  
  ## STL Decomposition
  df_stl = time_list[[i]] %>% 
    dplyr::select(PATNO, day_num, contains("_box")) %>% 
    pivot_longer(cols = contains("_box")) %>% 
    group_by(name) %>% 
    nest() %>% 
    mutate(dcmp = map(.x = data, .f = ~ .x %>% model(STL(value ~ trend(window = 7)))))
  
  # Extract decomposition
  df_decomp = df_stl$dcmp %>%
    lapply(
      function(x) x %>% 
        mutate(decomp = map(.x = `STL(value ~ trend(window = 7))`,
                            .f = ~ as_tibble(.x$fit$decomposition))) %>% 
        unnest(decomp) %>% 
        dplyr::select(-`STL(value ~ trend(window = 7))`)) %>% 
    `names<-`(predictors_box) %>% 
    bind_rows(.id = "var")
  
  # Pivot wider the decomposed trends to become our predictors
  df_predictors = df_decomp %>% 
    dplyr::select(c("var", "PATNO", "day_num", "trend")) %>% 
    mutate(var = paste0(var, "_trend")) %>% 
    pivot_wider(names_from = var,
                values_from = trend) %>% 
    arrange(PATNO, day_num) %>% 
    mutate(across(.cols = contains("trend"), .fns = ~ as.numeric(scale(.x)))) %>% # Here preds are scaled
    rename_with(.cols = contains("trend"), .fn = ~ paste0(.x, "_z"))
  
  # Merge with a subset of the original pd_off dataframe
  time_list[[i]] = left_join(time_list[[i]],
                             df_predictors, by = c("PATNO", "day_num")) %>% 
    as_tibble()
  
  # Now compute the slope for each predictor
  time_list[[i]] = left_join(time_list[[i]], 
            max_day_num_by_patno, by = "PATNO") %>% 
    group_by(PATNO) %>% 
    nest() %>% 
    mutate(inbed_time_slope           = map(.x = data, .f = ~ lm(inbed_time_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_sleep_time_slope     = map(.x = data, .f = ~ lm(total_sleep_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           wake_slope                 = map(.x = data, .f = ~ lm(wake_after_sleep_onset_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           sleep_efficiency_slope     = map(.x = data, .f = ~ lm(sleep_efficiency_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           num_awakenings_slope       = map(.x = data, .f = ~ lm(num_awakenings_box_trend_z   ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_nrem_time_slope      = map(.x = data, .f = ~ lm(total_nrem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_rem_time_slope       = map(.x = data, .f = ~ lm(total_rem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_deep_nrem_time_slope = map(.x = data, .f = ~ lm(total_deep_nrem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           total_light_nrem_time_slope= map(.x = data, .f = ~ lm(total_light_nrem_time_ms_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           daily_step_count_slope     = map(.x = data, .f = ~ lm(daily_step_count_sum_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_mean_rmssd_slope  = map(.x = data, .f = ~ lm(weigthed_mean_rmssd_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_median_rmssd_slope= map(.x = data, .f = ~ lm(weigthed_median_rmssd_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_mean_cv_rmssd_slope=map(.x = data, .f = ~ lm(weigthed_mean_cv_rmssd_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2]),
           weigthed_mean_pulse_rate_slope=map(.x = data, .f = ~ lm(weigthed_mean_pulse_rate_box_trend_z ~ day_num/max_day_num, data = .x)$coefficients[2])
    ) %>% 
    unnest(c(data, inbed_time_slope, total_sleep_time_slope, wake_slope, 
    sleep_efficiency_slope, num_awakenings_slope, total_nrem_time_slope, 
    total_rem_time_slope, total_deep_nrem_time_slope, total_light_nrem_time_slope, 
    daily_step_count_slope, weigthed_mean_rmssd_slope, weigthed_median_rmssd_slope, 
    weigthed_mean_cv_rmssd_slope, weigthed_mean_pulse_rate_slope))
  
  # Saving all the object for future reference and plots part 1
  lambda_meta_list[[i]] = lambda_list
  stl_list[[i]]         =  df_stl
}
  

# Saving all the object for future reference and plots part 2
stl_decomposition_pd_off_list = list(raw_pd_off        = pd_off_temp,
                                     decomposed_pd_off = time_list,
                                     lambda_meta_list  = lambda_meta_list,
                                     stl_list          = stl_list)

rm(df_stl, lambda_list, df_predictors, stl_list)
# substituted by raw_pd_off
rm(pd_off_temp)

save(stl_decomposition_pd_off_list,
     stl_decomposition_pd_on_list,
     file = file.path(data.path, "intermediate", "data_for_07_rmd", "stl slope data.rdata"))
