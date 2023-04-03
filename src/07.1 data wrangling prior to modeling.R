knitr::opts_chunk$set(echo = TRUE)

root        = dirname(getwd())
verily_path = file.path(root, "data", "sensors")
meta_path   = file.path(root, "data", "intermediate", "metadata")
analytical_dataset_path = file.path(root, "data", "raw",
                                    "study_data", "study_docs", "Quick_start",
                                    "Consensus_Committee_Analytic_Datasets_28OCT21.xlsx")
wr_path     = file.path(root, "data", "intermediate", "data_for_07_rmd")
sensors.path=file.path(root, "data", "intermediate", "temp")

# udf
assign_day_numbers <- function(vec) {
  # Define an empty vector to store the output
  out_vec <- c()

  # Loop through each element in the vector and assign a day number based on the previous day
  day_num <- 1
  for (i in 1:length(vec)) {
    if (i == 1) {
      out_vec[i] <- day_num
    } else if (vec[i] == vec[i-1]) {
      out_vec[i] <- day_num
    } else {
      day_num <- day_num + 1
      out_vec[i] <- day_num
    }
  }
  
  # Return the new vector
  return(out_vec)
}
minmax <- function(ema) {
  (ema - min(ema, na.rm = T))/(max(ema, na.rm = T)-min(ema, na.rm = T))
}


# libraries
library(tidyverse)
library(openxlsx)
library(fst)
library(patchwork)

compliance.list = readRDS(file.path(wr_path,"compliance_list.rds"))
clinical.list   = readRDS(file.path(wr_path, "clinical_list.rds"))

metadata_cohort = readRDS(file.path(meta_path, "cohort metadata.rds"))

sheet = readxl::excel_sheets(analytical_dataset_path)[3:5]
analytic_dataset = list()
for (i in sheet) {
  analytic_dataset[[i]] =read.xlsx(analytical_dataset_path, sheet = i)
}
names(analytic_dataset) = sheet

meta_clinical_data = readRDS(file.path(meta_path, "clinical timeline.rds"))

names = list.files(verily_path,".fst")[!str_detect(string = list.files(verily_path,".fst"), "onwrist|timezone")]
paths = list.files(verily_path,".fst",full.names = T)[!str_detect(string = list.files(verily_path,".fst"), "onwrist|timezone")]
sens.list = list()

for (i in 1:length(paths)) {
  sens.list[[i]]  = read.fst(path = paths[i])
}
names(sens.list) = names

rm(names,paths,i)

# How many subjects arrived to the 52th+-11 week? 251 which is approximately 77% of the sample
# These guys had a mean daily compliance of 80%

# 52: mean duration               // 364 days
# 58: parkinson median duration  //  401,5 days 
# 63: one year with a two months of tolerance for clinical visit // 441 days // 11 weeks more than 1 year
# week_selector = 63 

one_year_in_weeks = 52
weeks_of_tolerance = 11

# How many subjects had approximately 52 + 11 weeks of the study // duration criteria
all_about_time = left_join(compliance.list$compliance.df %>% 
                             mutate(day_num = str_remove(day_num, "day") %>% as.numeric()), 
                           compliance.list$tempabc %>%
                             mutate(subject = as.character(subject)), 
                           by = c("subject", "day_num"))

# Apply duration filter
all_about_time = all_about_time %>% 
  mutate(duration_visits = (max(AGE_AT_VISIT)- min(AGE_AT_VISIT))*52) %>% 
  filter(duration_visits >= (one_year_in_weeks - weeks_of_tolerance))

# Subset participants
compliant_participants = all_about_time %>% 
  mutate(duration_visits = (max(AGE_AT_VISIT)- min(AGE_AT_VISIT))*52) %>% 
  filter(duration_visits >= (one_year_in_weeks - weeks_of_tolerance)) %>% 
  .$subject %>%
  unique()

# Composition of the 251 subjects in the different cohorts
clinical.list$covariate_for_updrs %>% 
  dplyr::select(PATNO, diagnosis_updated) %>%
  rename(subject=PATNO) %>% 
  mutate(subject = as.numeric(subject)) %>%
  distinct() %>% 
  filter(subject %in% compliant_participants) %>%
  xtabs(~ diagnosis_updated, data = .) %>% 
  addmargins()

files = list.files(path       = sensors.path, 
                   pattern    = ".csv",
                   full.names = TRUE)

files = files[str_detect(string = files, pattern = paste(compliant_participants, collapse = '|'))] 


failed_subjects = c("3068",  # Subject with inbed time < 10
                    "4091",  # Subject with sleep metrics < 10
                    "59174", # Subject with sleep metrics < 10
                    "53258"  # Subject with sleep metrics < 10
                    )

files = files[!str_detect(files, pattern = paste(failed_subjects, collapse = "|"))]

cs_list = vector(mode = "list", length = length(files))
for (i in 1:length(cs_list)) {
  # Start routine for each subject (cs)
  cs       = suppressMessages(read_csv(files[i]))
  cs_patno = as.numeric(str_remove(word(files[i], start =-1, sep = "/"),".csv"))
  
  print(paste0(round(i/length(files)*100, 2),"%"))
  
  ## Define the metadata including the clinical timeline and the verily experiment timeline ----
  meta_cs = cs %>% 
    dplyr::select(subject, age_seconds, time_day, day_num, proximal_clinical_event,
                  correspondent_clinical_event, AGE_AT_VISIT, daily_minutes_on, daily_prop_of_use) %>%
    mutate(day_num = as.numeric(str_remove(day_num, "day"))) %>% 
    distinct()
  
  ## Update sensors data with the above metadata ----
  ### Sleep Time resolution: each row is a day ----
  ### When you plot it or compute the slope you use Daynum as x axis
  sleepmetrics = right_join(meta_cs,
                            sens.list$sleepmetrics2.fst %>% 
                              filter(subject == cs_patno) %>% 
                              dplyr::select(subject, age_seconds, time_day,
                                            sleep_onset_time_ms,sleep_offset_time_ms,
                                            total_sleep_time_ms, wake_after_sleep_onset_ms,
                                            sleep_efficiency, num_awakenings, total_nrem_time_ms,
                                            total_rem_time_ms, total_deep_nrem_time_ms, total_light_nrem_time_ms),
                            by = c("subject", "age_seconds", "time_day"))
  
  ### Filtering data to cover 1 year of the study ----
  sleepmetrics = 
    sleepmetrics %>%
    # filter(day_num <=  week_selector*7)
    filter(day_num <= (one_year_in_weeks+weeks_of_tolerance)*7)
  
  # Duplicate days here depend on multiple naps in the same day || We will summarise the variable
  dupl_days = sleepmetrics$day_num[duplicated(sleepmetrics$day_num)]
  
  if (length(dupl_days) > 0) {
    sleep_binder = sleepmetrics %>%
      mutate(duplicated = ifelse(day_num %in% dupl_days, "dupl", "ok")) %>% 
      filter(duplicated == "dupl") %>% 
      group_by(day_num) %>% 
      summarise(total_sleep_time_ms       = sum(total_sleep_time_ms),
                wake_after_sleep_onset_ms = mean(wake_after_sleep_onset_ms),
                sleep_efficiency          = mean(sleep_efficiency),
                num_awakenings            = sum(num_awakenings),
                total_nrem_time_ms        = sum(total_nrem_time_ms),
                total_rem_time_ms         = sum(total_rem_time_ms),
                total_light_nrem_time_ms  = sum(total_light_nrem_time_ms))
    
    sleepmetrics = bind_rows(sleepmetrics %>%
                               mutate(duplicated = ifelse(day_num %in% dupl_days, "dupl", "ok")) %>% 
                               filter(duplicated != "dupl"),
                             sleep_binder) %>% arrange(day_num) %>% dplyr::select(-duplicated)
  }
  
  ### In bed times ----
  ### Time resolution: each row is a day // When you plot it or compute the slope you use Daynum as x axis
  inbed = right_join(meta_cs,
                     sens.list$inbedtimes.fst %>% 
                       filter(subject==cs_patno) %>% 
                       mutate(dummy_start_time_local = lubridate::ydm_hms(paste(as.Date("1970-01-01"), start_time_local)),
                              dummy_end_time_local   = lubridate::ydm_hms(paste(as.Date("1970-02-01"), end_time_local)),
                              inbed_time       = dummy_end_time_local- dummy_start_time_local) %>% 
                       mutate(dummy_start_time_local = case_when(inbed_time > 24 ~ lubridate::ydm_hms(paste(as.Date("1970-02-01"), start_time_local)),
                                                                 TRUE ~ dummy_start_time_local),
                              inbed_time = case_when(inbed_time > 24 ~ dummy_end_time_local - dummy_start_time_local,
                                                     TRUE ~ inbed_time)) %>% 
                       dplyr::select(-c("dummy_start_time_local", "dummy_end_time_local")) %>% 
                       mutate(inbed_time = as.numeric(inbed_time)) %>% 
                       filter(inbed_time != 24),
                     by = c("subject", "age_seconds", "time_day"))
  
  inbed = inbed %>% filter(day_num <= (one_year_in_weeks+weeks_of_tolerance)*7)
  
  # Duplicate days check in the inbed variable
  dupl_days = inbed$day_num[duplicated(inbed$day_num)]
  if (length(dupl_days) > 0) {
    # print("Warning, duplicate values in inbed")
    inbed_binder = inbed %>%
      mutate(duplicated = ifelse(day_num %in% dupl_days, "dupl", "ok")) %>% 
      filter(duplicated == "dupl") %>% 
      group_by(day_num) %>% 
      summarise(inbed_time = sum(inbed_time))
    
    inbed = bind_rows(inbed %>%
                        mutate(duplicated = ifelse(day_num %in% dupl_days, "dupl", "ok")) %>% 
                        filter(duplicated != "dupl"),
                      inbed_binder) %>% arrange(day_num) %>% dplyr::select(-duplicated)
  }
  
  ### Step Count ----
  ### Hour resoultion
  #### Step count before filtering for awake hours ----
  stepcount = right_join(meta_cs, sens.list$stepcount.fst %>% 
                           filter(subject == cs_patno), by = c("subject", "age_seconds", "time_day")) %>% 
    group_by(day_num, time_local) %>% 
    mutate(day_hour_index = cur_group_id()) %>%  
    ungroup()  
  
  # matching the awake/sleep hours
  daily_sleep_intervals= right_join(meta_cs,sens.list$sleepmetrics2.fst %>% 
                                      filter(subject == cs_patno) %>% 
                                      dplyr::select(subject, age_seconds, time_day,sleep_onset_time_local,sleep_offset_time_local),
                                    by = c("subject", "age_seconds", "time_day")) %>%
    dplyr::select(subject, age_seconds, day_num, sleep_onset_time_local, sleep_offset_time_local) %>% 
    mutate(sleep_onset_time_rounded = paste0(word(sleep_onset_time_local, start = 1, sep = ":"), ":00:00"),
           sleep_offset_time_rounded= paste0(as.numeric(word(sleep_offset_time_local, start = 1, sep = ":"))+1,":00:00")) %>% 
    dplyr::select(subject, day_num, sleep_onset_time_rounded, sleep_offset_time_rounded)
  
  # filtering for known awake hours
  step_count_filtered = left_join(stepcount, daily_sleep_intervals, by = c("subject", "day_num")) %>% 
    mutate(sleep_onset_time_rounded = as.numeric(word(sleep_onset_time_rounded, start = 1, sep = ":")),
           sleep_offset_time_rounded= as.numeric(word(sleep_offset_time_rounded, start = 1, sep = ":")),
           time_local2              = as.numeric(word(time_local, start = 1, sep = ":")),
           flag                     = case_when((time_local2 >= sleep_onset_time_rounded &
                                                   time_local2 <= sleep_offset_time_rounded) & hourly_step_count_sum == 0 ~ "flag",
                                                TRUE ~ "Awake")) %>% 
    filter(flag == "Awake") %>% 
    dplyr::select(-c(time_local2, sleep_onset_time_rounded, sleep_offset_time_rounded)) %>% 
    group_by(day_num, time_local) %>% 
    mutate(day_hour_index = cur_group_id()) %>%  
    ungroup() 
  
  ### Porting step counts to day resolution ---
  ### We need to fix the day num duplication due to age in seconds
  step_count_filtered = step_count_filtered %>% 
    group_by(day_num) %>% 
    mutate(daily_step_count_sum = sum(hourly_step_count_sum)) %>% 
    dplyr::select(subject, age_seconds, 
                  time_day, day_num, daily_minutes_on, proximal_clinical_event,
                  correspondent_clinical_event, AGE_AT_VISIT, daily_step_count_sum) %>% 
    distinct() %>%
    ungroup()
  
  
  ### Filtering data to cover 1 year of the study ----
  step_count_filtered = step_count_filtered %>% filter(day_num <= (one_year_in_weeks+weeks_of_tolerance)*7)
  
  # Fix the day num duplication due to age in seconds
  step_count_filtered = step_count_filtered %>% 
    group_by(day_num) %>% 
    mutate(min_age_seconds = min(age_seconds),
           max_age_seconds = max(age_seconds),
           .before = "time_day") %>% 
    dplyr::select(-age_seconds) %>% 
    ungroup() %>% 
    distinct()
  
  # Duplicate days here depend on correspondent clinical visit indicator that is switching from true to false in the same day
  # We will keep only the true when the column is duplicated
  dupl_days = step_count_filtered$day_num[duplicated(step_count_filtered$day_num)]
  step_count_filtered = step_count_filtered %>%
    mutate(duplicated = ifelse(day_num %in% dupl_days, "dupl", "ok"),
           flag       = case_when(duplicated == "dupl" & correspondent_clinical_event == TRUE ~ "keep",
                                  duplicated == "dupl" & correspondent_clinical_event == FALSE ~ "discard",
                                  TRUE ~ "keep")) %>% 
    filter(flag == "keep") %>% 
    dplyr::select(-c("duplicated","flag"))
  
  if (any(str_detect(unique(step_count_filtered$proximal_clinical_event), "SC"))) {
    step_count_filtered = step_count_filtered %>% 
      filter(proximal_clinical_event != "SC")
  }
  
  if (cs_patno  == "75403") {
    step_count_filtered =  step_count_filtered %>% 
      mutate(AGE_AT_VISIT            = ifelse(proximal_clinical_event == "R06" & day_num == 201, 68.5, AGE_AT_VISIT),
             proximal_clinical_event = ifelse(proximal_clinical_event == "R06" & day_num == 201, "V05", proximal_clinical_event),
      ) %>% 
      distinct()
  }
  
  ### Heart data 1 ----
  ### Time resolution: each row is an hour and when you plot it or compute the slope you need to use day_hour_index as x axis
  prv = right_join(meta_cs, 
                   sens.list$prv.fst %>% 
                     filter(subject == cs_patno) %>% 
                     mutate(hourly_rmssd_variance = as.numeric(ifelse(hourly_rmssd_variance == "None", 0, hourly_rmssd_variance))),
                   by = c("subject", "age_seconds", "time_day")) %>% 
    group_by(day_num, time_local) %>% 
    mutate(day_hour_index = cur_group_id()) %>%  
    ungroup()
  
  ### Porting prv data to day resolution ---
  prv = left_join(meta_cs, prv %>% 
                    group_by(day_num) %>% 
                    summarise(weigthed_mean_rmssd    = weighted.mean(x=hourly_mean_rmssd, w = sample_count),
                              weigthed_median_rmssd  = matrixStats::weightedMedian(hourly_median_rmssd, w = sample_count),
                              weigthed_mean_cv_rmssd = weighted.mean(hourly_rmssd_variance/hourly_mean_rmssd, w = sample_count)), by = "day_num")
  
  ### Filtering data to cover 1 year of the study ----
  prv = prv %>% filter(day_num <= (one_year_in_weeks+weeks_of_tolerance)*7)
  
  # Fix the day num duplication due to age in seconds
  prv = prv %>% 
    group_by(day_num) %>% 
    mutate(min_age_seconds = min(age_seconds),
           max_age_seconds = max(age_seconds),
           .before = "time_day") %>% 
    dplyr::select(-age_seconds) %>% 
    ungroup() %>% 
    distinct()
  
  dupl_days = prv$day_num[duplicated(prv$day_num)]
  prv = prv %>%
    mutate(duplicated = ifelse(day_num %in% dupl_days, "dupl", "ok"),
           flag       = case_when(duplicated == "dupl" & correspondent_clinical_event == TRUE ~ "keep",
                                  duplicated == "dupl" & correspondent_clinical_event == FALSE ~ "discard",
                                  TRUE ~ "keep")) %>% 
    filter(flag == "keep") %>% 
    dplyr::select(-c("duplicated","flag"))
  
  
  if (any(str_detect(unique(prv$proximal_clinical_event), "SC"))) {
    prv = prv %>% 
      filter(proximal_clinical_event != "SC")
  }
  
  if (cs_patno  == "75403") {
    prv =  prv %>% 
      mutate(AGE_AT_VISIT            = ifelse(proximal_clinical_event == "R06" & day_num == 201, 68.5, AGE_AT_VISIT),
             proximal_clinical_event = ifelse(proximal_clinical_event == "R06" & day_num == 201, "V05", proximal_clinical_event),
      ) %>% 
      distinct()
  }
  
  ### Heart data 2 ----
  ### Time resolution: each row is an hour and when you plot it or compute the slope you need to use day_hour_index as x axis
  pulse_rate = right_join(meta_cs, 
                          sens.list$pulserate.fst %>% 
                            filter(subject == cs_patno),
                          by = c("subject", "age_seconds", "time_day")) %>% 
    group_by(day_num, time_local) %>% 
    mutate(day_hour_index = cur_group_id()) %>%  
    ungroup()
  
  ### Porting pulse rate to day resolution ---
  ### We need to fix the day num duplication due to age in seconds
  pulse_rate = pulse_rate %>% 
    group_by(day_num) %>% 
    summarise(weigthed_mean_pulse_rate = weighted.mean(hourly_mean_pulse_rate, w = sample_count))
  
  pulse_rate = left_join(meta_cs,pulse_rate, by = "day_num")
  
  ### Filtering data to cover 1 year of the study ----
  pulse_rate = pulse_rate %>% filter(day_num <= (one_year_in_weeks+weeks_of_tolerance)*7)
  
  # Fix the day num duplication due to age in seconds
  pulse_rate = pulse_rate %>% 
    group_by(day_num) %>% 
    mutate(min_age_seconds = min(age_seconds),
           max_age_seconds = max(age_seconds),
           .before = "time_day") %>% 
    dplyr::select(-age_seconds) %>% 
    ungroup() %>% 
    distinct()
  
  dupl_days = pulse_rate$day_num[duplicated(pulse_rate$day_num)]
  pulse_rate = pulse_rate %>%
    mutate(duplicated = ifelse(day_num %in% dupl_days, "dupl", "ok"),
           flag       = case_when(duplicated == "dupl" & correspondent_clinical_event == TRUE ~ "keep",
                                  duplicated == "dupl" & correspondent_clinical_event == FALSE ~ "discard",
                                  TRUE ~ "keep")) %>% 
    filter(flag == "keep") %>% 
    dplyr::select(-c("duplicated","flag"))
  
  if (any(str_detect(unique(pulse_rate$proximal_clinical_event), "SC"))) {
    pulse_rate = pulse_rate %>% 
      filter(proximal_clinical_event != "SC")
  }
  
  if (cs_patno  == "75403") {
    pulse_rate =  pulse_rate %>% 
      mutate(AGE_AT_VISIT            = ifelse(proximal_clinical_event == "R06" & day_num == 201, 68.5, AGE_AT_VISIT),
             proximal_clinical_event = ifelse(proximal_clinical_event == "R06" & day_num == 201, "V05", proximal_clinical_event),
      ) %>% 
      distinct()
  }
  
  # Padding the time series ----
  pad_max   = max(nrow(inbed), nrow(sleepmetrics), nrow(prv), nrow(pulse_rate), nrow(step_count_filtered))
  quant_seq = round(quantile(seq(1, pad_max), c(0.25, 0.50, 0.75, 1)))
  df_days   = data.frame(subject    = cs_patno, 
                       day_num    = seq(1,pad_max, by = 1))
  
  inbed               = left_join(df_days, inbed, by = c("subject", "day_num"))
  sleepmetrics        = left_join(df_days, sleepmetrics, by = c("subject", "day_num"))
  prv                 = left_join(df_days, prv, by = c("subject", "day_num"))
  pulse_rate          = left_join(df_days, pulse_rate, by = c("subject", "day_num"))
  step_count_filtered = left_join(df_days, step_count_filtered, by = c("subject", "day_num"))
  
  # Merging the sleep dataframes ----
  keys = c("subject", "day_num","daily_minutes_on", "daily_prop_of_use",
           "proximal_clinical_event", "correspondent_clinical_event", "AGE_AT_VISIT")
  inbed = inbed %>% dplyr::select(all_of(keys), inbed_time)
  sleepmetrics = sleepmetrics %>% dplyr::select(all_of(keys),names(sleepmetrics)[12:19])
  sleep = left_join(inbed, sleepmetrics,  by = c("subject", "day_num", "daily_minutes_on",
                                                 "daily_prop_of_use", "proximal_clinical_event",
                                                 "correspondent_clinical_event", "AGE_AT_VISIT"))
  
  # Imputatation with Spline and LM with outlier capping by quarter
  sleep = sleep %>% 
    mutate(sleep_is_imputed    = ifelse(is.na(inbed_time) |is.na(total_light_nrem_time_ms), "imputed", "observed"),
           quarter             = case_when(day_num <= quant_seq[[1]] ~ "q1",
                                           day_num > quant_seq[[1]] & day_num <= quant_seq[[2]] ~ "q2",
                                           day_num >quant_seq[[2]] & day_num <= quant_seq[[3]] ~ "q3",
                                           day_num >quant_seq[[3]] ~ "q4")) %>%
    group_by(quarter) %>% 
    mutate(across(.cols = inbed_time:total_light_nrem_time_ms,
                  .fns  = ~ zoo::na.spline(.x, maxgap = 7))) %>% 
    mutate(across(.cols = inbed_time:total_light_nrem_time_ms,
                  .fns = ~ ifelse(.x < 0, 0, .x))) %>% 
    ungroup() %>% 
    mutate(inbed_time = dlookr::imputate_outlier(.data = ., inbed_time, method = "capping", no_attrs = TRUE),
           total_sleep_time_ms = dlookr::imputate_outlier(.data = .,total_sleep_time_ms, method = "capping", no_attrs = TRUE),
           wake_after_sleep_onset_ms = dlookr::imputate_outlier(.data = .,wake_after_sleep_onset_ms, method = "capping", no_attrs = TRUE),
           sleep_efficiency = dlookr::imputate_outlier(.data = .,sleep_efficiency, method = "capping", no_attrs = TRUE),
           num_awakenings = dlookr::imputate_outlier(.data = .,num_awakenings, method = "capping", no_attrs = TRUE),
           total_nrem_time_ms = dlookr::imputate_outlier(.data = ., total_nrem_time_ms, method = "capping", no_attrs = TRUE),
           total_rem_time_ms = dlookr::imputate_outlier(.data = .,total_rem_time_ms, method = "capping", no_attrs = TRUE),
           total_deep_nrem_time_ms = dlookr::imputate_outlier(.data = .,total_deep_nrem_time_ms, method = "capping", no_attrs = TRUE),
           total_light_nrem_time_ms = dlookr::imputate_outlier(.data = .,total_light_nrem_time_ms, method = "capping", no_attrs = TRUE))
  
  # create matrix for predictorMatrix argument
  dim_names = 
    sleep %>%
    dplyr::select(day_num, inbed_time:total_light_nrem_time_ms) %>% 
    names()
  
  pred_mat =
    matrix(data = 0,
           nrow = length(dim_names),
           ncol = length(dim_names),
           dimnames = list(dim_names,dim_names))
  
  
  pred_mat[2:10, 1] = 1
  
  sleep.binder = mice::mice(data = sleep %>% dplyr::select(day_num, inbed_time:total_light_nrem_time_ms),
                            method = "norm.predict",
                            predictorMatrix = pred_mat,
                            printFlag = FALSE,
                            remove.collinear = FALSE) %>% 
    complete()
  
  sleep = left_join(sleep %>% dplyr::select(-names(sleep)[8:16]),
                    sleep.binder, by = "day_num") %>% 
    relocate(sleep_is_imputed, .after=everything()) %>% 
    relocate(quarter, .after = daily_prop_of_use)
  
  # Imputing the step count data frame ----
  step_count_filtered = step_count_filtered %>% 
    dplyr::select(subject, day_num, daily_step_count_sum) %>% 
    mutate(step_is_imputed = ifelse(is.na(daily_step_count_sum), "imputed", "observed"),
           quarter         = case_when(day_num <= quant_seq[[1]] ~ "q1",
                                           day_num > quant_seq[[1]] & day_num <= quant_seq[[2]] ~ "q2",
                                           day_num >quant_seq[[2]] & day_num <= quant_seq[[3]] ~ "q3",
                                           day_num >quant_seq[[3]] ~ "q4")) %>% 
    group_by(quarter) %>% 
    mutate(daily_step_count_sum = zoo::na.spline(daily_step_count_sum, maxgap = 7)) %>%
    ungroup() %>% 
    mutate(daily_step_count_sum = ifelse(daily_step_count_sum < 0, 0, daily_step_count_sum)) %>% 
    dplyr::select(-quarter) %>% 
    mutate(daily_step_count_sum = dlookr::imputate_outlier(.data = ., xvar = daily_step_count_sum, method = "capping", no_attrs = TRUE))
  
  if (sum(is.na(step_count_filtered$daily_step_count_sum)) > 0) {
    step.binder = mice::mice(data = step_count_filtered %>% dplyr::select(day_num, daily_step_count_sum),
                             method ="norm.predict",
                             predictorMatrix = mice::quickpred(step_count_filtered %>% dplyr::select(day_num, daily_step_count_sum),
                                                               include = "day_num"),
                             printFlag = FALSE) %>% 
      complete()
    
    step_count_filtered = left_join(step_count_filtered %>% dplyr::select(-daily_step_count_sum),
                                    step.binder, by = "day_num") %>% 
      relocate(step_is_imputed, .after=everything()) 
  }
  
  cs_imp = 
    left_join(sleep,
              step_count_filtered %>% 
                mutate(daily_step_count_sum = round(daily_step_count_sum)),
              by = c("subject", "day_num")) %>%
    relocate(daily_step_count_sum, .before = sleep_is_imputed)
  
  # Heart imputations
  heart = left_join(prv %>% dplyr::select(subject, day_num, weigthed_mean_rmssd, weigthed_median_rmssd, weigthed_mean_cv_rmssd),
                    pulse_rate %>% dplyr::select(subject, day_num, weigthed_mean_pulse_rate),
                    by = c("subject", "day_num"))
  heart = heart %>% 
    mutate(heart_is_imputed = ifelse(is.na(weigthed_mean_rmssd) | is.na(weigthed_mean_pulse_rate), "imputed", "observed"),
           quarter         = case_when(day_num <= quant_seq[[1]] ~ "q1",
                                           day_num > quant_seq[[1]] & day_num <= quant_seq[[2]] ~ "q2",
                                           day_num >quant_seq[[2]] & day_num <= quant_seq[[3]] ~ "q3",
                                           day_num >quant_seq[[3]] ~ "q4")) %>% 
    group_by(quarter) %>% 
    mutate(across(.cols = contains("weigthed"), .fns = ~ zoo::na.spline(.x, maxgap = 7))) %>% 
    ungroup() %>% 
    mutate(across(.cols = contains("weigthed"), .fns = ~ ifelse(.x <0, 0, .x))) %>% 
    dplyr::select(-quarter) %>% 
    mutate(weigthed_mean_rmssd = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_rmssd, method = "capping", no_attrs = T),
           weigthed_median_rmssd =dlookr::imputate_outlier(.data = ., xvar = weigthed_median_rmssd, method = "capping", no_attrs = T),
           weigthed_mean_cv_rmssd = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_cv_rmssd, method = "capping", no_attrs = T),
           weigthed_mean_pulse_rate = dlookr::imputate_outlier(.data = ., xvar = weigthed_mean_pulse_rate, method = "capping", no_attrs = T))
  
  if (sum(is.na(heart)) > 0) {
    heart.binder = mice::mice(data = heart %>% dplyr::select(day_num, contains("weigthed")),
                              method ="norm.predict",
                              predictorMatrix = mice::quickpred(heart %>% dplyr::select(day_num, contains("weigthed")),
                                                                include = "day_num",
                                                                exclude = names(heart)[3:6]),
                              printFlag = FALSE) %>% 
      complete()
    
    heart = left_join(heart %>% dplyr::select(-names(heart)[3:6]),
                      heart.binder, by = "day_num") %>% 
      relocate(heart_is_imputed, .after=everything())
  }
  
  cs_imp = left_join(cs_imp, heart, by = c("subject", "day_num"))
  cs_imp = cs_imp %>% distinct()
  cs_imp = cs_imp %>% 
    mutate(sex         = unique(clinical.list$covariate_for_updrs$SEX[clinical.list$covariate_for_updrs$PATNO == cs_patno]),
           age_seconds = seq(min(meta_cs$age_seconds, na.rm = T), min(meta_cs$age_seconds, na.rm = T) +((pad_max-1)*86400), by = 86400),
           diagnosis_updated  = unique(clinical.list$covariate_for_updrs$diagnosis_updated[clinical.list$covariate_for_updrs$PATNO == cs_patno]),
           .after      = "subject",
           daily_minutes_on = replace_na(daily_minutes_on, 0),
           daily_prop_of_use= replace_na(daily_prop_of_use, 0))
  
  cs_imp = cs_imp %>% 
    mutate(across(proximal_clinical_event:AGE_AT_VISIT, 
                  ~ ifelse(row_number() == 1 & is.na(.x), sort(unique(.x))[1], .x))) %>% 
    mutate(across(proximal_clinical_event:AGE_AT_VISIT,
                  .fns = ~ zoo::na.locf(.x))) %>% 
    relocate(sleep_is_imputed, step_is_imputed,
             .before = heart_is_imputed)
  
  cs_list[[i]] = cs_imp
}

saveRDS(cs_list, "cs list temp.rds")

cs_list = readRDS("cs list temp.rds")
cs      = cs_list %>% reduce(bind_rows)

# Adding the subgroup information
cs=left_join(cs %>% rename(PATNO = subject),
             bind_rows(analytic_dataset$PD %>% filter(PATNO %in% unique(cs$subject)) %>% dplyr::select(PATNO, Subgroup),
                       analytic_dataset$Prodromal %>% filter(PATNO %in%  unique(cs$subject)) %>% dplyr::select(PATNO, Subgroup)),by = "PATNO") %>% 
  relocate(Subgroup, .before = day_num)


# Real correspondent clinical visit creation ----
# This is the real date when the subject had the visit (is_visit_day)
cs = cs %>% 
  mutate(delta_age = abs((age_seconds/31556952)-AGE_AT_VISIT)) %>% 
  group_by(PATNO, proximal_clinical_event) %>% 
  mutate(is_visit_day = delta_age == min(delta_age), .after = correspondent_clinical_event, .keep = "unused"
         ) %>% 
  ungroup()

# Removing sensor data that exceed the day of the last clinical visit
# i.e. If you have the last visit at day 303 we don't need days from 304 to 364
# ****WARNING**** If you need the clean temporal series for each subject you should avoid this step
# ++++WARNING++++ If you cut based the visit day some subjects will have less days than 364 +/- 2 months. It is normal, but we have to remove some subjects.
cs = cs %>% 
  group_by(PATNO) %>% 
  mutate(flag  = ifelse(is_visit_day == TRUE, day_num, NA_integer_), .after = is_visit_day) %>% 
  filter(day_num <= max(flag, na.rm = TRUE)) %>% 
  dplyr::select(-flag) %>% 
  ungroup()

cs %>%
  dplyr::select(PATNO, day_num, is_visit_day) %>% 
  filter(is_visit_day == TRUE) %>% 
  group_by(PATNO) %>% 
  mutate(duration = max(day_num) - min(day_num)) %>%  
  dplyr::select(-c(day_num, is_visit_day)) %>% 
  distinct() %>% 
  filter(duration >= (365-62))

cs_patno_to_use = cs %>%
  dplyr::select(PATNO, day_num, is_visit_day) %>% 
  filter(is_visit_day == TRUE) %>% 
  group_by(PATNO) %>% 
  mutate(duration = max(day_num) - min(day_num)) %>% 
  dplyr::select(-c(day_num, is_visit_day)) %>% 
  distinct() %>% 
  filter(duration >= (365-62)) %>% 
  .$PATNO %>% unique()

cs = cs %>% 
  filter(PATNO %in% cs_patno_to_use)

# So these are real numbers for cs
cs %>% 
  dplyr::select(PATNO, diagnosis_updated) %>%
  distinct() %>% 
  xtabs(~ diagnosis_updated, data = .)

pd_cs = cs %>% filter(diagnosis_updated == "Parkinson's Disease")

# 79 PD with sensors data available
pd_patno = pd_cs$PATNO %>% unique()

# But only 79 have more than 1 visit in the first year
pd_patno = pd_cs %>% 
  dplyr::select(PATNO, proximal_clinical_event) %>% 
  distinct() %>% 
  arrange(PATNO) %>% 
  group_by(PATNO) %>% 
  mutate(n_visits = n()) %>%
  dplyr::select(-proximal_clinical_event) %>% 
  distinct() %>% 
  filter(n_visits > 1) %>% .$PATNO

# This is the visits matcher
pd_binder = pd_cs %>% 
  dplyr::select(PATNO, proximal_clinical_event) %>% 
  distinct() %>% 
  arrange(PATNO) %>% 
  group_by(PATNO) %>% 
  mutate(n_visits = n()) %>% 
  filter(n_visits > 1)

pd_binder %>% 
  filter(PATNO == 74067)

# Find the clinical variables for each PATNO
p1 = read.csv(normalizePath("E:\\ppmi_sensors\\data\\raw\\study_data\\Motor_Assessments\\MDS-UPDRS_Part_I.csv"))
p2 = read.csv(normalizePath("E:\\ppmi_sensors\\data\\raw\\study_data\\Motor_Assessments\\MDS_UPDRS_Part_II__Patient_Questionnaire.csv"))
p3 = read.csv(normalizePath("E:\\ppmi_sensors\\data\\raw\\study_data\\Motor_Assessments\\MDS_UPDRS_Part_III.csv"))
p4 = read.csv(normalizePath("E:\\ppmi_sensors\\data\\raw\\study_data\\Motor_Assessments\\MDS-UPDRS_Part_IV__Motor_Complications.csv"))
adl= read.csv(normalizePath("E:\\ppmi_sensors\\data\\raw\\study_data\\Motor_Assessments\\Modified_Schwab___England_Activities_of_Daily_Living.csv"))

updrs = plyr::join_all(list(left_join(pd_binder, p1 %>% 
            filter(PATNO %in% pd_patno) %>% 
            dplyr::select(PATNO, EVENT_ID, NP1RTOT) %>% rename(proximal_clinical_event = EVENT_ID),
          by = c("PATNO", "proximal_clinical_event")),
 left_join(pd_binder,
          p2 %>% 
            filter(PATNO %in% pd_patno) %>% 
            dplyr::select(PATNO, EVENT_ID, NP2PTOT) %>% rename(proximal_clinical_event = EVENT_ID),
          by = c("PATNO", "proximal_clinical_event"))))

t1 = left_join(pd_binder, 
               p3 %>% 
                 filter(PATNO %in% pd_patno) %>% 
                 dplyr::select(PATNO, EVENT_ID, PDSTATE,NHY,NP3TOT) %>% rename(proximal_clinical_event = EVENT_ID) %>% 
                 mutate(PDSTATE = ifelse(PDSTATE == "", NA_character_, PDSTATE)),
               by = c("PATNO", "proximal_clinical_event"))

updrs  =left_join(updrs, t1, by = c("PATNO", "proximal_clinical_event", "n_visits"))%>% 
  mutate(NHY = na_if(NHY, "UR"),
         NHY = na_if(NHY, ""))

# Filtering to remove NAs in clinical variables
updrs = updrs %>% filter(!is.na(NP3TOT)) %>% filter(!is.na(PDSTATE))

updrs = updrs %>% 
  relocate(NP3TOT, .before = NHY) %>% 
  mutate(updrs_tot = NP1RTOT + NP2PTOT +NP3TOT,
         .before = NHY) %>% 
  filter(!is.na(updrs_tot))

# 75 subject had all the clinical data so we keep only them
pd_patno = updrs %>% 
  dplyr::select(PATNO, proximal_clinical_event) %>% 
  distinct() %>% 
  arrange(PATNO) %>% 
  group_by(PATNO) %>% 
  mutate(n_visits = n()) %>%
  dplyr::select(-proximal_clinical_event) %>% 
  distinct() %>% 
  filter(n_visits > 1) %>% .$PATNO


adl = left_join(pd_binder, 
adl %>%
  dplyr::select(PATNO, EVENT_ID, MSEADLG) %>%
  rename(proximal_clinical_event = EVENT_ID) %>% 
  filter(PATNO %in% pd_patno), by = c("PATNO", "proximal_clinical_event"))

updrs = left_join(updrs, adl, by = c("PATNO", "proximal_clinical_event", "n_visits"))
updrs = updrs %>% filter(PATNO %in% pd_patno)

updrs %>% filter(PATNO == 74067)

# How many subject always ON or OFF? ----
# we have 59 subject that had always ON events
pd_on_subjects = updrs %>% 
  dplyr::select(PATNO, proximal_clinical_event, PDSTATE) %>%
  distinct() %>% 
  group_by(PATNO) %>% 
  filter(PDSTATE == "ON") %>% 
  filter(n() >= 2) %>%
  .$PATNO %>% unique()

pd_on_subjects %>% length()

# We have 43 subject that had always OFF events
pd_off_subjects = updrs %>% 
  dplyr::select(PATNO, proximal_clinical_event, PDSTATE) %>%
  distinct() %>% 
  group_by(PATNO) %>% 
  filter(PDSTATE == "OFF") %>% 
  filter(n() >= 2) %>%
  .$PATNO %>% unique()

pd_on = pd_cs %>% filter(PATNO %in% pd_on_subjects)
pd_off = pd_cs %>% filter(PATNO %in% pd_off_subjects)

pd_on %>% 
  filter(PATNO == 74067) %>% 
  dplyr::select(PATNO, day_num, proximal_clinical_event, is_visit_day) %>% 
  filter(is_visit_day == T)

pd_on = left_join(pd_on,
                  updrs %>% 
                    filter(PDSTATE == "ON") %>% 
                    dplyr::select(-c("NP1RTOT", "NP2PTOT", "PDSTATE", "NP3TOT", "NHY", "MSEADLG")), 
                  by = c("PATNO", "proximal_clinical_event")) %>% 
  mutate(updrs_tot = ifelse(is_visit_day, updrs_tot, NA_integer_))

pd_on_subjects = pd_on %>% 
  na.omit() %>% #<- This is the crucial difference. When we na omit the wrong visit goes away. So here we need to na omit and compute again duration.
  dplyr::select(PATNO, day_num, proximal_clinical_event, is_visit_day) %>% 
  filter(is_visit_day == T) %>%
  group_by(PATNO) %>% 
  mutate(flag_duration = (max(day_num) - min(day_num)) >= 287) %>% 
  filter(flag_duration == T) %>% 
  dplyr::select(-day_num, -proximal_clinical_event) %>% 
  distinct() %>% .$PATNO

pd_on = pd_on %>% 
  filter(PATNO %in% pd_on_subjects)

pd_off = left_join(pd_off,
                  updrs %>% 
                    filter(PDSTATE == "OFF") %>% 
                    dplyr::select(-c("NP1RTOT", "NP2PTOT", "PDSTATE", "NP3TOT", "NHY", "MSEADLG")), 
                  by = c("PATNO", "proximal_clinical_event")) %>% 
  mutate(updrs_tot = ifelse(is_visit_day, updrs_tot, NA_integer_))

pd_off_subjects = pd_off %>% 
  na.omit() %>% #<- This is the crucial difference. When we na omit the wrong visit goes away. So here we need to na omit and compute again duration.
  dplyr::select(PATNO, day_num, proximal_clinical_event, is_visit_day) %>% 
  filter(is_visit_day == T) %>%
  group_by(PATNO) %>% 
  mutate(flag_duration = (max(day_num) - min(day_num)) >= 287) %>% 
  filter(flag_duration == T) %>% 
  dplyr::select(-day_num, -proximal_clinical_event) %>% 
  distinct() %>% .$PATNO

pd_off = pd_off %>% 
  filter(PATNO %in% pd_off_subjects)

pd_on %>% 
  na.omit() %>% 
  group_by(PATNO) %>% 
  tally() %>% 
  arrange(-n)

pd_on %>% 
  filter(PATNO == 52062) %>%
  na.omit() %>% 
  #filter(updrs_tot == max(updrs_tot) | updrs_tot == min(updrs_tot)) %>% 
  ggplot() +
  aes(x = day_num,
      y = updrs_tot) +
  geom_point() + 
  geom_smooth()

all_data$pd_on %>% 
  na.omit() %>% 
  mutate(PATNO = as.factor(PATNO)) %>% 
  ggplot() +
  aes(x = day_num,
      y = PATNO) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 364, color = "red") +
  geom_vline(xintercept = 364 + 77, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 364 - 77, linetype = "dashed", color = "red") +
  theme_bw()

pd_on = left_join(pd_on, 
          age_disease,
          by = "PATNO") %>%
  relocate(age_at_diagnosis, age_at_symptoms, disease_duration_at_verily_start, time_from_symptoms_at_verily_start, .before = day_num) %>% 
  mutate(disease_duration         = disease_duration_at_verily_start+((day_num-1)*(1/365)),
         time_from_symptoms_onset = time_from_symptoms_at_verily_start+((day_num-1)*(1/365)),
         .before = day_num)

pd_off = left_join(pd_off, 
          age_disease,
          by = "PATNO") %>%
  relocate(age_at_diagnosis, age_at_symptoms, disease_duration_at_verily_start, time_from_symptoms_at_verily_start, .before = day_num) %>% 
  mutate(disease_duration         = disease_duration_at_verily_start+((day_num-1)*(1/365)),
         time_from_symptoms_onset = time_from_symptoms_at_verily_start+((day_num-1)*(1/365)),
         .before = day_num)

list(cs = cs,
     pd_on = pd_on,
     pd_off = pd_off,
     all_about_time =all_about_time) %>% saveRDS(file = "alldata.rds")

updrs_tot_to_impute =
  pd_some_visits_cs %>% 
  dplyr::select(PATNO, PDSTATE, day_num, is_visit_day, updrs_tot) %>% 
  mutate(updrs_tot   = ifelse(is_visit_day, updrs_tot, NA_integer_))

updrs_imputed = 
  updrs_tot_to_impute %>% 
  nest(-PATNO) %>% 
  mutate(model_lm = map(.x = data,
                        .f = ~ lm(formula = updrs_tot ~ day_num, data = .x)),
         updrs_tot_imputed = map2(.x = model_lm,
                                  .y = data,
                                  .f = ~ predict(.x,
                                                 data.frame(day_num = .y$day_num)))) %>% 
  dplyr::select(-model_lm) %>% 
  unnest()

pd_some_visits_cs = 
  pd_some_visits_cs %>% 
  left_join(updrs_imputed %>% 
              dplyr::select(PATNO, day_num, updrs_tot_imputed),
            by = c("PATNO", "day_num"))

age_disease <- readRDS("disease duration information.rds")
age_disease = age_disease %>% 
  dplyr::select(PATNO, age_at_diagnosis, age_at_symptoms, disease_duration_at_verily_start, time_from_symptoms_at_verily_start)

pd_some_visits_cs = left_join(pd_some_visits_cs, 
                              age_disease, by = "PATNO") %>% 
  relocate(age_at_diagnosis, age_at_symptoms, disease_duration_at_verily_start,
           time_from_symptoms_at_verily_start, .before = diagnosis_updated) %>% 
  group_by(PATNO) %>% 
  mutate(age_at_diagnosis = replace_na(age_at_diagnosis, min(age_seconds)/31556952),
         age_at_symptoms = replace_na(age_at_symptoms, min(age_seconds)/31556952),
         disease_duration_at_verily_start = replace_na(disease_duration_at_verily_start, 0),
         time_from_symptoms_at_verily_start = replace_na(time_from_symptoms_at_verily_start, 0))

pd_some_visits_cs = pd_some_visits_cs %>%
  group_by(PATNO) %>% 
  mutate(across(inbed_time:weigthed_mean_pulse_rate,
                ~ TTR::EMA(x = .x, ratio = 0.2),
                .names = "{.col}_ema")) %>% 
  ungroup()

saveRDS(pd_some_visits_cs, file = "verily watch data and clinical variable capped at the last visits for parkinson cohort.rds")
saveRDS(cs, file = "verily watch data capped at the last visit for cohort comparisons.rds")

pd_some_visits_cs = pd_some_visits_cs %>%
  group_by(PATNO) %>% 
  mutate(across(inbed_time:weigthed_mean_pulse_rate,
                ~ TTR::EMA(x = .x, ratio = 0.2),
                .names = "{.col}_ema"))

temp21 = 
pd_some_visits_cs %>% 
  mutate(across(.cols = c(day_num, inbed_time_ema:weigthed_mean_pulse_rate_ema), 
                .fns = ~ scale(.x)))

pd_some_visits_cs %>% 
  filter(PATNO == 14281) %>% 
  ggplot() +
  aes(x = day_num,
      y = sleep_efficiency) +
  geom_line() +
  theme_bw()+
  ggpmisc::stat_poly_line()

pd_some_visits_cs %>% 
  filter(PATNO == 14281) %>% 
  ggplot() +
  aes(x = day_num,
      y = sleep_efficiency_ema) +
  geom_line() +
  theme_bw() +
  ggpmisc::stat_poly_line()

temp21 %>% 
  filter(PATNO == 14281) %>% 
  ggplot() +
  aes(x = day_num,
      y = sleep_efficiency_ema) +
  geom_line() +
  theme_bw() +
  ggpmisc::stat_poly_line()

temp21 %>% 
  filter(PATNO == 14281) %>% 
  ggplot() +
  aes(x = day_num,
      y = total_nrem_time_ms_ema) +
  geom_line() +
  theme_bw() +
  ggpmisc::stat_poly_line()

pd_some_visits_cs %>% 
  filter(PATNO == 14281) %>% 
  ggplot() +
  aes(x = day_num,
      y = updrs_tot_imputed) +
  geom_line() +
  theme_bw()

fit_tot_1 = lme4::lmer(updrs_tot_imputed ~ daily_step_count_sum_ema*day_num + (1|PATNO),
                       data = temp21 %>% filter(!is.na(sleep_efficiency_ema)),
                       REML = FALSE)
fit_tot_2 = lme4::lmer(updrs_tot_imputed ~ day_num +(1|PATNO),
                       data = temp21 %>% filter(!is.na(sleep_efficiency_ema)),
                       REML = FALSE)

anova(fit_tot_1, fit_tot_2)

car::Anova(fit_tot_1)

sjPlot::plot_model(fit_tot_1,
                   type  = "pred",
                   terms = "sleep_efficiency_ema")

#sjPlot::plot_residuals(fit_tot_1, show.resid = TRUE, show.pred = TRUE, show.ci = TRUE)
sjPlot::plot_models(fit_tot_1, fit_tot_2, show.p = T)

#performance::check_model(fit_tot_1)
#autoplot(fit_tot_1)
# broom::augment(fit_tot_1)
# broom::glance(fit_tot_1)

performance::performance(fit_tot_1)
performance::check_collinearity(fit_tot_1) %>% plot()
performance::check_distribution(fit_tot_1) %>% plot()
performance::check_heteroscedasticity(fit_tot_1) %>% plot()
performance::check_predictions(fit_tot_1) %>% plot()
performance::check_outliers(fit_tot_1, method = "cook") %>% plot()
rsq::rsq(fit_tot_1)
report::report(fit_tot_1)

prova = pd_some_visits_cs %>% 
  filter(correspondent_clinical_event == TRUE) %>% 
  group_by(PATNO, proximal_clinical_event) %>% 
  mutate(mean = mean(daily_step_count_sum)) %>% 
  dplyr::select(PATNO, proximal_clinical_event, mean, updrs_tot) %>% 
  distinct() %>% 
  group_by(PATNO) %>% 
  mutate(time = row_number(),
         n = n()) %>% 
  filter(n > 1) %>% 
  mutate(mean = mean/1000,
         PATNO = as.factor(PATNO))

prova %>% 
  ggplot() +
  aes(x = mean,
      y = updrs_tot) +
  geom_point() +
  geom_smooth() +
  ggpmisc::stat_quant_band() +
  ggpmisc::stat_quant_line() +
  ggpmisc::stat_quant_eq()

library(lmerTest)
library(lme4)

fit1 = lme4::lmer(updrs_tot ~ mean*time +(1|PATNO), data = prova, REML = FALSE)
fit2 = lme4::lmer(updrs_tot ~ time      +(1|PATNO), data = prova, REML = FALSE)

anova(fit1, fit2)

fit = lme4::lmer(updrs_tot ~ mean*time +(1|PATNO), data = prova, REML = TRUE)

summary(fit)
car::Anova(fit)
sjPlot::plot_model(fit,
                   type  = "pred",
                   terms = "mean")

sjPlot::plot_residuals(fit, show.resid = TRUE, show.pred = TRUE, show.ci = TRUE)
sjPlot::plot_models(fit, fit2)

#performance::check_model(fit)
#autoplot(fit)
# broom::augment(fit)
# broom::glance(fit)

performance::performance(fit)
performance::compare_performance(fit1, fit2, rank = TRUE) %>% report::report()
performance::compare_performance(fit1, fit2, rank = TRUE) %>% plot()
performance::check_collinearity(fit) %>% plot()
performance::check_distribution(fit) %>% plot()
performance::check_heteroscedasticity(fit) %>% plot()
performance::check_predictions(fit) %>% plot()
performance::check_outliers(fit, method = "cook") %>% plot()
rsq::rsq(fit)
report::report(fit)

predict(fit,
        data.frame(PATNO = 666,
                   mean = c(5.2210709, 7.0331741),
                   time = c(1,2)),
        allow.new.levels = TRUE)

library(bayestestR)
# Bayes factor - models
mo0 <- lm(Sepal.Length ~ 1, data = iris)
mo1 <- lm(Sepal.Length ~ Species, data = iris)
mo2 <- lm(Sepal.Length ~ Species + Petal.Length, data = iris)
mo3 <- lm(Sepal.Length ~ Species * Petal.Length, data = iris)
BFmodels <- bayesfactor_models(mo1, mo2, mo3, denominator = mo0)

report::report(BFmodels)

fit3 = lme4::lmer(updrs_tot ~ time + (1|PATNO), data = prova,REML = F)
fit4 = lme4::lmer(updrs_tot ~ 1 + (1|PATNO), data = prova,REML = F)

summary(fit3)
car::Anova(fit3)

anova(fit3, fit4)





# Bayesian fit
bfit = rstanarm::stan_glmer(updrs_tot ~ mean*time +(1|PATNO), data = prova)
performance::performance(bfit)
report::report(bfit)
sjPlot::plot_model(bfit, type = "eff", show.p = T)
sjPlot::plot_model(bfit,
                   type  = "pred",
                   terms = "mean")
performance::check_distribution(bfit) %>% plot()
performance::check_predictions(bfit) 
performance::posterior_predictive_check(bfit)
