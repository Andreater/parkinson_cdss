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
library(glmulti)
library(fpp3)

parent          = dirname(getwd())
data.path       = file.path(parent, "data")
train.data.path = file.path(data.path, "intermediate")



load(file.path(train.data.path, "stl slope data.rdata"))

time_on = lapply(stl_decomposition_pd_on_list$decomposed_pd_on, function(x) x %>%
                    dplyr::select(PATNO, sex, age_at_diagnosis,
                                  age_at_symptoms, time_from_symptoms_at_verily_start, disease_duration,
                                  contains("slope")) %>% 
                    mutate(max_disease_duration = max(disease_duration), .keep = "unused", .before = "updrs_slope") %>% 
                    distinct() %>% 
                    ungroup() %>% 
                    mutate(PATNO = as.factor(PATNO),
                           sex   = as.factor(sex)))

check_outliers(time_on[[53]] %>% dplyr::select(where(is.numeric))) %>% plot()

time_on_correlation = time_on[[53]] %>% 
  dplyr::select(where(is.numeric)) %>% 
  rstatix::cor_test(method = "pearson") %>% 
  mutate(p.sig  = p<0.05, 
         .after = p)

time_on_correlation %>% 
  rstatix::as_cor_mat() %>%
  ggcorrplot::ggcorrplot(method = "square",
                         type   = "full",
                         insig  = "blank", 
                         sig.level = 0.05,
                         lab = T)

# This is for internal use // easier to filter and inspect
time_on[[52]] %>% 
  dplyr::select(where(is.numeric)) %>% 
  correlate(method = "pearson", quiet = T) %>% 
  stretch(na.rm = T, remove.dups = T) %>% 
  filter(abs(r) > 0.5)

# To be removed after collinearity check
collinear_predictors = c("age_at_symptoms",
                         "time_from_symptoms_at_verily_start",
                         "weigthed_median_rmssd_box_trend_z",
                         "total_nrem_time_ms_box_trend_z")

# This is potentially useful for the paper
time_on[[52]] %>%
  dplyr::select(where(is.numeric)) %>%
  correlate(method = "pearson", quiet = T) %>% 
  focus("updrs_slope") %>% 
  arrange(updrs_slope) %>%
  ggplot() +
  aes(x = fct_reorder(term, updrs_slope),
      y = updrs_slope) +
  geom_col() +
  labs(x = "",
       y = "Pearson r",
       title = "Focus on UPDRS correlation") + 
  geom_hline(yintercept = 0.1) +
  geom_hline(yintercept =-0.1) +
  coord_flip()

## PD DV ON ----
#https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/tweedie_distribution.html
qqnorm(y = time_on[[53]]$updrs_slope)
time_on[[53]]$updrs_slope %>% check_distribution() %>% plot()
time_on[[53]]$updrs_slope %>% fitdistrplus::descdist()
## PD DV OFF ----
#https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/tweedie_distribution.html
qqnorm(y = time_off[[53]]$updrs_slope)
time_off[[53]]$updrs_slope %>% check_distribution() %>% plot()
time_off[[53]]$updrs_slope %>% fitdistrplus::descdist()

check_outliers(time_on[[53]] %>% dplyr::select(updrs_slope), method = c("zscore", "iqr","bci")) %>% plot()

library(LEAP)
prova = glmulti(y = updrs_slope ~ sex + age_at_diagnosis + age_at_symptoms + time_from_symptoms_at_verily_start +
          max_disease_duration + inbed_time_slope + total_sleep_time_slope + wake_slope +
          sleep_efficiency_slope + num_awakenings_slope + total_nrem_time_slope + total_rem_time_slope + 
          total_deep_nrem_time_slope + total_light_nrem_time_slope + daily_step_count_slope + weigthed_mean_rmssd_slope +
          weigthed_mean_cv_rmssd_slope + weigthed_mean_rmssd_slope,
        data        = time_on[[53]],
        crit        = aicc,
        level       = 1,
        method      = "I",
        family      = gaussian,
        fitfunction = glm,
        confsetsize = 100,
        plotty      = F)



model_info = list()
for (i in 1:length(time_on)) {
  bs = glm(updrs_slope ~ 1, data = time_on[[i]])
  fit= glm(updrs_slope ~   total_rem_time_slope, data = time_on[[i]])
  
  
  btb       = suppressWarnings(anova(bs, fit, test = "Chisq") %>% broom::tidy() %>% na.omit() %>% .$p.value %>% unique())
  comp_perf = compare_performance(bs, fit, rank = T) %>% as_tibble() %>% pivot_longer(cols = R2:Performance_Score)
  fit %>% broom::glance()
  fit %>% broom::augment()
  fit %>% broom::tidy()
  
  temp = data.frame(week                 = names(time_on)[i],
                    better_than_baseline = btb)
  model_info[[i]] = bind_cols(temp, comp_perf)
}


model_info %>% 
  reduce(bind_rows) %>% 
  filter(name %in% c("RMSE", "R2","BIC_wt", "Performance_Score")) %>% 
  ggplot() +
  aes(x = as.numeric(week),
      y = value,
      group = Name,
      colour = Name) +
  scale_y_continuous() +
  geom_line() +
  facet_wrap(~ name, scales = "free")
model_info %>% 
  reduce(bind_rows) %>% 
  filter(name %in% c("RMSE")) %>% 
  ggplot() +
  aes(x = Name,
      y = value) +
  geom_boxplot()


stl_decomposition_pd_on_list$stl_list[[52]] %>% 
  filter(name == "total_rem_time_ms_box") %>% 
  pull(dcmp) %>% .[[1]] %>%  components() %>%  autoplot() + theme(legend.position = "none")

dlookr::diagnose_outlier(prova) %>% plot()
performance::check_outliers(prova, method = c("zscore","zscore_robust", "lof")) %>% plot()

pd %>% filter(PATNO == 51731) %>% filter(PATNO == 51731) %>% nrow()
# Removing duplicated rows
pd = pd %>% distinct()

# Fixing time resolution // Duplicated days
pd = pd %>% 
  group_by(PATNO, PDSTATE, day_num) %>% 
  mutate(day_index = row_number(),
         .after= PATNO) %>% 
  filter(day_index == 1) %>% 
  dplyr::select(-day_index) %>% 
  ungroup()

# Drop useless variables -> We have to perform EMA again
pd = pd %>% 
  dplyr::select(-contains("ema"))

pd = pd %>% relocate(PDSTATE, .before = age_seconds)

# Values in the predictors that were equal to zero due to outlier capping where recoded with a rollmean with window = 7
pd = pd %>% 
  mutate(across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ na_if(.x, 0))) %>% 
  ungroup()

pd = pd %>% 
  group_by(PATNO, quarter) %>% 
  mutate(across(.cols = inbed_time:weigthed_mean_pulse_rate,
                .fns  = ~ ifelse(is.na(.x) | .x < 0, 
                        Mode(.x, na.rm = T),
                        .x))) %>% 
  ungroup()

# Padding the most recent value to cover a total of 6 nas.
pd = pd %>% 
  mutate(inbed_time = replace_na(inbed_time, 8.408333),
         total_deep_nrem_time_ms =replace_na(total_deep_nrem_time_ms, 195962.845))

# If sleep efficiency is to high then cap it to the third quartile
pd = pd %>% mutate(sleep_efficiency = ifelse(sleep_efficiency > 1, 0.77, sleep_efficiency))

# inbed_time was capped
pd = pd %>% mutate(inbed_time = case_when(inbed_time > 24  ~ 12.94167,
                                     inbed_time < 3.658333333 ~ 3.658333333,
                                     TRUE ~ inbed_time))

pd = pd %>% 
  mutate(total_sleep_time_ms = ifelse(total_sleep_time_ms > 42269730, 24360000, total_sleep_time_ms))

pd = pd %>% 
  mutate(num_awakenings = round(num_awakenings),
         num_awakenings = dlookr::imputate_outlier(.data = .,
                                                   xvar = num_awakenings,
                                                   method = "capping",
                                                   no_attrs = T)) 

pd = pd %>%
  mutate(total_nrem_time_ms      = dlookr::imputate_outlier(.data = ., xvar = total_nrem_time_ms,method = "capping", no_attrs = T), 
         total_rem_time_ms       = dlookr::imputate_outlier(.data = ., xvar = total_rem_time_ms,method = "capping", no_attrs = T), 
         total_deep_nrem_time_ms = dlookr::imputate_outlier(.data = ., xvar = total_deep_nrem_time_ms,method = "capping", no_attrs = T),
         total_light_nrem_time_ms= dlookr::imputate_outlier(.data = ., xvar = total_light_nrem_time_ms,method = "capping", no_attrs = T))

pd %>% filter(PATNO == 51731) %>% nrow()

# number of participants = 75
pd$PATNO %>% unique() %>% length()

# We have only 6 subject that have at least 1 NA in PDSTATE
pd %>% 
  dplyr::select(PATNO, proximal_clinical_event, PDSTATE, updrs_tot) %>% 
  distinct() %>% 
  filter(is.na(PDSTATE)) %>% .$PATNO %>% unique() %>% length()

# Impute NAs in PDSTATE ----
prova = pd %>% 
  dplyr::select(PATNO, AGE_AT_VISIT,
                age_at_diagnosis, 
                age_at_symptoms,
                disease_duration_at_verily_start, 
                time_from_symptoms_at_verily_start,
                sex, proximal_clinical_event, PDSTATE, updrs_tot) %>% 
  group_by(PATNO) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(flag = ifelse(lag(updrs_tot) > updrs_tot & lag(PDSTATE) == "OFF", "probably on", PDSTATE))


fit = glmer(as.factor(PDSTATE) ~ AGE_AT_VISIT + sex + updrs_tot + age_at_diagnosis + 
              age_at_symptoms + disease_duration_at_verily_start + time_from_symptoms_at_verily_start +
              (1|PATNO),control = glmerControl(optimizer="Nelder_Mead",
                                               calc.derivs = F,
                                               nAGQ0initStep = F), 
            data   = prova,
            family = "binomial")

pdstate_brigde = prova %>% 
  mutate(ONprob     = predict(fit, prova, allow.new.levels = T, type = "response"),
         pred.class = ifelse(ONprob > 0.50, "ON", "OFF")) %>% 
  #na.omit() %>% 
  mutate(hit = ifelse(pred.class == PDSTATE, 1, 0),
         acc = round(sum(hit)/nrow(.),2)) %>% 
  dplyr::select(PATNO, proximal_clinical_event, pred.class)

pd = left_join(pd %>% ungroup(),
          pdstate_brigde, by = c("PATNO", "proximal_clinical_event")) %>%
  mutate(PDSTATE = ifelse(is.na(PDSTATE), pred.class, PDSTATE)) %>% 
  dplyr::select(-pred.class)

rm(pdstate_brigde, fit, prova)

pd = pd %>% distinct()

# How many subject always ON or OFF? ----
# we have 71 subject that had always ON events
pd_on_subjects = pd %>% 
  dplyr::select(PATNO, proximal_clinical_event, PDSTATE, is_visit_day) %>%
  filter(is_visit_day == TRUE) %>% 
  distinct() %>% 
  group_by(PATNO) %>% 
  filter(PDSTATE == "ON") %>% 
  filter(n() >= 2) %>%
  .$PATNO %>% unique()

pd_on_subjects %>% length()

# We have 48 subject that had always OFF events
pd_off_subjects = pd %>% 
  dplyr::select(PATNO, proximal_clinical_event, PDSTATE, is_visit_day) %>%
  filter(is_visit_day == TRUE) %>% 
  distinct() %>% 
  group_by(PATNO) %>% 
  filter(PDSTATE == "OFF") %>% 
  filter(n() >= 2) %>%
  .$PATNO %>% unique()

pd_off_subjects %>% length()

# Split by PD state ----
#pd_on_subjects =
pd %>% 
  filter(PATNO %in% pd_on_subjects) %>%
  filter(PATNO == 3021) %>% 
  filter(is_visit_day == TRUE) %>% 
  dplyr::select(PATNO, PDSTATE, is_visit_day, day_num, proximal_clinical_event, updrs_tot, updrs_tot_imputed)

# Split by PD state ----
pd_on_subjects = pd %>% 
  filter(PDSTATE == "ON") %>% 
  dplyr::select(PATNO, proximal_clinical_event) %>%
  group_by(PATNO) %>% 
  distinct() %>% 
  tally() %>% 
  filter(n>1) %>% .$PATNO

pd_off_subjects = pd %>% 
  filter(PDSTATE == "OFF") %>% 
  dplyr::select(PATNO, proximal_clinical_event) %>%
  group_by(PATNO) %>% 
  distinct() %>% 
  tally() %>% 
  filter(n>1) %>% .$PATNO

pd_on  = pd %>% filter(PDSTATE == "ON") %>% filter(PATNO %in% pd_on_subjects)
pd_off = pd %>% filter(PDSTATE == "OFF") %>% filter(PATNO %in% pd_off_subjects)

rm(pd_on_subjects)
rm(pd_off_subjects)

pd %>% filter(PATNO == 51731) %>% nrow()
pd_on %>% filter(PATNO == 51731) %>% nrow()
pd %>% 
  filter(PATNO == 51731) %>% 
  dplyr::select(PATNO, day_num, PDSTATE)

pd_on_imp = pd_on %>% 
  dplyr::select(PATNO, contains("is_imputed")) %>% 
  group_by(PATNO) %>% 
  mutate(sleep_imputed_perc = sum(str_count(sleep_is_imputed, "imputed"))/n(),
         step_imputed_perc  = sum(str_count(step_is_imputed, "imputed")/n()),
         heart_imputed_perc = sum(str_count(heart_is_imputed, "imputed")/n())) %>% 
  dplyr::select(PATNO, contains("perc")) %>% 
  distinct() %>% 
  ungroup() %>%
  column_to_rownames("PATNO") %>% 
  mutate(mean_imp = rowMeans(.)) %>% 
  rownames_to_column("PATNO")
  

pd_off_imp = pd_off %>% 
  dplyr::select(PATNO, contains("is_imputed")) %>% 
  group_by(PATNO) %>% 
  mutate(sleep_imputed_perc = sum(str_count(sleep_is_imputed, "imputed"))/n(),
         step_imputed_perc  = sum(str_count(step_is_imputed, "imputed")/n()),
         heart_imputed_perc = sum(str_count(heart_is_imputed, "imputed")/n())) %>% 
  dplyr::select(PATNO, contains("perc")) %>% 
  distinct() %>% 
  ungroup() %>% 
  column_to_rownames("PATNO") %>% 
  mutate(mean_imp = rowMeans(.)) %>% 
  rownames_to_column("PATNO")

# Do the dataframe are different for quantity of imputation performed?
bind_rows(pd_on_imp %>% mutate(dataset = "pd_on", .before = sleep_imputed_perc),
          pd_off_imp %>% mutate(dataset = "pd_off", .before = sleep_imputed_perc)) %>% 
  filter(PATNO != 3528) %>% 
  ggplot() +
  aes(x = dataset,
      y = mean_imp) +
  geom_boxplot() +
  ggsignif::geom_signif(comparisons = list(c("pd_on", "pd_off")))

# Removing the subject that has to many imputations
pd_on = pd_on %>% filter(PATNO != 3528)

storage = list()
for (i in 1:length(names(pd_on)[str_detect(names(pd_on), "is_imputed")])) {
  y        = names(pd_on)[str_detect(names(pd_on), "is_imputed")][i]
  formula  = as.formula(paste("updrs_tot ~",y, " + (1|PATNO)" ))
  baseline = lmer(updrs_tot ~ 1 + (1|PATNO), data = pd_on, REML = FALSE)
  fit      = lmer(formula, data = pd_on, REML = FALSE)
  storage[[i]] = anova(fit, baseline) %>% 
    broom::tidy() %>% 
    filter(term == "fit") %>%
    mutate(dataset = "pd_on", term = y, comparison = "baseline", .before="npar")
}

storage %>% reduce(bind_rows)
storage = list()
for (i in 1:length(names(pd_off)[str_detect(names(pd_off), "is_imputed")])) {
  y        = names(pd_on)[str_detect(names(pd_off), "is_imputed")][i]
  formula  = as.formula(paste("updrs_tot ~",y, " + (1|PATNO)" ))
  baseline = lmer(updrs_tot ~ 1 + (1|PATNO), data = pd_off, REML = FALSE)
  fit      = lmer(formula, data = pd_off, REML = FALSE)
  storage[[i]] = anova(fit, baseline) %>% 
    broom::tidy() %>% 
    filter(term == "fit") %>%
    mutate(dataset = "pd_off", term = y, comparison = "baseline", .before="npar")
}
storage %>% reduce(bind_rows)

df         = pd_on
predictors = names(df)[19:32]

# Find lambda
lambda.list = list()
for (i in 1:length(predictors)) {
  lambda.list[[i]] = round(forecast::BoxCox.lambda(x = df[[predictors[i] ]], method = "loglik"), 1)
}
names(lambda.list) = predictors

# Box Cox Transform
for (i in 1:length(predictors)) {
  df = df %>% 
  mutate("{predictors[i]}_box" := fabletools::box_cox(.data[[predictors[i] ]], lambda = lambda.list[[i]]))
}

# STL decomposition ----

# Create a tstibble
df = df %>%
  as_tsibble(key   = PATNO, 
             index = day_num)
 
predictors_box = paste0(predictors, "_box")

# Perform STL
df = df %>% 
  dplyr::select(PATNO, day_num, contains("_box")) %>% 
  pivot_longer(cols = contains("_box")) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(dcmp = map(.x = data, .f = ~ .x %>% model(STL(value ~ trend(window = 7)))))

# Extract decomposition
df_decomp = 
  df$dcmp %>%
  lapply(
    function(x) x %>% 
      mutate(decomp = map(.x = `STL(value ~ trend(window = 7))`,
                          .f = ~ as.tibble(.x$fit$decomposition))) %>% 
      unnest(decomp) %>% 
      dplyr::select(-`STL(value ~ trend(window = 7))`)
  ) %>% 
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
  rename_with(.cols = contains("trend"), .fn = ~ paste0(.x, "_z") )

# Merge with a subset of the original pd_on dataframe
pd_on_train = left_join(pd_on %>% 
  dplyr::select(PATNO, sex, age_seconds, age_at_diagnosis, age_at_symptoms, disease_duration_at_verily_start,
                time_from_symptoms_at_verily_start, day_num, proximal_clinical_event, is_visit_day,updrs_tot_imputed),
  df_predictors, by = c("PATNO", "day_num"))

# Saving all the object for future reference and plots
stl_decomposition_pd_on_list = list(pd_on_raw             = pd_on,
                                    pd_on_imputation_df   = pd_on_imp,
                                    lambda_list           = lambda.list,
                                    dcmp_object_df        = df,
                                    df_decomposition_only = df_decomp,
                                    df_predictors         = df_predictors,
                                    pd_on_train           = pd_on_train)

rm(df_predictors, lambda.list, df, df_decomp, df_predictors, pd_on_train)

df         = pd_off
predictors = names(df)[19:32]

# Find lambda
lambda.list = list()
for (i in 1:length(predictors)) {
  lambda.list[[i]] = round(forecast::BoxCox.lambda(x = df[[predictors[i] ]], method = "loglik"), 1)
}
names(lambda.list) = predictors

# Box Cox Transform
for (i in 1:length(predictors)) {
  df = df %>% 
  mutate("{predictors[i]}_box" := fabletools::box_cox(.data[[predictors[i] ]], lambda = lambda.list[[i]]))
}

# STL decomposition ----

# Create a tstibble
df = df %>%
  as_tsibble(key   = PATNO, 
             index = day_num)
 
predictors_box = paste0(predictors, "_box")

# Perform STL
df = df %>% 
  dplyr::select(PATNO, day_num, contains("_box")) %>% 
  pivot_longer(cols = contains("_box")) %>% 
  group_by(name) %>% 
  nest() %>% 
  mutate(dcmp = map(.x = data, .f = ~ .x %>% model(STL(value ~ trend(window = 7)))))

# Extract decomposition
df_decomp = 
  df$dcmp %>%
  lapply(
    function(x) x %>% 
      mutate(decomp = map(.x = `STL(value ~ trend(window = 7))`,
                          .f = ~ as.tibble(.x$fit$decomposition))) %>% 
      unnest(decomp) %>% 
      dplyr::select(-`STL(value ~ trend(window = 7))`)
  ) %>% 
  `names<-`(predictors_box) %>% 
  bind_rows(.id = "var")

# Pivot wider the decomposed trends to become our predictors
df_predictors = df_decomp %>% 
  dplyr::select(c("var", "PATNO", "day_num", "trend")) %>% 
  mutate(var = paste0(var, "_trend")) %>% 
  pivot_wider(names_from = var,
              values_from = trend) %>% 
  arrange(PATNO, day_num) %>% 
  mutate(across(.cols = contains("trend"), .fns = ~ as.numeric(scale(.x)))) %>% # Here a scaling happens
  rename_with(.cols = contains("trend"), .fn = ~ paste0(.x, "_z") )

# Merge with a subset of the original pd_off dataframe
pd_off_train = left_join(pd_off %>% 
  dplyr::select(PATNO, sex, age_seconds, age_at_diagnosis, age_at_symptoms, disease_duration_at_verily_start,
                time_from_symptoms_at_verily_start, day_num, proximal_clinical_event, is_visit_day,updrs_tot_imputed),
  df_predictors, by = c("PATNO", "day_num"))

# Saving all the object for future reference and plots
stl_decomposition_pd_off_list = list(pd_off_raw             = pd_off,
                                    pd_off_imputation_df   = pd_off_imp,
                                    lambda_list           = lambda.list,
                                    dcmp_object_df        = df,
                                    df_decomposition_only = df_decomp,
                                    df_predictors         = df_predictors,
                                    pd_off_train           = pd_off_train)

rm(df_predictors, lambda.list, df, df_decomp, df_predictors, pd_off_train)

stl_decomposition_pd_on_list$pd_on_train= stl_decomposition_pd_on_list$pd_on_train %>% 
  mutate(PATNO = as.factor(PATNO),
         sex   = as.factor(sex))
stl_decomposition_pd_off_list$pd_off_train= stl_decomposition_pd_off_list$pd_off_train %>% 
  mutate(PATNO = as.factor(PATNO),
         sex   = as.factor(sex))

stl_decomposition_pd_on_list$pd_on_train = stl_decomposition_pd_on_list$pd_on_train %>% 
  mutate(disease_duration  = disease_duration_at_verily_start+((day_num-1)*(1/365)),
         time_from_symptoms_onset = time_from_symptoms_at_verily_start+((day_num-1)*(1/365)),
         .before = day_num) %>% 
  dplyr::select(-c("disease_duration_at_verily_start", "time_from_symptoms_at_verily_start"))%>% 
  mutate(across(.cols = age_seconds:time_from_symptoms_onset, .fns=~ as.numeric(scale(.x))))

stl_decomposition_pd_off_list$pd_off_train = stl_decomposition_pd_off_list$pd_off_train %>% 
  mutate(disease_duration  = disease_duration_at_verily_start+((day_num-1)*(1/365)),
         time_from_symptoms_onset = time_from_symptoms_at_verily_start+((day_num-1)*(1/365)),
         .before = day_num) %>% 
  dplyr::select(-c("disease_duration_at_verily_start", "time_from_symptoms_at_verily_start"))%>% 
  mutate(across(.cols = age_seconds:time_from_symptoms_onset, .fns=~ as.numeric(scale(.x))))

pd_on_correlation_matrix = stl_decomposition_pd_on_list$pd_on_train %>% 
  dplyr::select(where(is.numeric)) %>% 
  rstatix::cor_test(method = "pearson") %>% 
  mutate(p.sig = p<0.05, .after = p)

pd_on_correlation_matrix %>%
  rstatix::as_cor_mat() %>%
  ggcorrplot::ggcorrplot(method = "square",
                         type   = "full",
                         insig  = "blank", 
                         sig.level = 0.05,
                         lab = T)


# This is for internal use // easier to filter and inspect
stl_decomposition_pd_on_list$pd_on_train %>% 
  dplyr::select(where(is.numeric)) %>% 
  correlate(method = "pearson", quiet = T) %>% 
  stretch(na.rm = T, remove.dups = T) %>% 
  filter(abs(r) > 0.5)

# To be removed after collinearity check
collinear_predictors = c("age_at_symptoms", "age_at_diagnosis", "weigthed_median_rmssd_box_trend_z", "total_nrem_time_ms_box_trend_z")

# This is potentially useful for the paper
# It is useless to mark for p value they are all significant (24345 observation make everything p<0.05)
stl_decomposition_pd_on_list$pd_on_train %>%
  dplyr::select(-all_of(collinear_predictors)) %>% 
  correlate(method = "pearson", quiet = T) %>% 
  focus("updrs_tot_imputed") %>% 
  arrange(updrs_tot_imputed) %>%
  ggplot() +
  aes(x = fct_reorder(term, updrs_tot_imputed),
      y = updrs_tot_imputed) +
  geom_col() +
  labs(x = "",
       y = "Pearson r",
       title = "Focus on UPDRS correlation") + 
  geom_hline(yintercept = 0.1) +
  geom_hline(yintercept =-0.1) +
  coord_flip()

pd_off_correlation_matrix = stl_decomposition_pd_off_list$pd_off_train %>% 
  dplyr::select(where(is.numeric)) %>% 
  rstatix::cor_test(method = "pearson") %>% 
  mutate(p.sig = p<0.05, .after = p)

pd_off_correlation_matrix %>%
  rstatix::as_cor_mat() %>%
  ggcorrplot::ggcorrplot(method = "square",
                         type   = "full",
                         insig  = "blank", 
                         sig.level = 0.05,
                         lab = T)


# This is for internal use // easier to filter and inspect
stl_decomposition_pd_off_list$pd_off_train %>% 
  dplyr::select(where(is.numeric)) %>% 
  correlate(method = "pearson", quiet = T) %>% 
  stretch(na.rm = T, remove.dups = T) %>% 
  filter(abs(r) > 0.5)

# To be removed after collinearity check
collinear_predictors = c("age_at_symptoms", "age_at_diagnosis", "weigthed_median_rmssd_box_trend_z", "total_nrem_time_ms_box_trend_z")

# This is potentially useful for the paper
# It is useless to mark for p value they are all significant (24345 observation make everything p<0.05)
stl_decomposition_pd_off_list$pd_off_train %>%
  dplyr::select(-all_of(collinear_predictors)) %>% 
  correlate(method = "pearson", quiet = T) %>% 
  focus("updrs_tot_imputed") %>% 
  arrange(updrs_tot_imputed) %>%
  ggplot() +
  aes(x = fct_reorder(term, updrs_tot_imputed),
      y = updrs_tot_imputed) +
  geom_col() +
  labs(x = "",
       y = "Pearson r",
       title = "Focus on UPDRS correlation") + 
  geom_hline(yintercept = 0.1) +
  geom_hline(yintercept =-0.1) +
  coord_flip()

## PD DV ON ----
#https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/tweedie_distribution.html
qqnorm(y = stl_decomposition_pd_on_list$pd_on_train$updrs_tot_imputed)
stl_decomposition_pd_on_list$pd_on_train$updrs_tot_imputed %>% check_distribution() %>% plot()
stl_decomposition_pd_on_list$pd_on_train$updrs_tot_imputed %>% fitdistrplus::descdist()
## PD DV OFF ----
#https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/tweedie_distribution.html
qqnorm(y = stl_decomposition_pd_off_list$pd_off_train$updrs_tot_imputed)
stl_decomposition_pd_off_list$pd_off_train$updrs_tot_imputed %>% check_distribution() %>% plot()
stl_decomposition_pd_off_list$pd_off_train$updrs_tot_imputed %>% fitdistrplus::descdist()

# check n subject
stl_decomposition_pd_on_list$pd_on_train$PATNO %>% unique() %>% length()

# Train test split
set.seed(12345) # <- Don't forget to seed mate
pd_on_partitioned = stl_decomposition_pd_on_list$pd_on_train %>% 
  dplyr::select(-all_of(collinear_predictors)) %>% 
  partition(p = 0.2,
            num_col = "updrs_tot_imputed",
            id_col  = "PATNO")

pd_on_partitioned[[1]]$PATNO %>% unique() %>% length()
pd_on_partitioned[[2]]$PATNO %>% unique() %>% length()
names(pd_on_partitioned) = c("test", "train")

pd_on_partitioned$train %>% names()

train = pd_on_partitioned$train

# Single Fixed effect of demographics
fitb      = lmer(updrs_tot_imputed ~ 1 + (1|PATNO), data = train, REML = FALSE)
fit_age   = lmer(updrs_tot_imputed ~  age_seconds + (1|PATNO), data = train, REML = FALSE)
fit_ts    = lmer(updrs_tot_imputed ~  time_from_symptoms_onset + (1|PATNO), data = train, REML = FALSE)
fit_dd    = lmer(updrs_tot_imputed ~  disease_duration + (1|PATNO), data = train, REML = FALSE)
fit_dn    = lmer(updrs_tot_imputed ~  day_num + (1|PATNO), data = train, REML = FALSE)

# Fit multiple to assess collinearity
fit_all = lmer(updrs_tot_imputed ~ age_seconds + time_from_symptoms_onset + disease_duration + day_num + (1|PATNO), data = train, REML = FALSE)

# As expected these variables encoding time are multicollinear
check_collinearity(fit_all) %>% plot()

# If we use the single term deletion method we can see that time_from_symptoms_onset is the best
# This is true also for RMSE, so we can get rid of the others
# In this case i want to remark that we are using the time from symptoms onset at verily start and adding up 1 day every day of the experiment
drop1(fit_all, test = "LRT")

# They seem all bit better than fitb but as we can see from drop1 is better to keep only time_from_symptoms
compare_performance(fitb, fit_age, fit_ts, fit_dd, fit_dn, fit_all,
                    rank = T)

fit_sex  = lmer(updrs_tot_imputed ~  sex + (1|PATNO), data = train, REML = FALSE)
compare_performance(fitb, fit_sex, rank = T)
drop1(fit_sex, test = "LRT")

# Single fit
fit_inb = lmer(updrs_tot_imputed ~  inbed_time_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_tots=lmer(updrs_tot_imputed ~  total_sleep_time_ms_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_wake=lmer(updrs_tot_imputed ~  wake_after_sleep_onset_ms_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_effi=lmer(updrs_tot_imputed ~  sleep_efficiency_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_awak=lmer(updrs_tot_imputed ~  num_awakenings_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_rem=lmer(updrs_tot_imputed ~  total_rem_time_ms_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_drem=lmer(updrs_tot_imputed ~  total_deep_nrem_time_ms_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_lrem=lmer(updrs_tot_imputed ~  total_light_nrem_time_ms_box_trend_z + (1|PATNO), data = train, REML = FALSE)

# As we can see inbed time performs worst than baseline so it is out
compare_performance(fitb, fit_inb, fit_tots, fit_wake, fit_effi, fit_awak, fit_rem, fit_drem, fit_lrem, rank = T)

# Fit all sleep terms except for inbed_time
fit_sleep_all = lmer(updrs_tot_imputed ~ total_sleep_time_ms_box_trend_z + wake_after_sleep_onset_ms_box_trend_z + sleep_efficiency_box_trend_z + num_awakenings_box_trend_z + total_rem_time_ms_box_trend_z + total_deep_nrem_time_ms_box_trend_z + total_light_nrem_time_ms_box_trend_z + (1|PATNO), data = train, REML = FALSE)

# Wake_after_sleep_onset_ms_box_trend_z is collinear and not significant when testing for LRT so it needs to go out
drop1(fit_sleep_all)
check_collinearity(fit_sleep_all)

# Single fit
fit_step    = lmer(updrs_tot_imputed ~ daily_step_count_sum_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_rm_mean = lmer(updrs_tot_imputed ~ weigthed_mean_rmssd_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_rm_cv   = lmer(updrs_tot_imputed ~ weigthed_mean_cv_rmssd_box_trend_z + (1|PATNO), data = train, REML = FALSE)
fit_pulse   = lmer(updrs_tot_imputed ~ weigthed_mean_pulse_rate_box_trend_z + (1|PATNO), data = train, REML = FALSE)

# They are all better than baseline
compare_performance(fitb, fit_step, fit_rm_mean, fit_rm_cv, fit_pulse, rank = T)

fit_heartstep= lmer(updrs_tot_imputed ~ daily_step_count_sum_box_trend_z + weigthed_mean_rmssd_box_trend_z + weigthed_mean_cv_rmssd_box_trend_z +
                      weigthed_mean_pulse_rate_box_trend_z +
                      (1|PATNO), data = train, REML = FALSE)

# LRT They seem all good again
drop1(fit_heartstep)

# And they are not really correlated so ok
check_collinearity(fit_heartstep)

fit_combined = lmer(updrs_tot_imputed ~ daily_step_count_sum_box_trend_z + weigthed_mean_rmssd_box_trend_z + 
                      weigthed_mean_cv_rmssd_box_trend_z + weigthed_mean_pulse_rate_box_trend_z + total_sleep_time_ms_box_trend_z + 
                      sleep_efficiency_box_trend_z + num_awakenings_box_trend_z + total_rem_time_ms_box_trend_z + total_deep_nrem_time_ms_box_trend_z + total_light_nrem_time_ms_box_trend_z+ sex+ time_from_symptoms_onset+ 
                      (1|PATNO), data = train, REML = F )
summary(fit_combined)

compare_performance(fitb, fit_combined, rank = T)
check_collinearity(fit_combined)
drop1(fit_combined)

# Removing total ligth nrem and sleep efficiency
fit_combined1 = lmer(updrs_tot_imputed ~ daily_step_count_sum_box_trend_z + weigthed_mean_rmssd_box_trend_z + 
                       weigthed_mean_cv_rmssd_box_trend_z + weigthed_mean_pulse_rate_box_trend_z  + num_awakenings_box_trend_z + total_rem_time_ms_box_trend_z + total_deep_nrem_time_ms_box_trend_z + total_light_nrem_time_ms_box_trend_z+ sex+ time_from_symptoms_onset+ (1|PATNO), data = train, REML = F)


compare_performance(fitb, fit_combined1, rank = T)
check_collinearity(fit_combined1)
drop1(fit_combined1)
summary(fit_combined1)

report::report(fit_combined1, include_effectsize = TRUE, include_diagnostic = TRUE)
check_predictions(fit_combined1)

fit_reml = lmer(updrs_tot_imputed ~ daily_step_count_sum_box_trend_z + weigthed_mean_rmssd_box_trend_z + 
                       weigthed_mean_cv_rmssd_box_trend_z + weigthed_mean_pulse_rate_box_trend_z  + num_awakenings_box_trend_z + total_rem_time_ms_box_trend_z + total_deep_nrem_time_ms_box_trend_z + total_light_nrem_time_ms_box_trend_z+ sex+ time_from_symptoms_onset+ (1|PATNO), data = train, REML = T)
report::report(fit_reml, include_effectsize = TRUE, include_diagnostic = TRUE)

set.seed(12345)
train_cv = fold(train,
                k = 4,
                num_fold_cols = 3,
                num_col = "updrs_tot_imputed",
                id_col = "PATNO")

formula_cvs = "updrs_tot_imputed ~ daily_step_count_sum_box_trend_z + weigthed_mean_rmssd_box_trend_z + 
                       weigthed_mean_cv_rmssd_box_trend_z + weigthed_mean_pulse_rate_box_trend_z  + num_awakenings_box_trend_z + total_rem_time_ms_box_trend_z + total_deep_nrem_time_ms_box_trend_z + total_light_nrem_time_ms_box_trend_z+ sex+ time_from_symptoms_onset+ (1|PATNO)"
cvs = cross_validate(data     = train_cv,
                     formulas = formula_cvs,
                     fold_cols = paste0(".folds_", 1:3),
                     family = "gaussian")
cvs$Results[[1]]
cvs$Results[[1]] %>% .$RMSE %>% mean()
cvs$Results[[1]] %>% .$RMSE %>% sd()

library(caret)
prova = pd_on_partitioned$test %>% filter(PATNO != 51731)
prova = prova %>% 
  dplyr::select(PATNO, day_num, updrs_tot_imputed) %>% 
  mutate(baseline_predictions = predict(fitb, prova, allow.new.levels = T),
         model_predictions    = predict(fit_combined1, prova, allow.new.levels = T),
         rmse_baseline        = RMSE(baseline_predictions, updrs_tot_imputed),
         rmse_model           = RMSE(model_predictions, updrs_tot_imputed),
         r2_baseline          = R2(baseline_predictions, updrs_tot_imputed, form = "traditional"),
         r2_model             = R2(model_predictions, updrs_tot_imputed, form = "traditional")
         )

prova %>% 
  dplyr::select(rmse_baseline, rmse_model) %>% distinct()


pd_on_partitioned = lapply(pd_on_partitioned, function(x) x %>% 
         group_by(PATNO) %>% 
         mutate(week_num = 1:n() %/% 7, .after = day_num) %>% 
         ungroup())

pd_on_partitioned = lapply(pd_on_partitioned, function(x) x %>%
         group_by(PATNO) %>% 
         nest() %>% 
         mutate(slope_updrs = map(.x = data, .f = ~ lm(updrs_tot_imputed ~ day_num, data = .x)$coefficients[2])) %>% 
         unnest() %>% 
         ungroup())

pd_on_partitioned$test %>% 
  names()

pd_on_partitioned$test %>% 
  mutate(PATNO = droplevels(PATNO)) %>% 
  split(f = .$PATNO) %>% 
  lapply(., function(x) x %>% .$age_seconds %>% plot())

storage_3[[i]] %>% 
  mutate(age_seconds1 = lm(age_seconds ~ day_num, data = storage_3[[i]])$coefficients[2], .after = age_seconds)
storage_3[[i]]
stl_decomposition_pd_on_list$pd_on_train %>% filter(PATNO == 51731) %>%ggplot() + aes(x = day_num, y=age_seconds) + geom_point() + geom_line()

stl_decomposition_pd_on_list$pd_on_train

stl_decomposition_pd_on_list$pd_on_raw%>% filter(PATNO == 51731)
