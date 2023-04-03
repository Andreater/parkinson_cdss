#' ---
#' title: "Untitled"
#' author: "Andrea Termine"
#' date: "2023-01-30"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(12345)

library(tidyverse)
library(assertthat)
library(parallel)
library(multidplyr)
library(tictoc)
library(fst)
library(openxlsx)

#' 
## -------------------------------------------------------------------------------------------------------------------------------------
root        = dirname(getwd())
verily_path = file.path(root, "data", "sensors")
meta_path   = file.path(root, "data", "intermediate", "metadata")
out_path    = file.path(root, "data", "intermediate", "sensors")
analytical_dataset_path = file.path(root, "data", "raw",
                                    "study_data", "study_docs", "Quick_start",
                                    "Consensus_Committee_Analytic_Datasets_28OCT21.xlsx")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------
metadata_cohort = readRDS(file.path(meta_path, "cohort metadata.rds"))
sensors         = read.fst(path = file.path(verily_path, "onwrist.fst"))
visits          = read.csv(file.path(root, "data", "raw", "study_data",
                                     "Subject_Characteristics", "demographics", "Age_at_visit.csv"))

sheet = readxl::excel_sheets(analytical_dataset_path)[3:5]
analytic_dataset = list()
for (i in sheet) {
  analytic_dataset[[i]] =read.xlsx(analytical_dataset_path, sheet = i)
}
names(analytic_dataset) = sheet

#' 
#' ## Test phase
#' 
#' This chunk let's you test 1 or multiple subjects. The last run tested 5 patients.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
# slice 5 random subject to test the code
example_ids = sample(metadata_cohort$part_stat$PATNO, 5)
example_set = sensors %>% 
  filter(subject %in% example_ids)

# Compute Age for the test subjects
example_set = example_set %>% 
  mutate(age_at_verily_event = age_seconds/31556952) %>% 
  relocate(age_at_verily_event, .after = subject)

# Compute time from enrollment
example_set = left_join(example_set,
metadata_cohort$part_stat %>% 
  dplyr::select(PATNO, ENROLL_AGE) %>% 
  rename(subject = PATNO), by = "subject") %>% 
  mutate(time_from_enrollment_years = age_at_verily_event - ENROLL_AGE, .before = age_at_verily_event)

clinical_events = visits %>% 
  filter(PATNO %in% example_ids) %>% 
  arrange(PATNO, AGE_AT_VISIT) %>% 
  rename(subject = PATNO)


#' 
#' 0.1 anni (36.52425 giorni) è la finestra temporale in cui sostieni che la visita è corrispondente.
#' 
#' This R code creates a "storage" data frame and initializes the columns with empty values. Then it creates a unique list of subjects "ids" by extracting the unique values from the "subject" column in the "example_set" data frame. Next, a "for" loop is executed for each unique value in "ids", which filters the corresponding records in the "example_set" and "clinical_events" data frames. For each unique age value in "subset_example_set", the code calculates the difference with every age present in "subset_clinical_events" and identifies the closest clinical event (minimum). A "corr" flag is then set based on the proximity of the age. Finally, for each iteration of the "for" loop, the values are added to the "storage" data frame as a new row.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
storage = data.frame(subject                      = integer(),
                     age_at_verily_event          = double(),
                     proximal_clinical_event      = character(),
                     correspondent_clinical_event = logical())

ids = example_set$subject %>% unique()

for (id in 1:length(ids)) {
  subset_example_set     = example_set %>% filter(subject == ids[id])
  subset_clinical_events = clinical_events %>% filter(subject == ids[id])
  
  a = subset_example_set$age_at_verily_event %>% unique()
  b = subset_clinical_events$AGE_AT_VISIT
  
  for (i in 1:length(a)) {
    a_minus_b      = abs(a[[i]] - b)
    min_a_minus_b  = a_minus_b %>% min()
    proximal_event = subset_clinical_events$EVENT_ID[which(a_minus_b == min_a_minus_b)]
    
    assert_that(length(min_a_minus_b) == 1,
                msg = "Multiple events were found for the same age.")
    
    if (min_a_minus_b <= 0.1) {
      corr = TRUE
    } else {corr = FALSE}
    
    storage = storage %>% 
      add_row(subject                 = ids[[id]],
              age_at_verily_event     = a[[i]],
              proximal_clinical_event = proximal_event,
              correspondent_clinical_event = corr) 
  }
}

#' 
#' ## Real implementation
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
# Compute Age for the test subjects
sensors = sensors %>% 
  mutate(age_at_verily_event = age_seconds/31556952) %>% 
  relocate(age_at_verily_event, .after = subject)

# Compute time from enrollment
sensors = left_join(sensors,
metadata_cohort$part_stat %>% 
  dplyr::select(PATNO, ENROLL_AGE) %>% 
  rename(subject = PATNO), by = "subject") %>% 
  mutate(time_from_enrollment_years = age_at_verily_event - ENROLL_AGE, .before = age_at_verily_event)

clinical_events = visits %>% 
  filter(PATNO %in% unique(sensors$subject)) %>% 
  arrange(PATNO, AGE_AT_VISIT) %>% 
  rename(subject = PATNO)


#' 
#' 0.1 anni (36.52425 giorni) è la finestra temporale in cui sostieni che la visita è corrispondente.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
storage = data.frame(subject                      = integer(),
                     age_at_verily_event          = double(),
                     proximal_clinical_event      = character(),
                     correspondent_clinical_event = logical())

ids = sensors$subject %>% unique()

for (id in 1:length(ids)) {
  subset_sensors     = sensors %>% filter(subject == ids[id])
  subset_clinical_events = clinical_events %>% filter(subject == ids[id])
  
  a = subset_sensors$age_at_verily_event %>% unique()
  b = subset_clinical_events$AGE_AT_VISIT
  
  for (i in 1:length(a)) {
    a_minus_b      = abs(a[[i]] - b)
    min_a_minus_b  = a_minus_b %>% min()
    proximal_event = subset_clinical_events$EVENT_ID[which(a_minus_b == min_a_minus_b)]
    
    assert_that(length(min_a_minus_b) == 1,
                msg = "Multiple events were found for the same age.")
    
    if (min_a_minus_b <= 0.1) {
      corr = TRUE
    } else {corr = FALSE}
    
    storage = storage %>% 
      add_row(subject                 = ids[[id]],
              age_at_verily_event     = a[[i]],
              proximal_clinical_event = proximal_event,
              correspondent_clinical_event = corr) 
  }
}

# Perform some checks
assert_that(storage$subject %>% unique() %>% length() == 
              sensors$subject %>% unique() %>% length(),
            msg = "you missed a subject")


#' 
## -------------------------------------------------------------------------------------------------------------------------------------
sensors = left_join(sensors,
          storage, 
          by = c("subject", "age_at_verily_event")) %>% 
  relocate(ENROLL_AGE, .before = time_from_enrollment_years)

# Ok
sensors$proximal_clinical_event %>% unique()

#' This code modifies a data frame called "sensors" using the dplyr library. It performs the following operations:
#' 
#' Joins the "sensors" data frame with the "clinical_events" data frame using the "subject" and "proximal_clinical_event" columns. The "EVENT_ID" column in "clinical_events" is renamed to "proximal_clinical_event".
#' 
#' Adds a new column to the "sensors" data frame called "years_covered_by_clinical_events", which is calculated as the difference between the maximum and minimum "age_at_verily_event" values for each "subject".
#' 
#' Adds a new column to the "sensors" data frame called "n_unique_visits", which is the number of unique values in the "AGE_AT_VISIT" column for each "subject".
#' 
#' Verifies the "proximal_clinical_event" values for subject 3961 in the "sensors" data frame.
#' 
#' Creates a new data frame called "unique_visit_checker" by selecting the "subject", "proximal_clinical_event", and "AGE_AT_VISIT" columns from "sensors". This new data frame is processed to determine if the "AGE_AT_VISIT" values are duplicated or unique. This information is added as a new column called "is_the_clinical_event_unique".
#' 
#' Joins the "sensors" data frame with the "unique_visit_checker" data frame using the "subject" and "proximal_clinical_event" columns.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
sensors = left_join(sensors, 
          clinical_events %>% 
            rename("proximal_clinical_event" = EVENT_ID),
          by = c("subject", "proximal_clinical_event"))

sensors = sensors %>% 
  group_by(subject) %>% 
  mutate(years_covered_by_clinical_events = max(age_at_verily_event)-min(age_at_verily_event)) %>% 
  ungroup()

sensors = sensors %>% 
  group_by(subject) %>% 
  mutate(n_unique_visits = length(unique(AGE_AT_VISIT))) %>% 
  ungroup()

# sensors$proximal_clinical_event[sensors$subject == 3961] %>% unique()
# sensors$n_unique_visits[sensors$subject == 3961] %>% unique()

unique_visit_checker = sensors %>%
  dplyr::select(subject, proximal_clinical_event, AGE_AT_VISIT) %>%
  distinct() %>% 
  group_by(subject) %>% 
  arrange(subject, AGE_AT_VISIT) %>% 
  mutate(logical = case_when(duplicated(AGE_AT_VISIT) ~ AGE_AT_VISIT),
         logical2 = ifelse(AGE_AT_VISIT %in% logical, "block_duplicated", "unique")) %>% 
  ungroup() %>% 
  group_by(subject, logical2) %>% 
  mutate(logical3 = ifelse(logical2 == "block_duplicated", paste(logical2, AGE_AT_VISIT, sep = "_"), logical2)) %>% ungroup() %>% 
  dplyr::select(subject, proximal_clinical_event, logical3) %>% 
  rename(is_the_clinical_event_unique = logical3)
 

sensors = left_join(sensors,
          unique_visit_checker,
          by = c("subject", "proximal_clinical_event"))

temp = left_join(metadata_cohort$part_stat,
                 bind_rows(analytic_dataset$PD %>% dplyr::select(PATNO, Subgroup),

                                                      analytic_dataset$Prodromal %>% dplyr::select(PATNO, Subgroup)))

sensors=left_join(sensors,
          temp%>% 
            dplyr::select(PATNO, CONCOHORT_DEFINITION, Subgroup) %>% 
            rename(subject = PATNO), by = "subject")

#' 
#' The code below let's you inspect the visits for each subgroup
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
sensors %>%  
  dplyr::select(subject, n_unique_visits, CONCOHORT_DEFINITION) %>% 
  distinct() %>% 
 xtabs(~ n_unique_visits + CONCOHORT_DEFINITION, data = .) %>% 
  addmargins()

sensors %>%  
  dplyr::select(subject, n_unique_visits,Subgroup) %>% 
  distinct() %>% 
 xtabs(~ n_unique_visits + Subgroup, data = .) %>%
  addmargins()

sensors$subject %>% unique() %>% length()

#' 
#' ## Export
#' **This is the matcher export. If you use id and age_seconds as keys, you can left join the other sensors data.**
## -------------------------------------------------------------------------------------------------------------------------------------
write.fst(x = sensors, path = file.path(root, "data", "sensors", "onwrist with age and corresponding clinical visit.fst"))

#' 
#' 
#' 
