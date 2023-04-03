#' ---
#' title: "Untitled"
#' author: "Andrea Termine"
#' date: "2023-02-06"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(12345)
library(tidyverse)
meta_path   = file.path(root, "data", "intermediate", "metadata")
metadata_cohort = readRDS(file.path(meta_path, "cohort metadata.rds"))
updrs = read.csv(file.path(root, "data","raw","study_data","Motor_Assessments", "MDS-UPDRS_Part_I.csv"))
biosp = read.csv(file.path(root, "data","raw","study_data","Biospecimen", "Current_Biospecimen_Analysis_Results.csv"))

#' 
## -------------------------------------------------------------------------------------------------------------------------------------
meta_clinical_events = data.frame(
  stringsAsFactors = FALSE,
       check.names = FALSE,
               clinical_event = c("0","AV1","AV133","AV133TC",
                       "AV2","AV3","AV4","BL","CONSENT","CTCCONLY","FLORBET",
                       "FLORBETC","FNL","GMU","LOG","P102","P114","P126",
                       "P138","P150","P78","P90","PW","PW1","R01","R04",
                       "R06","R08","R10","R12","R13","R14","R15","R16",
                       "R17","R18","R19","RANDOM","RS1","RS2","SC","SCBL",
                       "SKINBIO","SKINBITC","ST","STC","T06","T108","T12",
                       "T132","T15","T156","T17","T18","T19","T21",
                       "T24","T27","T30","T33","T36","T39","T42","T45","T48",
                       "T51","T54","T57","T60","T72","T84","T96",
                       "TAPFNL","TBL","TDB","TPW","TSC","TST","U01","U02",
                       "U03","U04","U05","U06","UP1","UP2","UP3","UT1","UT2",
                       "UT3","UT4","V01","V02","V03","V04","V05","V06",
                       "V07","V08","V09","V10","V11","V12","V13","V14",
                       "V15","V16","V17","V18","V19","V20","X01","X02",
                       "X03"),
      description = c("Not Completed", "Unscheduled Telephone AV-133",
                       "AV-133","AV-133 Telephone Follow up",
                       "Unscheduled Telephone AV-133","Unscheduled Telephone AV-133",
                       "Unscheduled Telephone AV-133","Baseline","Consent","CTCCONLY",
                       "Florbetaben Imaging","Florbetaben Telephone Call",
                       "Final Visit","Genetic Testing","Logs",
                       "Phone Visit (Month 102)","Phone Visit (Month 114)",
                       "Phone Visit (Month 126)","Phone Visit (Month 138)",
                       "Phone Visit (Month 150)","Phone Visit (Month 78)","Phone Visit (Month 90)",
                       "Premature Withdrawal","Premature Withdrawl -ND",
                       "Remote Visit 01 (Month 6)","Remote Visit 04 (Month 18)",
                       "Remote Visit 06 (Month 30)","Remote Visit 08 (Month 42)",
                       "Remote Visit 10 (Month 54)","Remote Visit 12 (Month 66)",
                       "Remote Visit 13 (Month 78)",
                       "Remote Visit 14 (Month 90)","Remote Visit 15 (Month 102)",
                       "Remote Visit 16 (Month 114)","Remote Visit 17 (Month 126)",
                       "Remote Visit 18 (Month 138)","Remote Visit 19 (Month 150)","Randomize",
                       "Re-Screen","Second Re-Screen","Screening",
                       "Screening/Baseline Combined","Skin Biopsy",
                       "Skin Biopsy Telephone Call","Symptomatic Therapy",
                       "Symptomatic Therapy Telephone Call","Telephone Contact (Month 6)",
                       "Telephone Contact","Telephone Contact (Month 12)",
                       "Telephone Contact","Telephone Contact (Month 15)","Telephone Contact",
                       "Telephone Contact","Telephone Contact (Month 18)",
                       "Telephone Contact","Telephone Contact (Month 21)",
                       "Telephone Contact (Month 24)","Telephone Contact (Month 27)",
                       "Telephone Contact (Month 30)",
                       "Telephone Contact (Month 33)","Telephone Contact (Month 36)",
                       "Telephone Contact (Month 39)","Telephone Contact (Month 42)",
                       "Telephone Contact (Month 45)","Telephone Contact (Month 48)",
                       "Telephone Contact (Month 51)","Telephone Contact (Month 54)",
                       "Telephone Contact (Month 57)",
                       "Telephone Contact (Month 60)","Telephone Contact (Month 72)",
                       "Telephone Contact","Telephone Contact (Month 96)","TAP Final",
                       "Telephone Contact (BL)","Digital Biomarker Telephone Followup",
                       "Telephone Contact - PW","Telephone Contact (SC)",
                       "Telephone Contact - Symptomatic Therapy",
                       "Unscheduled Visit 1","Unscheduled Visit 2","Unscheduled Visit 03",
                       "Unscheduled Visit 04","Unscheduled Visit 05",
                       "Unscheduled Visit 06","Unscheduled Telephone Contact",
                       "Unscheduled Telephone Contact","Unscheduled Telephone Contact",
                       "Unscheduled Telephone Contact",
                       "Unscheduled Telephone Contact","Unscheduled Telephone Contact",
                       "Unscheduled Telephone Contact","Visit 01 (Month 3)","Visit 02 (Month 6)",
                       "Visit 03 (Month 9)","Visit 04 (Month 12)",
                       "Visit 05 (Month 18)","Visit 06 (Month 24)","Visit 07 (Month 30)",
                       "Visit 08 (Month 36)","Visit 09 (Month 42)",
                       "Visit 10 (Month 48)","Visit 11 (Month 54)","Visit 12 (Month 60)",
                       "Visit 13 (Month 72)","Visit 14 (Month 84)",
                       "Visit 15 (Month 96)","Visit 16 (Month 108)",
                       "Visit 17 (Month 120)","Visit 18 (Month 132)","Visit 19 (Month 144)",
                       "Visit 20 (Month 156)","Transfer Event","Transfer Event",
                       "Transfer Event"))


#saveRDS(meta_clinical_events, "clinical timeline.rds")

#' 
## -------------------------------------------------------------------------------------------------------------------------------------
matcher = updrs %>% 
  filter(PATNO %in% metadata_cohort$part_stat$PATNO) %>% 
  dplyr::select(PATNO, EVENT_ID, INFODT)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
visits = read.csv(file.path(root, "data", "raw", "study_data",
                            "Subject_Characteristics", "demographics", "Age_at_visit.csv"))

visits = visits %>% filter(PATNO %in% metadata_cohort$part_stat$PATNO)
visits = visits %>% 
  group_by(PATNO) %>% 
  mutate(is_duplicated = duplicated(AGE_AT_VISIT)) 

# visits = visits %>% 
#   mutate(AGE_AT_VISIT = ifelse(EVENT_ID == "SC" & is_duplicated == TRUE, AGE_AT_VISIT-0.1, AGE_AT_VISIT),
#          is_duplicated = duplicated(AGE_AT_VISIT))


visits = left_join(visits,
          meta_clinical_events %>% rename(EVENT_ID= clinical_event), by = "EVENT_ID")


visits = visits %>% 
  mutate(months = str_extract(description, "(Month \\d+)") %>% 
                                str_extract("\\d+") %>% 
                                as.numeric(),
         months = case_when(EVENT_ID == "BL" ~ 1,
                            EVENT_ID == "SC" ~ 0,
                            TRUE ~ months)) %>% 
  na.omit() %>% 
  arrange(PATNO, months) %>% 
  mutate(is_duplicated = duplicated(AGE_AT_VISIT))

visits = visits %>% 
  mutate(expected_age_at_visit = round(min(AGE_AT_VISIT) + months/12,1),
         delay_in_visit        = AGE_AT_VISIT-expected_age_at_visit)

visits = left_join(visits, matcher, by = c("PATNO", "EVENT_ID"))

saveRDS(visits, "clinical timeline.rds")


#' 
#' 
