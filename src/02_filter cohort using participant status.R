#' ---
#' title: "Untitled"
#' author: "Andrea Termine"
#' date: "2023-01-27"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(12345)

#' 
## ----library--------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(openxlsx)

#' 
## ----parameters-----------------------------------------------------------------------------------------------------------------------
root = dirname(getwd())
analytical_dataset_path = file.path(root, "data", "raw",
                                    "study_data", "study_docs", "Quick_start",
                                    "Consensus_Committee_Analytic_Datasets_28OCT21.xlsx")

sc_path      = file.path(root, "data", "raw", "study_data", "Subject_Characteristics")
sensors_path = file.path(root, "data", "sensors")

#' 
## ----metadata-------------------------------------------------------------------------------------------------------------------------
dictionary = read.csv(file.path(root, "data", "raw", "study_data",
          "study_docs", "Data_and_Databases", "Data_Dictionary_-__Annotated_.csv"), 
          na.strings = "")

codelist = read.csv(file.path(root, "data", "raw", "study_data", "study_docs", "Data_and_Databases", "Code_List_-__Annotated_.csv"), na.strings = "")

#' 
## ----import verily participants-------------------------------------------------------------------------------------------------------
verily_common_ids = readRDS(file = file.path(sensors_path, "common ids.rds"))

#' 
#' 1
#' Subset PPMI cohorts to obtain only the individuals that participated to the verily study.
#' 
#' 2
#' out of 342 PPMI participant that joined the verily study, 10 belonged to the Early Imaging cohort. 34 were Healthy Control, 2 were non-PD, non-Prodromal, non-HC (participants to be excluded), 142 individuals were affected by Parkinson's Disease. 152 subjects were labeled as Prodromal and 2 belonged to SWEDD legacy cohort.	
#' 
#' 3
#' out of 152 Prodromal subjects, 145 did not converted (level:0) while 7 converted(level:1)
#' 
#' 4
#' The Early Imaging Cohort are PD patients https://clinicaltrials.gov/ct2/show/NCT04507139, but they are not considered PD in the analytic dataset. So we should exclude them.
#' 
#' 5
#' There are two patients that were enrolled in PD but they were reassigned to Prodromal using consensus diagnosis: 54144, 57127. We decided to exclude them.
#' 
#' 6
#' We removed two swedd legacy patients. They do not lack of dopamine and so they are different from PD classical patients.
#' 
#' 
## ----examine the patient status-------------------------------------------------------------------------------------------------------
par_stat        = read.csv(file = file.path(sc_path, "Participant_Status.csv"), na.strings = "")
par_stat_dict     = dictionary %>%
  filter(ITM_NAME %in% names(par_stat),
         MOD_NAME == "PATIENT_STATUS")
  
par_stat_codelist = codelist %>% filter(ITM_NAME %in% names(par_stat),
                                        MOD_NAME == "PATIENT_STATUS")

#1
par_stat = par_stat %>% filter(PATNO %in% verily_common_ids)
#2
par_stat %>% group_by(CONCOHORT_DEFINITION) %>% tally()
par_stat = par_stat %>% filter(CONCOHORT_DEFINITION != "non-PD, non-Prodromal, non-HC (participants to be excluded)")
#3
par_stat %>% group_by(CONCOHORT_DEFINITION, PHENOCNV) %>% tally()
#4
par_stat = par_stat %>% filter(COHORT_DEFINITION != "Early Imaging (original study participants only)")
#5
par_stat$PATNO[par_stat$COHORT_DEFINITION != par_stat$CONCOHORT_DEFINITION]
par_stat = par_stat %>% filter(!PATNO %in% c("54144", "57127"))
#6
par_stat = par_stat %>% filter(CONCOHORT_DEFINITION != "SWEDD")


#' 
#' We corrected the participant status using the analytic dataset. Now we can define additional criteria.
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
part_stat_list = list(part_stat          = par_stat,
     part_stat_codelist = par_stat_codelist,
     par_stat_dict      = par_stat_dict,
     description        = "part_stat è il participant status filtrato per includere solo i soggetti della nostra coorte. Gli altri due documenti rappresentano il codebook del participant status. Il file originale è participants status csv")

saveRDS(part_stat_list, file.path(root, "data", "intermediate","metadata",
                                  "cohort metadata.rds"))

#' 
