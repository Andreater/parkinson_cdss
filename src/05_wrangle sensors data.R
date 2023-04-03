#' ---
#' title: "Untitled"
#' author: "Andrea Termine"
#' date: "2023-02-02"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(12345)

library(tidyverse)
library(fst)
library(openxlsx)
library(assertthat)

#' 
## ----parameters-----------------------------------------------------------------------------------------------------------------------
root        = dirname(getwd())
verily_path = file.path(root, "data", "sensors")
meta_path   = file.path(root, "data", "intermediate", "metadata")
analytical_dataset_path = file.path(root, "data", "raw",
                                    "study_data", "study_docs", "Quick_start",
                                    "Consensus_Committee_Analytic_Datasets_28OCT21.xlsx")

n_visit_for_filtering = c(2,3)

#' 
## ----import meta----------------------------------------------------------------------------------------------------------------------
metadata_cohort = readRDS(file.path(meta_path, "cohort metadata.rds"))
sensors         = read.fst(path = file.path(verily_path, "onwrist with age and corresponding clinical visit.fst"))
visits          = read.csv(file.path(root, "data", "raw", "study_data",
                                     "Subject_Characteristics", "demographics", "Age_at_visit.csv"))

sheet = readxl::excel_sheets(analytical_dataset_path)[3:5]
analytic_dataset = list()
for (i in sheet) {
  analytic_dataset[[i]] =read.xlsx(analytical_dataset_path, sheet = i)
}
names(analytic_dataset) = sheet

#' 
#' 1) Filter out the subject that were removed by the cohort due to consensus diagnosis or analytic dataset reasons.
#' 2) Filtering for minimum number of visits or time span covered in the study?
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
#1
sensors = sensors %>% 
  filter(subject %in% metadata_cohort$part_stat$PATNO)
assert_that(length(unique(sensors$subject)) == length(metadata_cohort$part_stat$PATNO), msg ="Something went wrong")

#' 
