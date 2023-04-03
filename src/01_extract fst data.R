#' ---
#' title: "Untitled"
#' author: "Andrea Termine"
#' date: "2023-01-26"
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE-------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------
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
verily_path = file.path(root, "data", "raw", "study_data", "Verily_Study_Watch")

verily_files_names = list.files(path    = verily_path,
                          pattern = ".zip") %>% 
  word(sep = "\\.")

verily_files_paths = list.files(path    = verily_path,
                                pattern = ".zip",
                                full.names = TRUE)

#' 
#' The following code is checking the integrity of a set of files by making sure that each file has a unique name and that no files are missing or duplicated. Using the assert_that function to check that the number of files in each "Alias" group is equal to 1, if not, an error is thrown indicating that there is an issue with the integrity of the files.
#' 
## ----intcheck1------------------------------------------------------------------------------------------------------------------------
# Unzip the files
integrity_check_1 = lapply(verily_files_paths, unzip, list = TRUE)

# Bind rows and add columns
integrity_check_1 =
  reduce(integrity_check_1, bind_rows) %>% 
  mutate(Parent = verily_files_paths,
         Alias = verily_files_names) %>%
  select(Alias, Parent, everything())

# Check that the number of files in each group is equal to 1
assert_that(sum(integrity_check_1 %>%
                group_by(Alias) %>%
                tally() %>% .$n == 1) == 9,
            msg = "Multiple file detected")

#' This code uses parallel processing to read multiple csv files and unzip them in parallel, and then assigns the names of the verily_files to the verily_files_names for easy identification.
#' However, it takes 13 minutes to load the data (18.3 GB). It is possible to implement faster solutions, but I will not try it now.
#' 
## ----import---------------------------------------------------------------------------------------------------------------------------
verily_files = vector(mode="list", length =length(verily_files_names))

cl = makeCluster(detectCores())
showConnections()

tic()
verily_files = parLapply(cl = cl, verily_files_paths, function(x){read.csv(unzip(x))})
stopCluster(cl)
names(verily_files) = verily_files_names 
toc()

#' 
#' This code is looping through a list of dataframes called "verily_files", and for each dataframe in the list, it is using the "write.fst" function to save the dataframe as an FST file. The path of the saved file is being constructed using the "file.path" function, which is concatenating the "root" directory, the "data" directory, the "sensors" directory, and the name of the dataframe (which is being accessed using the "names" function and the index "i" of the loop) with the ".fst" file extension. The "compress" argument is set to 50, which means the data in the file will be compressed by 50% before it is saved.
#' 
## ----export the files-----------------------------------------------------------------------------------------------------------------
for (i in 1:length(verily_files)) {
  write.fst(x        = verily_files[[i]],
            path     = file.path(root,"data","sensors",paste0(names(verily_files)[i], ".fst")),
            compress = 50)
}

#' 
#' This code is reading in two CSV files, one called "Data_Dictionary_-Annotated.csv" and another called "Code_List-_Annotated.csv", using the "read.csv" function and specifying the "na.strings" argument as an empty string. The file paths for these two files are constructed using the "file.path" function, which is concatenating the "root" directory, the "data" directory, the "raw" directory, the "study_data" directory, the "study_docs" directory, and the "Data_and_Databases" directory.
#' 
#' Then, it creates a list of unique variable names from the "verily_files" list by using the "lapply" function to extract the names of each dataframe in the list, "reduce" function to concatenate all the names, and "unique" function to extract only unique names.
#' 
#' After that, it filters the "dictionary" dataframe by keeping only the rows where the "ITM_NAME" column is in the list of unique variable names.
#' 
#' Finally, it filters the "codelist" dataframe by keeping only the rows where the "ITM_NAME" column is in the list of unique variable names. There is a comment indicating that each level of the variables are not described in the code list.
#' 
## ----build a codebook metadata--------------------------------------------------------------------------------------------------------
dictionary = read.csv(file.path(root, "data", "raw", "study_data",
          "study_docs", "Data_and_Databases", "Data_Dictionary_-__Annotated_.csv"), 
          na.strings = "")

codelist = read.csv(file.path(root, "data", "raw", "study_data", "study_docs", "Data_and_Databases", "Code_List_-__Annotated_.csv"), na.strings = "")

names.list = lapply(verily_files, names) %>% reduce(c) %>% unique()

# here the timezone is missing, but it is UTC-05
dictionary = dictionary %>% filter(ITM_NAME %in% names.list)

# It seems that each level of the variables are not described in the code list.
codelist   = codelist %>% filter(ITM_NAME %in% names.list) 

write.xlsx(dictionary, file.path(root, "data", "sensors", paste0("verily_codebook.xlsx")))

#' 
#' This code creates a list of unique subject IDs for each of the dataframes in the 'verily_files' list. It then assigns the name of each dataframe to the corresponding element of the 'id.list' list. Finally, it uses the Reduce() function with the intersect operator to find the common subject IDs across all of the dataframes, which are stored in the 'common_ids' variable. This is done to identify the common set of subject IDs that have at least one row in each of the dataframes. Note that the number of common subject IDs may be lower if the analysis is restricted to a specific time window or if all data points over time are used.
#' 
## ----identify participants------------------------------------------------------------------------------------------------------------
id.list = list()
for (i in 1:length(verily_files)) {
  id.list[[i]] = unique(verily_files[[i]]$subject)
  names(id.list)[i] = names(verily_files)[i]
}

# This is the common id set in the dataframe
# These are the ids with at least one row in each dataframe. The real number may be lower if 
# we restrict the analysis to a specific time window or we use all the data points over time
common_ids = Reduce(intersect, id.list)

saveRDS(common_ids, file = file.path(root, "data", "sensors", "common ids.rds"))

#' 
#' This code creates a new cluster using the "new_cluster" function from the "parallel" package, which uses the number of cores on the system as a parameter. Then it partitions the "onwrist" dataframe from the "verily_files" list by the "subject" column and applies a series of dplyr functions on it:
#' 
#' It calculates the difference between the maximum and minimum values of the "age_seconds" column and assigns it to a new column "min_max"
#' It calculates the equivalent number of years for this difference and assigns it to a new column "years"
#' It selects only the "subject" and "years" columns and summarizes the data by taking the mean of the "years" column
#' It collects the resulting dataframe and adds a new column "days" which is calculated by multiplying the mean number of years by 365
#' Finally, it uses the "skim" function from the "skimr" package to print a summary of the data in the resulting dataframe "verily_duration"
#' 
## ----identify the duration------------------------------------------------------------------------------------------------------------
# So what is the time window of this study?
cl = new_cluster(detectCores())

part_cl = verily_files$onwrist %>% 
  group_by(subject) %>% 
  partition(cl)
verily_duration = part_cl %>% 
  mutate(min_max = max(age_seconds)- min(age_seconds),
         years   = min_max/31556952) %>% 
  dplyr::select(subject, years) %>% 
  summarise(years = mean(years)) %>% 
  collect()%>% 
  mutate(days = years*365) 

verily_duration %>% 
  skimr::skim()

write.xlsx(verily_duration, file = file.path(root, "data", "sensors", "verily duration in years.xlsx"))

#' 
#' 
#' 
