first_wrangler <- function(data, custom_gp_removal) {
  data = left_join(core_set %>% dplyr::select(PATNO, label),
                                data %>%
                                  mutate(PATNO = as.character(PATNO)), by = "PATNO") %>% 
    dplyr::select(PATNO, label, EVENT_ID,all_of(dvs)) %>% 
    pivot_longer(cols = all_of(dvs),
                 names_to = "dv",
                 values_to= "value",
                 values_transform = as.character) %>% 
    mutate(value    = na_if(value, ""),
           value    = na_if(value,"UR"),
           value    = na_if(value, "Indeterminate"),
           EVENT_ID = na_if(EVENT_ID, "")) %>%
    filter(!is.na(EVENT_ID))
  
  # CUSTOM REMOVAL OF PRODROMAL: PAY ATTENTION
  if (custom_gp_removal == TRUE) {
    data = data %>% filter(label %in%c("PDC1", "PDC2"))
  }
  return(data)
}

second_wrangler <- function(data) {
  # Time point sanity check and filtering (selected_period)
  data = data %>% 
    filter(str_detect(EVENT_ID, pattern = paste(c("BL", "V"), collapse = "|"))) %>%
    mutate(EVENT_ID = str_replace_all(string = EVENT_ID, pattern = "V0", replacement = "V"),
           EVENT_ID = ifelse(str_count(EVENT_ID) == 2 & EVENT_ID != "BL", str_replace_all(EVENT_ID, "V", "V0"), EVENT_ID),
           EVENT_ID = as.factor(EVENT_ID)) %>%
    filter(EVENT_ID %in% selected_period) %>% 
    mutate(EVENT_ID = droplevels(EVENT_ID))
  
  # Nesting
  data = data %>% 
    group_by(dv) %>%
    nest()
  
  # Adding information about the dependent variable
  y_annotation = codebook$data_dictionary_annotated %>% 
    filter(ITM_NAME %in% unique(data$dv)) %>% 
    dplyr::select(ITM_NAME, DSCR, ITM_TYPE) %>% 
    rename(dv = ITM_NAME)
  
  data = left_join(data, y_annotation, by = "dv") %>% 
    relocate(ITM_TYPE, DSCR, .before = data)
  rm(y_annotation)
  
  # 2 Dataframe Sanity check ----
  # Check the duplicates and decide what to do (manual) 
  data = data %>% 
    mutate(n_duplicates     = map(.x = data, .f =~ .x %>% duplicated() %>% sum()),
           patno_duplicates = map(.x = data, .f =~ .x$PATNO[which(duplicated(.x))]),
           data             = map(.x = data, .f =~ .x %>% distinct())) %>% 
    unnest(n_duplicates)
  
  # MVA ----
  # Group level (Are there groups that are totally empty regardless the timepoint?)
  # if yes  remove useless groups updating data
  data = data %>% 
    mutate(mva_discard_group_data = map(.x = data, .f=~  .x %>% 
                                          distinct() %>% 
                                          group_by(label) %>% 
                                          summarise(n_subjects  = n(),
                                                    n_na        = sum(is.na(value))) %>% 
                                          mutate(na_percentage = round((n_na/n_subjects)*100,2),
                                                 discard_group = ifelse(na_percentage >= 90, TRUE, FALSE),
                                          )),
           mva_group_to_discard = map(.x = mva_discard_group_data, .f =~ .x$label[.x$discard_group == TRUE]),
           data                 = map(.x = data, .f = ~ .x %>% 
                                        filter(!label %in% unlist(mva_group_to_discard)))) 
  
  # How many subjects with repeated measures?
  # Select only them and move on
  data = data %>% 
    mutate(rm_subjects = map(.x = data, .f =~ .x %>%
                               group_by(PATNO) %>%
                               tally() %>%
                               filter(n == length(selected_period)) %>%
                               .$PATNO),
           data       = map(.x = data, .f =~ .x %>% 
                              filter(PATNO %in% unlist(rm_subjects)))) 
  #  This check is needed because there are some dataset that are totally empty after rm selection
  data = data %>%
    mutate(is_data_empty = map(.x= data, .f = ~ plyr::empty(.x) )) %>%
    filter(is_data_empty == FALSE)
  
  if (plyr::empty(data) == TRUE) {
    print("Warning! No observation in the dataset after rm subjects selection")
  }
  
  # Verify NA percentage
  # in the dataset (dataset missingness)
  # in the rows (na_percentage_row_checker)
  # remove the na subjects in all timepoints. These subjects  have number  of na > 0
  # check how many subjects are left
  # Declare if n is sufficient to test
  data = data %>% 
    mutate(mva_start_dataset_missingness = unlist(map(.x = data, .f =~ round(datasetmissingness(.x)*100,2))),
           mva_na_percentage_row_checker = map(.x = data, .f =~ .x$PATNO[narows_percent(.x) > 0]),
           data                          = map(.x = data, .f =~ .x %>% filter(!PATNO %in% unlist(mva_na_percentage_row_checker))),
           mva_end_dataset_missingness   = unlist(map(.x = data, .f =~ round(datasetmissingness(.x)*100, 2))),
           mva_summary_n_subject         = map(.x = data, .f =~ .x %>% 
                                                 group_by(label, EVENT_ID) %>% 
                                                 summarise(n = n(),
                                                           is_testable = ifelse(n> 20, TRUE, FALSE),
                                                           .groups = "drop")),
           is_testable                   = map(.x = mva_summary_n_subject, .f= ~ .x$is_testable %>% all()))
  
  return(data)
}

third_tester <- function(data) {
  data = data %>% 
    mutate(data = map(.x = data, .f=~ if (ITM_TYPE == "CHAR") { .x %>% 
        mutate(value = as.factor(value))} else {
          .x %>% mutate(value = as.numeric(value))}
    ))
  
  # Check if Char variables are ordinal or binominal
  data = data %>% 
    mutate(nlev = unlist(map(.x= data, .f = ~ ifelse(ITM_TYPE == "CHAR", .x %>% .$value %>% nlevels(), NA_real_)))) %>% 
    relocate(nlev, .after = DSCR)
  
  
  data = data %>%
    mutate(modeling = map(.x = data, .f = ~ if (is_testable == TRUE & ITM_TYPE == "NUMBER") {
      try(numeric_modeling(df = .x,
                           gp = gp,
                           tp = tp,
                           id = id,
                           y  = y,
                           REML = REML))
    } else if (is_testable == TRUE & ITM_TYPE == "CHAR" & nlev == 2) {
      try(binomial_modeling(df = .x,
                            gp = gp,
                            tp = tp,
                            id = id,
                            y = y))
    } else if(is_testable == TRUE & ITM_TYPE == "CHAR" & nlev > 2) {
      try(ordinal_modeling(df = .x,
                           gp = gp,
                           tp = tp,
                           id = id,
                           y = y))
      
    }
    ))
  return(data)
}