quality_checker <- function(df, gp, tp, id) {
  # Subjects with RM
  individuals = df %>% 
    group_by(.data[[id]], .data[[gp]]) %>% 
    tally() %>% 
    filter(n > 1)
  
  l_individuals_total   = length(unique(df[[id]]))
  l_individuals         = length(unique(individuals[[id]]))
  median_rm_individuals = median(individuals$n)
  print(paste("We found", paste0(l_individuals,"/",l_individuals_total), "individuals with a median of", median_rm_individuals,"repeated measures"))
  
  # How many subject for label and timepoint
  indiv_timepoints = df %>% 
    filter(.data[[id]] %in% unique(individuals[[id]])) %>% 
    group_by(.data[[tp]],.data[[gp]]) %>% 
    tally() %>% 
    arrange(sort(.data[[tp]]))
  
  # Filtering out timepoints without all the groups
  indiv_timepoints_filtered = indiv_timepoints %>%
    tally() %>% 
    mutate(index = ifelse(n < length(unique(df[[gp]])), "discard", "keep")) %>% 
    filter(index == "keep")
  
  l_timepoint_original = length(unique(unique(df[[tp]] )))
  l_timepoint_filtered = length(unique(indiv_timepoints_filtered[[tp]]))
  
  if (l_timepoint_filtered == l_timepoint_original) {
    print("All the groups are represented in all the timepoint. No filtering occurred")
  } else {
    print(paste("We found", paste0(l_timepoint_filtered, "/", l_timepoint_original), "timepoint with all the groups"))
  }
  
  l_gp = length(unique(df[[gp]]))
  subject_number_by_filtered_timepoint = indiv_timepoints %>% 
    ungroup(.data[[tp]]) %>% 
    filter(.data[[tp]] %in% indiv_timepoints_filtered[[tp]]) %>% 
    group_by(.data[[tp]]) %>%
    summarise(n_sum = sum(n)) %>% 
    mutate(index    = round(n_sum/l_gp),
           decision = ifelse(index < 10, "discard", "keep")) %>% 
    filter(decision == "keep")
  
  l_timepoint_filtered_n_subject_x_group = length(subject_number_by_filtered_timepoint[[tp]])
  if (l_timepoint_filtered_n_subject_x_group == 0) {
    print("All the timepoint had n subjects x group >= 10, No timepoint were removed")
  } else {
    print(paste("We found",paste0(l_timepoint_filtered_n_subject_x_group, "/", l_timepoint_original), "timepoints with n subjects x group >= 10"))
  }
  
  n_indiv_x_label_and_timepoint = indiv_timepoints %>% 
    filter(.data[[tp]] %in% subject_number_by_filtered_timepoint[[tp]]) %>% 
    group_by(.data[[gp]]) %>% 
    summarise(n_sum = sum(n)) %>% 
    mutate(index    = round(n_sum/l_timepoint_filtered_n_subject_x_group),
           decision = ifelse(index < 10, "discard", "keep"))
  
  label_to_discard = n_indiv_x_label_and_timepoint[[gp]][n_indiv_x_label_and_timepoint$decision == "discard"]
  
  cond_group= is_empty(label_to_discard)
  cond_time = length(l_timepoint_filtered_n_subject_x_group) == l_timepoint_original
  
  if (cond_group == TRUE & cond_time == TRUE) {
    print("No adjustment needed")
  } else if (cond_group== FALSE & cond_time == FALSE) {
    print("Removing failed label and timepoints")
    df = df %>% 
      filter(!.data[[gp]] %in% label_to_discard) %>% 
      filter(.data[[tp]] %in% unique(subject_number_by_filtered_timepoint[[tp]]))
  } else if (cond_group == FALSE & cond_time == TRUE) {
    print("Adjusting only labels")
    df = df %>% 
      filter(!.data[[gp]] %in% label_to_discard)
  } else if (cond_group == TRUE & cond_time == FALSE) {
    print("Adjusting only timepoints")
    df = df %>% filter(.data[[tp]] %in% unique(subject_number_by_filtered_timepoint[[tp]]))
  } 
  
  if (cond_time == FALSE) {
    remaining_individuals = df %>% 
      group_by(.data[[id]]) %>% 
      tally() %>% 
      filter(n == l_timepoint_filtered_n_subject_x_group) %>% .$PATNO
    print("Finally selecting the remaining individuals")
    df = df %>% 
      filter(.data[[id]] %in% remaining_individuals)
  }
  
  updated_label = length(unique(df[[gp]]))
  temp = df %>% 
    group_by(.data[[tp]], .data[[gp]]) %>% 
    tally() %>% 
    ungroup() %>% 
    dplyr::select(-.data[[tp]]) %>% 
    distinct() %>% 
    mutate(index = ifelse(n < 10, "discard", "keep")) %>% 
    filter(index == "keep")
  
  remaining_labels = temp[[gp]]
  
  if (length(remaining_labels) != updated_label) {
    print("Filtering out groups with n subject x timepoint < 10")
    df = df %>% 
      filter(.data[[gp]] %in% remaining_labels)
  }
  
  return(df)
}

wrangler_rm <- function(df) {
  df = left_join(df %>% mutate(PATNO = as.character(PATNO)),
                 core_set %>% dplyr::select( PATNO, GENDER, label),
                 by = "PATNO") %>% 
    filter(str_detect(EVENT_ID, pattern = paste(c("BL", "V"), collapse = "|"))) %>% 
    relocate(GENDER,label, .before = EVENT_ID) %>% 
    filter(label %in% c("PDC1", "PDC2", "Healthy Control", "Prodromal")) %>% 
    mutate(EVENT_ID = str_replace_all(string = EVENT_ID, pattern = "V0", replacement = "V"),
           EVENT_ID = ifelse(str_count(EVENT_ID) == 2 & EVENT_ID != "BL", str_replace_all(EVENT_ID, "V", "V0"), EVENT_ID)) %>% 
    arrange(label, PATNO, EVENT_ID)
}

batch_csv_import <- function(pathDir) {
  listvalue = list.files(path       = pathDir,
                         pattern    = ".csv",
                         full.names = T) %>% lapply(read.csv) 
  names(listvalue) = list.files(path       = pathDir,
                                pattern    = ".csv",
                                full.names = F) %>% make.names() %>% tolower() %>% str_remove_all(".csv")
  
  return(listvalue)
}

wide_tester <- function(gp, dvs, df) {
  compa = expand.grid(df[[gp]] %>% unique(), 
                      df[[gp]] %>% unique()) %>%
    remove_redundant_combinations() %>%
    dplyr::filter(Var1 != Var2)
  
  storage = rec.list(len = c(length(dvs),nrow(compa)))
  
  for (i in 1:length(dvs)) {
    y = dvs[i]
    
    for (ii in 1:nrow(compa)) {
      comparison = c(compa[ii,][[1]], compa[ii,][[2]]) %>% as.character()
      formula    = paste("~", y, "+", gp) %>% as.formula()
      
      if (class(df[[y]]) %in% c("character", "factor")) {
        df_totest = df %>% 
          filter(.data[[gp]] %in% comparison)
        if (length(unique(df_totest[[y]])) <= 1) {
          print("Only one level in categorical variable spotted, skipping") 
          next
        }
        chisq = df_totest %>% 
          xtabs(formula = formula, data = .) %>% 
          chisq.test() %>% 
          broom::glance() 
        chisq_effect_size = df_totest %>% 
          xtabs(formula = formula, data = .) %>% 
          cramer_v()
        df_desc = df_totest %>%
          arrange(.data[[gp]]) %>% 
          group_by(.data[[gp]]) %>% 
          summarise(n = n()) %>% 
          mutate(n12 = paste0("n", row_number())) %>% 
          pivot_wider(names_from = n12,
                      values_from = n) %>% 
          mutate(group = paste0("group", row_number())) %>% 
          pivot_wider(names_from = group,
                      values_from = gp) %>%
          summarise_all(purrr::discard, is.na)
        
        storage[[i]][[ii]] = bind_cols(chisq, df_desc) %>% 
          mutate(comparison = paste(group1, group2, sep = "-"),
                 variable   = y,
                 effsize    = chisq_effect_size,
                 magnitude  = case_when(effsize <= 0.2 ~ "small",
                                        effsize > 0.2 & effsize <= 0.6 ~ "moderate",
                                        effsize > 0.6 ~ "large")) %>% 
          dplyr::select(variable, comparison, everything()) 
      }
      else { 
        formula = paste(y, "~", gp) %>% as.formula()
        df_totest     = df %>% filter(.data[[gp]] %in% comparison)
        w             = wilcox.test(formula = formula, data = df_totest) %>% broom::glance()
        w_effect_size = wilcox_effsize(formula = formula, data = df_totest)   
        desc_stat = df_totest %>% 
          group_by(.data[[gp]]) %>% 
          summarise(mean = mean(.data[[y]], na.rm = TRUE),
                    sd   = sd(.data[[y]],   na.rm = TRUE)) %>% 
          arrange(.data[[gp]]) %>% 
          mutate(group = paste0("group", row_number())) %>%
          dplyr::select(-.data[[gp]]) %>% 
          pivot_wider(names_from = "group",
                      values_from = c("mean", "sd"))
        
        storage[[i]][[ii]] =  bind_cols(w, w_effect_size, desc_stat) %>%
          rename(variable = .y.) %>%
          mutate(comparison = paste(group1, group2, sep = "-")) %>% 
          dplyr::select(variable, comparison, everything())
      }
    }
  }
  return(lapply(storage, function(x) x %>% reduce(bind_rows)) %>% 
           reduce(bind_rows) %>% 
           mutate(sig = ifelse(p.value <= 0.05, "yes", "no")) %>% 
           arrange(p.value) %>% 
           relocate(variable, comparison, .before = statistic) %>% 
           relocate(sig, effsize, magnitude, .after = p.value)
  )
}

effsizeLMER <- function(m) {
  lmfit       = lm(model.response(model.frame(m)) ~ fitted(m))
  # Correlation between the fitted and the observed values
  r2.corr.mer = summary(lmfit)$r.squared
  
  # Fi squared proposed in this paper: https://doi.org/10.1002/sim.1572
  fi_squared = 1-var(residuals(m))/(var(model.response(model.frame(m))))
  
  # These methods usually yield similar results. 
  # Source: https://stats.stackexchange.com/questions/95054/how-to-get-an-overall-p-value-and-effect-size-for-a-categorical-factor-in-a-mi
  
  return(data.frame(method     = c("r2.corr.mer", "fi_squared"),
                    effectsize = c(r2.corr.mer, fi_squared)))
}

timepoint_quality_checker <- function(df, gp, tp, id) {
  checker = 
    df %>% 
    group_by(.data[[gp]], .data[[tp]]) %>% 
    summarise(n = n()) %>% 
    arrange(.data[[tp]]) %>% 
    mutate(n_subjects = df[[id]] %>% unique() %>% length(),
           nrow = nrow(df),
           dataset_missingness = datasetmissingness(df)) %>% 
    ungroup() %>% 
    group_by(.data[[tp]]) %>% 
    mutate(n_subjects_by_time    = sum(n),
           flag                  = ifelse(n_subjects_by_time < 10, "n<10", "n>=10"),
           flag2                 = ifelse(n < 10, "n<10", "n>=10"),
           timepoint_is_balanced = case_when(flag == "n>=10" & flag2 == "n>=10" ~ TRUE,
                                             flag == "n<10"  & flag2 == "n<10" ~ FALSE,
                                             TRUE ~ FALSE),
           timepoint_is_balanced_summary = ifelse(sum(timepoint_is_balanced) <=1, FALSE, TRUE))
  return(checker)
}

timepoint_selector <- function(data, gp, tp, id, dvs, selected_period) {
  storage_df      = vector(mode = "list")
  storage_checker = vector(mode = "list")
  for (i in 1:length(dvs)) {
    y  = dvs[i]
    df = data %>% 
      dplyr::select(all_of(c(id,gp, y, tp))) %>% 
      distinct() %>% 
      filter(.data[[tp]] %in% selected_period)
    
    # Individuals with repeated measures
    rm_subj = df %>% 
      group_by(.data[[id]]) %>% 
      tally() %>% 
      filter(n == length(selected_period))
    rm_subj = rm_subj[[id]]
    
    # Final number of subject
    fsub = df %>% 
      filter(.data[[id]] %in% rm_subj) %>% 
      group_by(.data[[tp]], .data[[gp]]) %>% 
      tally()
    
    df = df %>% 
      filter(.data[[id]] %in% rm_subj)
    
    storage_df[[i]]      = df
    storage_checker[[i]] = fsub
  }
  
  names(storage_df)      = dvs
  names(storage_checker) = dvs
  return(list(df = storage_df, check = storage_checker))
  
}