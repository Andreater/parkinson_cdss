wide_rm_tester <- function(data, gp, tp, id, dvs, REML = FALSE) {
    res_storage        = vector(mode = "list", length = length(dvs))
    names(res_storage) = dvs
    for (i in 1:length(dvs)) {
    print("Initializing")
    y                  = dvs[i]
    y_class            = class(df[[y]])
    print(paste("Working on", y))
    ## MVA ----
    df = mva(df = data, id = id)
    
    ## Outlier Testing ----
    if (y_class == "numeric") {
      df = anomaly_detection(df = df, y = y, id = id)
    } else {
      print("Y is not numeric: Skipping outlier detection")
    }
    ## Imputations ----
    df = imputations(df = df, id = id)
    
    ## Model Testing ----
    print("Model Testing...")
    if (y_class == "numeric") {
      print("Numerical variable spotted")
      res_storage[[i]] = numeric_modeling(df = df, gp = gp, tp = tp, id = id, y = y, REML = REML)  
    } else if (y_class == "factor" & length(unique(df[[y]])) < 2) {
      print("The factor variable is costant, skipping testing")
    } else if (y_class == "factor" & length(unique(df[[y]])) == 2) {
      print("Binomial Variable Spotted: Fitting GLMER")
      res_storage[[i]] = binomial_modeling(df = df, gp = gp, tp = tp, id = id, y = y)
    } else {
      print("Ordinal variable Spotted, fitting CLMM")
      res_storage[[i]] = ordinal_modeling(df = df, gp = gp, tp = tp, id = id, y = y)
    }
  } # CHIUDI FOR
  return(res_storage)
}# CHIUDI F

## Perform a top level MVA ----
mva <- function(df, id) {
  print("Performing MVA")
  na_percentage_column_checker = names(df)[nacolumns_percent(df) >= 40]
  na_percentage_row_checker    = df[[id]][narows_percent(df) >=40]
  
  if (is_empty(na_percentage_column_checker) == FALSE) {
    print("Removing columns with NAs n>=40%")
    df = df %>% 
      dplyr::select(-all_of(na_percentage_column_checker))
    na_percentage_row_checker = df[[id]][narows_percent(df) >=40]
    if (is_empty(na_percentage_row_checker) == FALSE) {
      print("Removing rows with NAs n>=40%")
      df = df %>% 
        filter(! .data[[id]] %in% na_percentage_row_checker)}
  } else if (is_empty(na_percentage_row_checker) == FALSE) {
    print("Removing rows with NAs n>=40%")
    df = df %>% 
      filter(! .data[[id]] %in% na_percentage_row_checker)
  } else {
    print("NAs in the dataset <40%")
  }
  return(df)
}

## Anomaly detection ----
anomaly_detection <- function(df, y, id) {
  print("Performing Anomaly Detection")
  y_to_test        = df[[y]]
  names(y_to_test) = df[[id]]
  names_with_na    = names(y_to_test)[is.na(y_to_test)]
  outliers_names   = boxplot(y_to_test, plot = FALSE)$out %>% names() %>% unique()
  
  if (length(outliers_names)>0) {
    print("Outliers filtered based on BOX-PLOT/IQR method")
    df = df %>% 
      filter(! PATNO %in% outliers_names)
  } else {
    print("No outlier detected")
  }
  return(df)
}


## Imputations ----
imputations <- function(df, id) {
  na_number = sum(is.na(df))
  if (na_number > 0) {
    datamissingness = datasetmissingness(df)
    print(paste("DF has", paste0(round(datamissingness*100,2), "%"), "missingness"))
    print("Performing Imputations")
    if (datamissingness*100 > 5) {
      temp =  mice(data            = df,
                   m               = 5,
                   maxit           = 5,
                   method          = "pmm",
                   predictorMatrix = quickpred(data = df, exclude = id),
                   seed            = 12345,
                   printFlag       = FALSE) %>% invisible()
      df = complete(temp)
    } else {
      temp =  mice(data            = df,
                   m               = round(datamissingness*100),
                   maxit           = 5,
                   method          = "pmm",
                   predictorMatrix = quickpred(data = df, exclude = id),
                   seed            = 12345,
                   printFlag       = FALSE) %>% invisible()
      df = complete(temp)
    }
  } else {
    print("No imputations nedeed. Skipping to test phase")
  }
  return(df)
}
## Numeric modeling ----
numeric_modeling <- function(df, gp, tp, id, y, REML) {
  storage = data.frame(tp_x_gp_p.value = double(),
                       gp_p.value      = double(),
                       bl_p.value      = double())
  # Interaction testing 
  int_f_formula     = paste(y, "~", gp, "*", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  int_r_formula     = paste(y, "~", gp, "+", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  int_full_model    = lmer(formula = int_f_formula, data = df, REML = REML)
  int_reduced_model = lmer(formula = int_r_formula, data = df, REML = REML)
  
  # Interaction testing with Likelihood Ratio Test
  aov        = anova(int_full_model, int_reduced_model)
  int_pvalue = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
  
  # GP Testing 
  fixed_f_formula     = paste(y, "~", gp, "+", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  fixed_r_formula     = paste(y, "~", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  fixed_full_model    = lmer(formula = fixed_f_formula, data = df, REML = REML)
  fixed_reduced_model = lmer(formula = fixed_r_formula, data = df, REML = REML)
  
  # Likelihood ratio test via ANOVA (Likelihood ratio test)
  aov       = anova(fixed_full_model, fixed_reduced_model)
  gp_pvalue = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
  
  ## Baseline modeling ----
  baseline_formula = paste(y, "~ 1","+", paste0("(1|", id, ")")) %>% as.formula()
  baseline_model   = lmer(formula = baseline_formula, data = df, REML = REML)
  
  ## Define the winning model ----
  if (int_pvalue <= 0.05){
    print("Interaction Model is significant: comparing with the BL model")
    selected_model = int_full_model
    aov            = anova(selected_model, baseline_model)
    bl_pvalue      = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
    post_hoc       = emmeans(selected_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq            = rsq.glmm(selected_model, adj = TRUE)
    # Storing the results from the various Likelihood Ratio Test
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    
    # Building the model result dataframe
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq$model, 
             rsq_adj_fixed = rsq$fixed, 
             rsq_adj_random= rsq$random)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean = mean(.data[[y]]),
                sd  = sd(.data[[y]]),
                .groups = "drop") 
    
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
    
  } else if(gp_pvalue <= 0.05 & int_pvalue > 0.05) {
    print("GP is significant: Comparing with the BL model")
    selected_model = fixed_full_model
    aov            = anova(selected_model, baseline_model)
    bl_pvalue      = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
    post_hoc       = emmeans(selected_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq            = rsq.glmm(selected_model, adj = TRUE)
    
    # Storing the results from the various Likelihood Ratio Test
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    # Building the model result dataframe
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq$model, 
             rsq_adj_fixed = rsq$fixed, 
             rsq_adj_random= rsq$random)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean = mean(.data[[y]]),
                sd  = sd(.data[[y]]),
                .groups = "drop") 
    
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  } else {
    print("No gp or gp x tp interaction were significant: No model was compared with the BL model")
    print("Reporting values from the not significant interaction model")
    selected_model = NULL 
    bl_pvalue      = NA_real_
    post_hoc       = emmeans(int_full_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq            = rsq.glmm(int_full_model, adj = TRUE)
    # Storing the results from the various Likelihood Ratio Test
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    # Building the model result dataframe
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq$model, 
             rsq_adj_fixed = rsq$fixed, 
             rsq_adj_random= rsq$random)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean = mean(.data[[y]]),
                sd  = sd(.data[[y]]),
                .groups = "drop") 
    
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  }
  model_res_list = list(df = df, storage = storage, selected_model = selected_model)
return(model_res_list)
}

## Ordinal modeling ----
ordinal_modeling <- function(df, gp, tp, id, y) {
  # Model Testing
  storage = data.frame(tp_x_gp_p.value = double(),
                       gp_p.value      = double(),
                       bl_p.value      = double())
  # Set a control object with proprer optimizator
  control = clm.control(method = "nlminb", maxIter = 100)
  
  # Interaction testing 
  int_f_formula     = paste(y, "~", gp, "*", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  int_r_formula     = paste(y, "~", gp, "+", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  int_full_model    = clmm(formula = int_f_formula, data = df, control = control)
  int_reduced_model = clmm(formula = int_r_formula, data = df, control = control)
  
  #Interaction testing 
  aov        = anova(int_full_model, int_reduced_model)
  int_pvalue = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
  
  # GP Testing
  fixed_f_formula     = paste(y, "~", gp, "+", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  fixed_r_formula     = paste(y, "~", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  fixed_full_model    = clmm(formula = fixed_f_formula, data = df, control = control)
  fixed_reduced_model = clmm(formula = fixed_r_formula, data = df, control = control)
  
  # Likelihood ratio test via ANOVA
  aov       = anova(fixed_full_model, fixed_reduced_model)
  gp_pvalue = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
  
  # Baseline testing with Likelihood Ratio Test
  baseline_formula = paste(y, "~ 1","+", paste0("(1|", id, ")")) %>% as.formula()
  baseline_model   = clmm(formula = baseline_formula, data = df, control = control)
  
  if (int_pvalue <= 0.05){
    print("Interaction Model is significant: comparing with the BL model")
    selected_model = int_full_model
    aov            = anova(selected_model, baseline_model)
    bl_pvalue      = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
    post_hoc       = emmeans(selected_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq            = nagelkerke(fit = selected_model, null = int_reduced_model)
    rsq            = rsq$Pseudo.R.squared.for.model.vs.null[[3]] # Selecting the nagelkerke method
    # Storing the results from the various Likelihood Ratio Test
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    # Building the model result dataframe
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq, 
             rsq_adj_fixed = NA_real_, 
             rsq_adj_random=  NA_real_)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean = NA_real_,
                sd  = NA_real_,
                .groups = "drop") 
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  } else if(gp_pvalue <= 0.05 & int_pvalue > 0.05) {
    print("Only gp is significant: Comparing with the BL model")
    selected_model = fixed_full_model
    aov            = anova(selected_model, baseline_model)
    bl_pvalue      = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
    post_hoc       = emmeans(selected_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq            = nagelkerke(fit = selected_model, null = baseline_model)
    rsq            = rsq$Pseudo.R.squared.for.model.vs.null[[3]] # Selecting the nagelkerke method
    # Storing the results from the various Likelihood Ratio Test
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    # Building the model result dataframe
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq, 
             rsq_adj_fixed = NA_real_, 
             rsq_adj_random= NA_real_)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean = NA_real_,
                sd  = NA_real_,
                .groups = "drop")
    
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  } else {
    print("No fixed effect is significant: No model was compared with the BL model")
    bl_pvalue      = NA_real_
    selected_model = NULL
    # Storing the model testing results
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    
    # Building the model result dataframe
    post_hoc  = emmeans(int_full_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq       = nagelkerke(fit = int_full_model, null = int_reduced_model)
    rsq       = rsq$Pseudo.R.squared.for.model.vs.null[[3]] # Selecting the nagelkerke method
    
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq, 
             rsq_adj_fixed = NA_real_, 
             rsq_adj_random= NA_real_)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean    = NA_real_,
                sd      = NA_real_,
                .groups = "drop") 
    
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  }
  model_res_list = list(df = df, storage = storage, selected_model = selected_model)
  return(model_res_list)
}

## Binomial modeling ----
binomial_modeling <- function(df, gp, tp, id, y) {
  # Model Testing
  storage = data.frame(tp_x_gp_p.value = double(),
                       gp_p.value      = double(),
                       bl_p.value      = double())
  
  # Set a control object for the glmer
  glmercontrol = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  
  # Interaction testing 
  int_f_formula     = paste(y, "~", gp, "*", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  int_r_formula     = paste(y, "~", gp, "+", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  int_full_model    = glmer(formula = int_f_formula, data = df,family = "binomial", control = glmercontrol)
  int_reduced_model = glmer(formula = int_r_formula, data = df,family = "binomial", control = glmercontrol)
  
  #Interaction testing 
  aov        = anova(int_full_model, int_reduced_model)
  int_pvalue = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
  
  # GP Testing
  fixed_f_formula     = paste(y, "~", gp, "+", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  fixed_r_formula     = paste(y, "~", tp, "+", paste0("(1|", id, ")")) %>% as.formula()
  fixed_full_model    = glmer(formula = fixed_f_formula, data = df,family = "binomial", control = glmercontrol)
  fixed_reduced_model = glmer(formula = fixed_r_formula, data = df,family = "binomial", control = glmercontrol)
  
  # Likelihood ratio test via ANOVA
  aov       = anova(fixed_full_model, fixed_reduced_model)
  gp_pvalue = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
  
  # Baseline testing with Likelihood Ratio Test
  baseline_formula = paste(y, "~ 1","+", paste0("(1|", id, ")")) %>% as.formula()
  baseline_model   = glmer(formula = baseline_formula, data = df, family = "binomial", control = glmercontrol)
  
  if (int_pvalue <= 0.05){
    print("Interaction Model is significant: comparing with the BL model")
    selected_model = int_full_model
    aov            = anova(selected_model, baseline_model)
    bl_pvalue      = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
    post_hoc       = emmeans(selected_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq            = rsq(selected_model, adj = TRUE)
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    # Building the model result dataframe
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq$model, 
             rsq_adj_fixed = rsq$fixed, 
             rsq_adj_random=  rsq$random)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean = NA_real_,
                sd  = NA_real_,
                .groups = "drop") 
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  } else if(gp_pvalue <= 0.05 & int_pvalue > 0.05) {
    print("Only gp is significant: Comparing with the BL model")
    selected_model = fixed_full_model
    aov            = anova(selected_model, baseline_model)
    bl_pvalue      = aov %>% as_tibble() %>% na.omit() %>% .$`Pr(>Chisq)`
    post_hoc       = emmeans(selected_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq            = rsq(selected_model, adj = TRUE)
    # Storing the results from the various Likelihood Ratio Test
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    # Building the model result dataframe
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq$model, 
             rsq_adj_fixed = rsq$fixed, 
             rsq_adj_random= rsq$random)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean = NA_real_,
                sd  = NA_real_,
                .groups = "drop")
    
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  } else {
    print("No fixed effect is significant: No model was compared with the BL model")
    bl_pvalue      = NA_real_
    selected_model = int_full_model
    # Storing the model testing results
    storage = storage %>%
      add_row(tp_x_gp_p.value = int_pvalue,
              gp_p.value      = gp_pvalue,
              bl_p.value      = bl_pvalue)
    
    # Building the model result dataframe
    post_hoc  = emmeans(int_full_model, specs = as.formula(paste("pairwise", "~", gp, "|", tp)), adjust = "tukey")
    rsq       = rsq(selected_model, adj =TRUE)
    
    contrasts = post_hoc$contrasts %>% 
      broom::tidy() %>%
      mutate(y = y, 
             contrast = str_remove_all(contrast, " ")) %>% 
      mutate(method = "Tukey HSD",
             df_adj = "Kenward-roger") %>% 
      dplyr::select(-c(term)) %>% 
      relocate(df_adj, method, .after= df) %>% 
      relocate(y, .before = everything()) %>% 
      mutate(rsq_adj_model = rsq$model, 
             rsq_adj_fixed = rsq$fixed, 
             rsq_adj_random= rsq$random)
    info = df %>% 
      group_by(.data[[gp]], .data[[tp]]) %>% 
      summarise(n = n(),
                mean    = NA_real_,
                sd      = NA_real_,
                .groups = "drop") 
    
    contrasts = suppressMessages(left_join(contrasts, info))
    storage   = bind_cols(contrasts, storage)
  }
  model_res_list = list(df = df, storage = storage, selected_model = selected_model)
  return(model_res_list)
}
