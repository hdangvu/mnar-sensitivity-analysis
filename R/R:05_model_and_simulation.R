# R/05_model_and_simulation.R

generate_sim_bank_complete <- function(
    iterations,
    n,
    generate_predictors_all,
    attach_passing,
    fit_logit,
    train_df,
    seed = 123,
    verbose_every = 50
) {
  set.seed(seed)
  
  sim_bank_complete <- vector("list", length = iterations)
  
  message("Generating ", iterations, " complete synthetic datasets...")
  
  for (i in seq_len(iterations)) {
    Xi <- generate_predictors_all(n)
    Xi_y <- attach_passing(Xi, model = fit_logit, train_df = train_df)
    sim_bank_complete[[i]] <- Xi_y
    
    if (!is.null(verbose_every) && i %% verbose_every == 0) {
      message("Generated dataset ", i, " of ", iterations)
    }
  }
  
  message("Finished generating all datasets.")
  sim_bank_complete
}

# Align factor levels to the training schema
align_to_schema <- function(df_to_fix, schema_df) {
  for (name in names(df_to_fix)) {
    if (name %in% names(schema_df)) {
      tr_col <- schema_df[[name]]
      if (is.factor(tr_col)) {
        df_to_fix[[name]] <- factor(
          df_to_fix[[name]],
          levels = levels(tr_col),
          ordered = is.ordered(tr_col)
        )
      }
    }
  }
  df_to_fix
}

compute_metrics <- function(y_true, p_hat, threshold = 0.5) {
  pred_class <- ifelse(p_hat > threshold, 1, 0)
  acc <- mean(pred_class == y_true)
  auc <- as.numeric(pROC::roc(y_true, p_hat, quiet = TRUE)$auc)
  list(acc = acc, misc = 1 - acc, auc = auc)
}

run_mnar_simulation <- function(
    sim_bank_complete,
    data_for_sim,
    iterations,
    props_to_test,
    scenarios_uni,
    scenarios_multi,
    base_model_formula,
    threshold = 0.5,
    verbose_every = 50
) {
  
  # `data_for_sim` is the schema template (no id/grade)
  
  total_runs <- (length(scenarios_uni) + length(scenarios_multi)) * iterations * length(props_to_test)
  results_list <- vector("list", total_runs)
  idx <- 1
  start <- Sys.time()
  
  for (prop in props_to_test) {
    for (i in seq_len(iterations)) {
      
      df_complete <- sim_bank_complete[[i]]
      
      # stratified split
      split <- caTools::sample.split(df_complete$passing, SplitRatio = 0.8)
      
      df_train <- align_to_schema(subset(df_complete, split == TRUE), schema_df = data_for_sim)
      df_test  <- align_to_schema(subset(df_complete, split == FALSE), schema_df = data_for_sim)
      y_test <- df_test$passing
      
      # baseline
      model_baseline <- glm(base_model_formula, data = df_train, family = binomial(link = "logit"))
      p_base <- predict(model_baseline, newdata = df_test, type = "response")
      m_base <- compute_metrics(y_test, p_base, threshold = threshold)
      
      # ---------------------------
      # Univariate scenarios
      # ---------------------------
      for (scenario in scenarios_uni) {
        
        # Apply MNAR
        df_train_mnar <- apply_mnar_scenario(df_train, scenario = scenario, prop = prop)
        
        # Model 2: PMM only
        df_pmm <- impute_with_strategy(df_train_mnar, "PMM", exclude_target = TRUE, seed = i)
        df_pmm <- align_to_schema(df_pmm, schema_df = data_for_sim)
        
        model_pmm <- glm(base_model_formula, data = df_pmm, family = binomial(link = "logit"))
        p_pmm <- predict(model_pmm, newdata = df_test, type = "response")
        m_pmm <- compute_metrics(y_test, p_pmm, threshold = threshold)
        
        # Model 3: PMM + missingness flag
        flag_name <- paste0(scenario, "_is_missing")
        df_flag <- df_train_mnar
        df_flag[[flag_name]] <- ifelse(is.na(df_flag[[scenario]]), 1, 0)
        
        df_test_flag <- df_test
        df_test_flag[[flag_name]] <- 0
        
        base_formula_str <- paste(deparse(base_model_formula), collapse = "")
        flag_model_formula <- stats::as.formula(paste(base_formula_str, "+", flag_name))
        
        df_pmm_flag <- impute_with_strategy(df_flag, "PMM", exclude_target = TRUE, seed = i)
        df_pmm_flag <- align_to_schema(df_pmm_flag, schema_df = data_for_sim)
        
        model_pmm_flag <- glm(flag_model_formula, data = df_pmm_flag, family = binomial(link = "logit"))
        p_pmm_flag <- predict(model_pmm_flag, newdata = df_test_flag, type = "response")
        m_pmm_flag <- compute_metrics(y_test, p_pmm_flag, threshold = threshold)
        
        # Model 4/5: delta adjustments
        # correct direction based on scenario design
        mnar_type <- dplyr::case_when(
          scenario == "final" ~ "RIGHT",
          scenario == "participation" ~ "LEFT",
          scenario == "study" ~ "LEFT",
          TRUE ~ "RIGHT"
        )
        delta_correct <- ifelse(mnar_type == "RIGHT", "ADD", "SUBTRACT")
        delta_wrong   <- ifelse(mnar_type == "RIGHT", "SUBTRACT", "ADD")
        
        df_delta_correct <- apply_delta_adjustment(
          df_imputed = df_pmm,
          df_mnar = df_train_mnar,
          target_vars = c(scenario),
          direction = delta_correct
        )
        model_delta_correct <- glm(base_model_formula, data = df_delta_correct, family = binomial(link = "logit"))
        p_delta_correct <- predict(model_delta_correct, newdata = df_test, type = "response")
        m_delta_correct <- compute_metrics(y_test, p_delta_correct, threshold = threshold)
        
        df_delta_wrong <- apply_delta_adjustment(
          df_imputed = df_pmm,
          df_mnar = df_train_mnar,
          target_vars = c(scenario),
          direction = delta_wrong
        )
        model_delta_wrong <- glm(base_model_formula, data = df_delta_wrong, family = binomial(link = "logit"))
        p_delta_wrong <- predict(model_delta_wrong, newdata = df_test, type = "response")
        m_delta_wrong <- compute_metrics(y_test, p_delta_wrong, threshold = threshold)
        
        results_list[[idx]] <- data.frame(
          iteration = i,
          missing_aggressiveness = prop,
          scenario = scenario,
          
          auc_baseline = m_base$auc,
          acc_baseline = m_base$acc,
          misc_baseline = m_base$misc,
          
          auc_pmm = m_pmm$auc,
          acc_pmm = m_pmm$acc,
          misc_pmm = m_pmm$misc,
          
          auc_pmm_flag = m_pmm_flag$auc,
          acc_pmm_flag = m_pmm_flag$acc,
          misc_pmm_flag = m_pmm_flag$misc,
          
          auc_delta_dir_correct = m_delta_correct$auc,
          acc_delta_dir_correct = m_delta_correct$acc,
          misc_delta_dir_correct = m_delta_correct$misc,
          
          auc_delta_dir_wrong = m_delta_wrong$auc,
          acc_delta_dir_wrong = m_delta_wrong$acc,
          misc_delta_dir_wrong = m_delta_wrong$misc
        )
        
        idx <- idx + 1
      }
      
      # ---------------------------
      # Multivariate scenarios (named list)
      # ---------------------------
      for (scenario_name in names(scenarios_multi)) {
        sc <- scenarios_multi[[scenario_name]]
        
        df_train_mnar <- mnar_ampute_multi(
          df = df_train,
          target_pattern = sc$target_pattern,
          mech_variable = sc$mech_variable,
          prop = prop,
          type = sc$type
        )
        
        df_pmm <- impute_with_strategy(df_train_mnar, "PMM", exclude_target = TRUE, seed = i)
        df_pmm <- align_to_schema(df_pmm, schema_df = data_for_sim)
        
        model_pmm <- glm(base_model_formula, data = df_pmm, family = binomial(link = "logit"))
        p_pmm <- predict(model_pmm, newdata = df_test, type = "response")
        m_pmm <- compute_metrics(y_test, p_pmm, threshold = threshold)
        
        # PMM + flag (flag driven by mech variable)
        flag_name <- paste0(sc$mech_variable, "_is_missing")
        df_flag <- df_train_mnar
        df_flag[[flag_name]] <- ifelse(is.na(df_flag[[sc$mech_variable]]), 1, 0)
        
        df_test_flag <- df_test
        df_test_flag[[flag_name]] <- 0
        
        base_formula_str <- paste(deparse(base_model_formula), collapse = "")
        flag_model_formula <- stats::as.formula(paste(base_formula_str, "+", flag_name))
        
        df_pmm_flag <- impute_with_strategy(df_flag, "PMM", exclude_target = TRUE, seed = i)
        df_pmm_flag <- align_to_schema(df_pmm_flag, schema_df = data_for_sim)
        
        model_pmm_flag <- glm(flag_model_formula, data = df_pmm_flag, family = binomial(link = "logit"))
        p_pmm_flag <- predict(model_pmm_flag, newdata = df_test_flag, type = "response")
        m_pmm_flag <- compute_metrics(y_test, p_pmm_flag, threshold = threshold)
        
        # Delta correct/wrong based on RIGHT/LEFT
        delta_correct <- ifelse(sc$type == "RIGHT", "ADD", "SUBTRACT")
        delta_wrong   <- ifelse(sc$type == "RIGHT", "SUBTRACT", "ADD")
        
        df_delta_correct <- apply_delta_adjustment(
          df_imputed = df_pmm,
          df_mnar = df_train_mnar,
          target_vars = sc$target_pattern,
          direction = delta_correct
        )
        model_delta_correct <- glm(base_model_formula, data = df_delta_correct, family = binomial(link = "logit"))
        p_delta_correct <- predict(model_delta_correct, newdata = df_test, type = "response")
        m_delta_correct <- compute_metrics(y_test, p_delta_correct, threshold = threshold)
        
        df_delta_wrong <- apply_delta_adjustment(
          df_imputed = df_pmm,
          df_mnar = df_train_mnar,
          target_vars = sc$target_pattern,
          direction = delta_wrong
        )
        model_delta_wrong <- glm(base_model_formula, data = df_delta_wrong, family = binomial(link = "logit"))
        p_delta_wrong <- predict(model_delta_wrong, newdata = df_test, type = "response")
        m_delta_wrong <- compute_metrics(y_test, p_delta_wrong, threshold = threshold)
        
        results_list[[idx]] <- data.frame(
          iteration = i,
          missing_aggressiveness = prop,
          scenario = scenario_name,
          
          auc_baseline = m_base$auc,
          acc_baseline = m_base$acc,
          misc_baseline = m_base$misc,
          
          auc_pmm = m_pmm$auc,
          acc_pmm = m_pmm$acc,
          misc_pmm = m_pmm$misc,
          
          auc_pmm_flag = m_pmm_flag$auc,
          acc_pmm_flag = m_pmm_flag$acc,
          misc_pmm_flag = m_pmm_flag$misc,
          
          auc_delta_dir_correct = m_delta_correct$auc,
          acc_delta_dir_correct = m_delta_correct$acc,
          misc_delta_dir_correct = m_delta_correct$misc,
          
          auc_delta_dir_wrong = m_delta_wrong$auc,
          acc_delta_dir_wrong = m_delta_wrong$acc,
          misc_delta_dir_wrong = m_delta_wrong$misc
        )
        
        idx <- idx + 1
      }
      
      if (!is.null(verbose_every) && i %% verbose_every == 0) {
        message("Completed iteration: ", i, " for prop: ", prop)
      }
    }
  }
  
  end <- Sys.time()
  message("Total runtime: ", round(as.numeric(difftime(end, start, units = "mins")), 2), " minutes")
  
  dplyr::bind_rows(results_list)
}