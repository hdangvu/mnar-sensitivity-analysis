# R/04_imputation_methods.R

# PMM via mice (m = 1).
# By default excludes 'passing' from being imputed or used as a predictor (no target leakage).
impute_pmm <- function(df, exclude_target = TRUE, m = 1, maxit = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  meth <- mice::make.method(df)
  pred <- mice::make.predictorMatrix(df)
  
  if (exclude_target && "passing" %in% names(df)) {
    meth["passing"] <- ""
    pred[, "passing"] <- 0
  }
  
  imp <- mice::mice(
    df, m = m, maxit = maxit,
    method = meth, predictorMatrix = pred,
    printFlag = FALSE
  )
  
  mice::complete(imp)
}

# Random forest imputation (optional, heavier)
impute_rf <- function(df, exclude_target = TRUE, m = 1, maxit = 5, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  meth <- mice::make.method(df)
  pred <- mice::make.predictorMatrix(df)
  
  if (exclude_target && "passing" %in% names(df)) {
    meth["passing"] <- ""
    pred[, "passing"] <- 0
  }
  
  # set all non-empty methods to rf
  meth[meth != ""] <- "rf"
  
  imp <- mice::mice(
    df, m = m, maxit = maxit,
    method = meth, predictorMatrix = pred,
    printFlag = FALSE
  )
  
  mice::complete(imp)
}

# Delta adjustment: shift only the cells that were originally MNAR-missing.
# direction: ADD (correct) or SUBTRACT (incorrect)
apply_delta_adjustment <- function(df_imputed, df_mnar, target_vars, direction = c("ADD", "SUBTRACT"), k_sd = 1.0) {
  direction <- match.arg(direction)
  df_out <- df_imputed
  
  for (var in target_vars) {
    if (!var %in% names(df_out)) next
    na_mask <- is.na(df_mnar[[var]])
    if (sum(na_mask) == 0) next
    
    # numeric variables: shift by k * SD
    if (is.numeric(df_out[[var]])) {
      delta <- stats::sd(df_out[[var]], na.rm = TRUE) * k_sd
      if (direction == "ADD") {
        df_out[[var]][na_mask] <- df_out[[var]][na_mask] + delta
      } else {
        df_out[[var]][na_mask] <- df_out[[var]][na_mask] - delta
      }
      
      # ordered factors: shift on integer scale then clamp to level range
    } else if (is.factor(df_out[[var]]) && is.ordered(df_out[[var]])) {
      vals_int <- as.integer(df_out[[var]])
      delta <- stats::sd(vals_int, na.rm = TRUE) * k_sd
      
      vals_shifted <- vals_int
      if (direction == "ADD") {
        vals_shifted[na_mask] <- vals_shifted[na_mask] + delta
      } else {
        vals_shifted[na_mask] <- vals_shifted[na_mask] - delta
      }
      
      num_levels <- length(levels(df_out[[var]]))
      vals_shifted <- round(vals_shifted)
      vals_shifted <- pmin(pmax(vals_shifted, 1), num_levels)
      
      df_out[[var]] <- factor(
        vals_shifted,
        levels = 1:num_levels,
        labels = levels(df_out[[var]]),
        ordered = TRUE
      )
    }
  }
  
  df_out
}

# strategy: "PMM", "RF", "PMM+DELTA_ADD", "PMM+DELTA_SUB"
impute_with_strategy <- function(df_mnar, strategy, target_vars = NULL, exclude_target = TRUE, seed = NULL) {
  
  strategy <- toupper(strategy)
  
  if (strategy == "PMM") {
    return(impute_pmm(df_mnar, exclude_target = exclude_target, seed = seed))
  }
  
  if (strategy == "RF") {
    return(impute_rf(df_mnar, exclude_target = exclude_target, seed = seed))
  }
  
  if (strategy %in% c("PMM+DELTA_ADD", "PMM+DELTA_SUB")) {
    base <- impute_pmm(df_mnar, exclude_target = exclude_target, seed = seed)
    
    # if target_vars not provided, adjust all columns that contain any NA in df_mnar
    if (is.null(target_vars)) {
      target_vars <- names(df_mnar)[colSums(is.na(df_mnar)) > 0]
    }
    
    dir <- ifelse(strategy == "PMM+DELTA_ADD", "ADD", "SUBTRACT")
    return(apply_delta_adjustment(base, df_mnar, target_vars = target_vars, direction = dir))
  }
  
  stop("Unknown imputation strategy: ", strategy)
}