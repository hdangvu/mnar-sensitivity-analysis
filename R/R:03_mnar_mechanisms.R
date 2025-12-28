# R/03_mnar_mechanisms.R

MNAR_PROPS <- c(0.25, 0.40, 0.55, 0.70)

# Univariate scenarios (single variable goes missing)
MNAR_UNI_VARS <- c("final", "participation", "study")

# Multivariate scenarios (multiple variables go missing, driven by a mechanism variable)
MNAR_MULTI_SCENARIOS <- list(
  academic_disengagement = list(
    mech_variable = "final",
    target_pattern = c("final", "participation", "midterm"),
    type = "RIGHT"
  ),
  student_burnout = list(
    mech_variable = "stress",
    target_pattern = c("stress", "sleep", "study"),
    type = "RIGHT"
  )
)

# Convert to numeric frame for mice::ampute (only numeric columns allowed)
ampute_numeric_frame <- function(df) {
  df %>%
    dplyr::mutate(
      stress = as.numeric(stress),
      .keep = "unused"
    ) %>%
    dplyr::select(age, study, sleep, participation, midterm, final, stress)
}

# Univariate MNAR: missingness depends on the variable's own value (MNAR)
mnar_ampute_one <- function(df, target, prop = 0.10, type = c("LEFT", "RIGHT", "TAIL", "MID")) {
  type <- match.arg(type)
  
  numDF <- ampute_numeric_frame(df)
  if (!target %in% names(numDF)) stop("target not in numeric frame: ", target)
  
  patterns <- matrix(1, nrow = 1, ncol = ncol(numDF), dimnames = list(NULL, names(numDF)))
  patterns[1, target] <- 0
  
  w <- rep(0, ncol(numDF)); names(w) <- names(numDF); w[target] <- 1
  W <- matrix(w, nrow = 1)
  
  amp <- mice::ampute(
    data     = numDF,
    prop     = prop,
    patterns = patterns,
    mech     = "MNAR",
    weights  = W,
    type     = type
  )
  
  out <- df
  out[[target]] <- amp$amp[[target]]
  
  # restore stress factor if we amputated stress
  if (target == "stress") {
    out$stress <- factor(round(out$stress), levels = 1:10, ordered = TRUE)
  }
  
  out
}

# Multivariate MNAR: multiple targets missing; missingness driven by mech_variable
mnar_ampute_multi <- function(df, target_pattern, mech_variable, prop = 0.10, type = c("LEFT", "RIGHT", "TAIL", "MID")) {
  type <- match.arg(type)
  
  numDF <- ampute_numeric_frame(df)
  if (!mech_variable %in% names(numDF)) stop("mech_variable not in numeric frame: ", mech_variable)
  
  # weights: missingness depends only on mech_variable
  w <- rep(0, ncol(numDF)); names(w) <- names(numDF)
  w[mech_variable] <- 1
  W <- matrix(w, nrow = 1)
  
  patterns <- matrix(1, nrow = 1, ncol = ncol(numDF), dimnames = list(NULL, names(numDF)))
  patterns[1, target_pattern] <- 0
  
  amp <- mice::ampute(
    data     = numDF,
    prop     = prop,
    patterns = patterns,
    mech     = "MNAR",
    weights  = W,
    type     = type
  )
  
  out <- df
  for (col in target_pattern) {
    if (col %in% names(out)) out[[col]] <- amp$amp[[col]]
  }
  
  if ("stress" %in% target_pattern) {
    out$stress <- factor(round(out$stress), levels = 1:10, ordered = TRUE)
  }
  
  out
}

# Single clean entry point: apply a named scenario
apply_mnar_scenario <- function(df, scenario, prop) {
  
  # Univariate scenarios: final/participation/study
  if (scenario %in% MNAR_UNI_VARS) {
    # final = RIGHT tail missing, participation = LEFT tail missing, study = LEFT tail missing
    type <- dplyr::case_when(
      scenario == "final" ~ "RIGHT",
      scenario == "participation" ~ "LEFT",
      scenario == "study" ~ "LEFT",
      TRUE ~ "RIGHT"
    )
    
    return(mnar_ampute_one(df, target = scenario, prop = prop, type = type))
  }
  
  # Multivariate scenarios
  if (scenario %in% names(MNAR_MULTI_SCENARIOS)) {
    sc <- MNAR_MULTI_SCENARIOS[[scenario]]
    return(mnar_ampute_multi(
      df,
      target_pattern = sc$target_pattern,
      mech_variable  = sc$mech_variable,
      prop           = prop,
      type           = sc$type
    ))
  }
  
  stop("Unknown scenario: ", scenario)
}