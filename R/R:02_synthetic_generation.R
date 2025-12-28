# R/02_synthetic_generation.R

run_synthetic_generation <- function(
    data,
    n = 5000,
    seed = 123,
    fig2_path = "figures/fig2_real_vs_synth.png"
) {
  
  # 1) Fit logistic model for label generation
  data_model <- data %>%
    dplyr::mutate(
      grade     = factor(grade, levels = c("F","D","C","B","A"), ordered = TRUE),
      internet  = factor(internet, levels = c("No","Yes")),
      education = factor(education,
                         levels = c("None","High School","Bachelor's","Master's","PhD"),
                         ordered = TRUE),
      income    = factor(income, levels = c("Low","Medium","High"), ordered = TRUE),
      stress    = factor(stress, levels = 1:10, ordered = TRUE),
      
      participation = as.numeric(participation),
      midterm       = as.numeric(midterm),
      final         = as.numeric(final),
      age           = as.numeric(age),
      study         = as.numeric(study),
      sleep         = as.numeric(sleep),
      
      passing = ifelse(grade %in% c("A","B","C"), 1L, 0L)
    )
  
  fit_logit <- glm(
    passing ~ age + internet + education + income +
      study + sleep + stress +
      participation + midterm + final,
    data = data_model,
    family = binomial(link = "logit")
  )
  
  # 2) Fit marginals
  fit_uniform <- function(x, trim = 0.01) {
    x <- x[is.finite(x)]
    a <- as.numeric(stats::quantile(x, probs = trim))
    b <- as.numeric(stats::quantile(x, probs = 1 - trim))
    list(a = a, b = b)
  }
  
  fit_multinom <- function(fct) {
    fct <- factor(fct)
    lev <- levels(fct)
    p <- as.numeric(prop.table(table(fct)))
    names(p) <- lev
    list(levels = lev, prob = p)
  }
  
  age_fit   <- fit_uniform(data_model$age)
  study_fit <- fit_uniform(data_model$study)
  sleep_fit <- fit_uniform(data_model$sleep)
  part_fit  <- fit_uniform(data_model$participation)
  mid_fit   <- fit_uniform(data_model$midterm)
  final_fit <- fit_uniform(data_model$final)
  
  stress_levels <- 1:10
  stress_probs <- as.numeric(prop.table(table(factor(data_model$stress, levels = stress_levels))))
  stress_fit <- list(levels = stress_levels, prob = stress_probs)
  
  internet_fit  <- fit_multinom(data_model$internet)
  education_fit <- fit_multinom(data_model$education)
  income_fit    <- fit_multinom(data_model$income)
  
  # 3) Mixed correlation via hetcor -> Gaussian copula
  data_mixed <- data_model %>%
    dplyr::mutate(
      age           = as.numeric(age),
      study         = as.numeric(study),
      sleep         = as.numeric(sleep),
      participation = as.numeric(participation),
      midterm       = as.numeric(midterm),
      final         = as.numeric(final),
      
      stress    = factor(stress, levels = 1:10, ordered = TRUE),
      education = factor(education,
                         levels = c("None","High School","Bachelor's","Master's","PhD"),
                         ordered = TRUE),
      income    = factor(income, levels = c("Low","Medium","High"), ordered = TRUE),
      internet  = factor(internet, levels = c("No","Yes"))
    ) %>%
    dplyr::select(
      age, study, sleep, participation, midterm, final,
      stress, education, income, internet
    ) %>%
    tidyr::drop_na()
  
  hc <- polycor::hetcor(data_mixed, std.err = FALSE)
  R_mix <- hc$correlations
  R_mix_pd <- as.matrix(Matrix::nearPD(R_mix)$mat)
  
  cop_all <- copula::normalCopula(
    copula::P2p(R_mix_pd),
    dim = ncol(R_mix_pd),
    dispstr = "un"
  )
  
  # 4) Sampling helpers + generator
  clamp01 <- function(u, eps = 1e-12) pmin(pmax(u, eps), 1 - eps)
  
  q_uniform_fit <- function(u, fit) {
    stats::qunif(clamp01(u), min = fit$a, max = fit$b)
  }
  
  r_from_u <- function(u, levels, probs) {
    br <- c(0, cumsum(probs))
    idx <- findInterval(clamp01(u), br, rightmost.closed = TRUE)
    factor(levels[idx], levels = levels)
  }
  
  generate_predictors_all <- function(n) {
    U <- copula::rCopula(n, cop_all)
    colnames(U) <- colnames(R_mix_pd)
    
    age <- round(q_uniform_fit(U[, "age"], age_fit))
    age <- pmin(pmax(age, 18), 24)
    
    study <- q_uniform_fit(U[, "study"], study_fit)
    sleep <- q_uniform_fit(U[, "sleep"], sleep_fit)
    participation <- q_uniform_fit(U[, "participation"], part_fit)
    midterm <- q_uniform_fit(U[, "midterm"], mid_fit)
    final <- q_uniform_fit(U[, "final"], final_fit)
    
    stress <- r_from_u(U[, "stress"], as.character(stress_fit$levels), stress_fit$prob)
    education <- r_from_u(U[, "education"], education_fit$levels, education_fit$prob)
    income <- r_from_u(U[, "income"], income_fit$levels, income_fit$prob)
    internet <- r_from_u(U[, "internet"], internet_fit$levels, internet_fit$prob)
    
    tibble::tibble(
      age = as.numeric(age),
      study = as.numeric(study),
      sleep = as.numeric(sleep),
      participation = as.numeric(participation),
      midterm = as.numeric(midterm),
      final = as.numeric(final),
      
      stress = factor(stress, levels = as.character(stress_fit$levels), ordered = TRUE),
      education = factor(education, levels = education_fit$levels, ordered = TRUE),
      income = factor(income, levels = income_fit$levels, ordered = TRUE),
      internet = factor(internet, levels = internet_fit$levels)
    )
  }
  
  align_to_model_schema <- function(newdata, model, train_df) {
    terms_obj <- stats::terms(model)
    pred_vars <- attr(terms_obj, "term.labels")
    
    out <- newdata
    for (nm in pred_vars) {
      tr_col <- train_df[[nm]]
      if (is.factor(tr_col)) {
        out[[nm]] <- factor(out[[nm]], levels = levels(tr_col), ordered = is.ordered(tr_col))
      } else if (is.numeric(tr_col)) {
        out[[nm]] <- as.numeric(out[[nm]])
      }
    }
    out
  }
  
  attach_passing <- function(Xsyn, model, train_df) {
    train_schema <- train_df %>% dplyr::select(-id, -grade, -passing)
    
    X_aligned <- align_to_model_schema(Xsyn, model = model, train_df = train_schema)
    
    p <- stats::predict(model, newdata = X_aligned, type = "response")
    p <- pmin(pmax(p, .Machine$double.eps), 1 - .Machine$double.eps)
    
    X_aligned$passing <- stats::rbinom(n = nrow(X_aligned), size = 1, prob = p)
    X_aligned
  }
  
  # 5) Generate one synthetic dataset + validation figure (Figure 2)
  set.seed(seed)
  synthetic_X <- generate_predictors_all(n)
  synthetic <- attach_passing(synthetic_X, model = fit_logit, train_df = data_model)
  
  # Overlay plots
  plot_numeric_overlay <- function(v) {
    dplyr::bind_rows(
      data_model %>% dplyr::transmute(value = .data[[v]], src = "real"),
      synthetic %>% dplyr::transmute(value = .data[[v]], src = "synthetic")
    ) %>%
      ggplot2::ggplot(ggplot2::aes(value, fill = src)) +
      ggplot2::geom_histogram(position = "identity", alpha = 0.45, bins = 30) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::labs(title = paste("Real vs Synthetic:", v), x = v, y = "Count")
  }
  
  plot_categorical_overlay <- function(v) {
    r <- data_model[[v]]
    s <- synthetic[[v]]
    if (is.factor(r)) s <- factor(s, levels = levels(r), ordered = is.ordered(r))
    
    df <- dplyr::bind_rows(
      tibble::tibble(var = r, src = "real"),
      tibble::tibble(var = s, src = "synthetic")
    )
    
    ggplot2::ggplot(df, ggplot2::aes(x = var, fill = src)) +
      ggplot2::geom_bar(position = "fill") +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(title = paste("Proportion Overlay:", v), x = v, y = "Proportion")
  }
  
  fig2 <- (
    plot_numeric_overlay("final") +
      plot_numeric_overlay("participation") +
      plot_numeric_overlay("study") +
      plot_categorical_overlay("stress")
  ) +
    patchwork::plot_layout(ncol = 2) +
    patchwork::plot_annotation(
      title = "Figure 2: Original vs Synthetic Data Distribution",
      theme = ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
    )
  
  dir.create(dirname(fig2_path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(fig2_path, plot = fig2, width = 11, height = 8, dpi = 300)
  
  # Return everything the simulation loop will need
  list(
    fit_logit = fit_logit,
    R_mix_pd = R_mix_pd,
    cop_all = cop_all,
    marginals = list(
      age_fit = age_fit, study_fit = study_fit, sleep_fit = sleep_fit,
      part_fit = part_fit, mid_fit = mid_fit, final_fit = final_fit,
      stress_fit = stress_fit,
      internet_fit = internet_fit, education_fit = education_fit, income_fit = income_fit
    ),
    generator = list(
      generate_predictors_all = generate_predictors_all,
      attach_passing = attach_passing
    ),
    synthetic = synthetic,
    fig2_path = fig2_path,
    synth_pass_rate = mean(synthetic$passing)
  )
}