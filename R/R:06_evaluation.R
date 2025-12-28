# R/06_evaluation.R

summarize_results <- function(all_results) {
  all_results %>%
    dplyr::group_by(missing_aggressiveness, scenario) %>%
    dplyr::summarize(
      dplyr::across(dplyr::starts_with("auc_"), mean, .names = "{.col}"),
      dplyr::across(dplyr::starts_with("acc_"), mean, .names = "{.col}"),
      dplyr::across(dplyr::starts_with("misc_"), mean, .names = "{.col}"),
      .groups = "drop"
    )
}

to_long_format <- function(results_summary) {
  results_summary %>%
    tidyr::pivot_longer(
      cols = -c(missing_aggressiveness, scenario),
      names_to = "name",
      values_to = "value"
    ) %>%
    tidyr::separate(
      name,
      into = c("metric", "model_type"),
      sep = "_",
      extra = "merge"
    ) %>%
    dplyr::mutate(
      metric = factor(metric,
                      levels = c("auc", "acc", "misc"),
                      labels = c("AUC", "Accuracy", "Misclassification")),
      model_type = factor(
        model_type,
        levels = c("baseline", "pmm_flag", "pmm", "delta_dir_correct", "delta_dir_wrong"),
        labels = c("Baseline", "PMM_Flag", "PMM_Only", "Delta_Dir_Correct", "Delta_Dir_Wrong")
      )
    )
}

plot_metric_facet <- function(results_long, metric_name = "Accuracy") {
  gg <- results_long %>%
    dplyr::filter(metric == metric_name) %>%
    ggplot2::ggplot(ggplot2::aes(
      x = missing_aggressiveness, y = value,
      color = model_type, group = model_type, linetype = model_type
    )) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ scenario, ncol = 3) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::labs(
      title = paste("Impact of MNAR on Model Performance:", metric_name),
      x = "Percent of Data Missing",
      y = metric_name,
      color = "Model Type",
      linetype = "Model Type"
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(legend.position = "bottom",
                   strip.text = ggplot2::element_text(face = "bold"))
  
  # metric-specific y formatting
  if (metric_name == "AUC") {
    gg <- gg + ggplot2::scale_y_continuous(labels = scales::number_format(accuracy = 0.001))
  } else {
    gg <- gg + ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
  }
  
  gg
}

plot_high_impact_accuracy <- function(results_long) {
  figure_data <- results_long %>%
    dplyr::filter(
      scenario == "academic_disengagement",
      metric == "Accuracy",
      model_type != "PMM_Flag"
    ) %>%
    dplyr::mutate(
      model_type = factor(
        model_type,
        levels = c("Baseline", "PMM_Only", "Delta_Dir_Correct", "Delta_Dir_Wrong"),
        labels = c("Baseline", "PMM Only", "Delta Correct", "Delta Incorrect")
      )
    )
  
  ggplot2::ggplot(figure_data,
                  ggplot2::aes(x = missing_aggressiveness, y = value,
                               color = model_type, group = model_type, linetype = model_type)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Impact of Imputation Strategy on Model Accuracy",
      subtitle = "MNAR Scenario: Multivariate High Impact (Academic Disengagement)",
      x = "Missingness Intensity",
      y = "Classification Accuracy",
      color = "Repair Strategy",
      linetype = "Repair Strategy"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "right",
                   panel.grid.minor = ggplot2::element_blank())
}

plot_degradation_by_feature_importance <- function(results_long) {
  fig_data <- results_long %>%
    dplyr::filter(
      metric == "Accuracy",
      model_type == "PMM_Only",
      scenario %in% c("final", "participation", "study")
    ) %>%
    dplyr::mutate(
      scenario_label = factor(
        scenario,
        levels = c("final", "participation", "study"),
        labels = c("Final Exam (High Corr)", "Participation (Mod. Corr.)", "Study Hours (Low Corr.)")
      )
    )
  
  ggplot2::ggplot(fig_data,
                  ggplot2::aes(x = missing_aggressiveness, y = value,
                               color = scenario_label, group = scenario_label)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_x_continuous(labels = scales::percent_format()) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    ggplot2::labs(
      title = "Degradation Profiles by Feature Importance",
      subtitle = "PMM Only across MNAR Univariate Scenarios",
      x = "Missingness Intensity",
      y = "Classification Accuracy",
      color = "Missing Feature"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

plot_risk_manual_intervention <- function(results_long) {
  fig_data <- results_long %>%
    dplyr::filter(
      scenario == "academic_disengagement",
      metric == "Accuracy",
      missing_aggressiveness == 0.70,
      model_type %in% c("Baseline", "PMM_Only", "Delta_Dir_Correct", "Delta_Dir_Wrong")
    ) %>%
    dplyr::mutate(
      model_type = factor(
        model_type,
        levels = c("Baseline", "Delta_Dir_Correct", "PMM_Only", "Delta_Dir_Wrong"),
        labels = c("Baseline", "Delta Correct", "PMM Only", "Delta Incorrect")
      )
    )
  
  ggplot2::ggplot(fig_data, ggplot2::aes(x = model_type, y = value, fill = model_type)) +
    ggplot2::geom_col(width = 0.6, alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(value, accuracy = 0.1)),
                       vjust = -0.5, size = 5, fontface = "bold") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.85)) +
    ggplot2::labs(
      title = "The Risk of Manual Intervention",
      subtitle = "Accuracy at 70% Missingness (Multivariate High Impact)",
      x = "",
      y = "Accuracy"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
                   panel.grid.major.x = ggplot2::element_blank())
}

save_fig <- function(plot_obj, path, width = 12, height = 7, dpi = 300) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(path, plot = plot_obj, width = width, height = height, dpi = dpi)
  path
}

run_paired_tests <- function(all_results, prop = 0.40, scenario = "final") {
  df <- all_results %>% dplyr::filter(missing_aggressiveness == prop, scenario == scenario)
  
  t1 <- stats::t.test(df$auc_baseline, df$auc_pmm, paired = TRUE, alternative = "greater")
  t2 <- stats::t.test(df$auc_pmm_flag, df$auc_pmm, paired = TRUE, alternative = "greater")
  
  list(
    baseline_vs_pmm = t1,
    pmmflag_vs_pmm  = t2
  )
}

build_and_save_all_figures <- function(results_long, out_dir = "figures") {
  paths <- list()
  
  paths$auc_facet  <- save_fig(plot_metric_facet(results_long, "AUC"),
                               file.path(out_dir, "fig_auc_facet.png"), width = 14, height = 8)
  paths$acc_facet  <- save_fig(plot_metric_facet(results_long, "Accuracy"),
                               file.path(out_dir, "fig_acc_facet.png"), width = 14, height = 8)
  paths$misc_facet <- save_fig(plot_metric_facet(results_long, "Misclassification"),
                               file.path(out_dir, "fig_misc_facet.png"), width = 14, height = 8)
  
  paths$fig_high_impact <- save_fig(plot_high_impact_accuracy(results_long),
                                    file.path(out_dir, "fig_high_impact_accuracy.png"), width = 11, height = 7)
  paths$fig_degradation <- save_fig(plot_degradation_by_feature_importance(results_long),
                                    file.path(out_dir, "fig_degradation_feature_importance.png"), width = 11, height = 7)
  paths$fig_risk <- save_fig(plot_risk_manual_intervention(results_long),
                             file.path(out_dir, "fig_risk_manual_intervention.png"), width = 10, height = 7)
  
  paths
}