# R/01_data_preparation.R

run_data_preparation <- function(
    csv_path,
    fig_corr_path = "figures/fig1_heterogeneous_corr.png",
    fig_dist_path = NULL  
) {
  # --- Load raw data ---
  raw <- read.csv(csv_path, check.names = FALSE, na.strings = c("", "NA", "N/A"))
  
  # --- Create analysis dataset with selected/renamed variables ---
  data <- raw %>%
    dplyr::rename(
      id            = Student_ID,
      age           = Age,
      internet      = Internet_Access_at_Home,
      education     = Parent_Education_Level,
      income        = Family_Income_Level,
      study         = Study_Hours_per_Week,
      stress        = `Stress_Level (1-10)`,
      sleep         = Sleep_Hours_per_Night,
      participation = Participation_Score,
      midterm       = Midterm_Score,
      final         = Final_Score,
      grade         = Grade
    ) %>%
    dplyr::select(id, age, internet, education, income, study,
                  stress, sleep, participation, midterm, final, grade)
  
  # --- Build data for heterogeneous correlation plot (Figure 1) ---
  demo_grade_vars <- c(
    "Gender",
    "Age",
    "Department",
    "Internet_Access_at_Home",
    "Parent_Education_Level",
    "Family_Income_Level",
    "Final_Score",
    "Midterm_Score",
    "Participation_Score",
    "Grade",
    "Projects_Score",
    "Assignments_Avg",
    "Quizzes_Avg"
  )
  
  demo_grade_df <- raw %>%
    dplyr::select(all_of(demo_grade_vars)) %>%
    dplyr::mutate(
      Grade = factor(Grade, levels = c("F","D","C","B","A"), ordered = TRUE),
      Department = factor(Department),
      Internet_Access_at_Home = factor(Internet_Access_at_Home, levels = c("No","Yes")),
      Parent_Education_Level = factor(
        Parent_Education_Level,
        levels = c("None","High School","Bachelor's","Master's","PhD"),
        ordered = TRUE
      ),
      Family_Income_Level = factor(Family_Income_Level, levels = c("Low","Medium","High"), ordered = TRUE),
      Gender = factor(Gender),
      Age = as.numeric(Age),
      
      Final_Score = as.numeric(Final_Score),
      Midterm_Score = as.numeric(Midterm_Score),
      Participation_Score = as.numeric(Participation_Score),
      Projects_Score = as.numeric(Projects_Score),
      Assignments_Avg = as.numeric(Assignments_Avg),
      Quizzes_Avg = as.numeric(Quizzes_Avg),
      
      Passing = ifelse(Grade %in% c("A","B","C"), 1L, 0L)
    ) %>%
    dplyr::select(-Grade) %>%
    tidyr::drop_na()
  
  hc <- polycor::hetcor(demo_grade_df, std.err = FALSE)
  cor_mat <- as.matrix(hc$correlations)
  
  # ensure PD for stable plotting
  cor_mat <- as.matrix(Matrix::nearPD(cor_mat)$mat)
  cor_mat[lower.tri(cor_mat, diag = TRUE)] <- NA
  
  cor_long <- reshape2::melt(cor_mat, na.rm = TRUE)
  
  p_corr <- ggplot2::ggplot(cor_long, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)), size = 3, color = "black") +
    ggplot2::scale_fill_viridis_c(
      name = "Correlation",
      limits = c(-1, 1),
      na.value = "transparent"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      legend.position = "right"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = "Figure 1: Heterogeneous correlation matrix",
      subtitle = "Academic, Behavioral, and Sociodemographic variables vs. Passing"
    )
  
  # Create output directory if needed
  dir.create(dirname(fig_corr_path), showWarnings = FALSE, recursive = TRUE)
  
  ggplot2::ggsave(
    filename = fig_corr_path,
    plot = p_corr,
    width = 9,
    height = 7,
    dpi = 300
  )
  
  # --- Optional: distribution grid plot ---
  p_dist <- NULL
  if (!is.null(fig_dist_path)) {
    vars_to_plot <- setdiff(names(data), c("id", "grade"))
    num_vars <- vars_to_plot[sapply(data[vars_to_plot], is.numeric)]
    cat_vars <- vars_to_plot[!sapply(data[vars_to_plot], is.numeric)]
    
    plot_numeric <- function(df, var) {
      ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
        ggplot2::geom_histogram(bins = 30, fill = "#0072B2", color = "white", alpha = 0.8) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::labs(title = var, x = NULL, y = "Count") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
          axis.text.x = ggplot2::element_text(size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          axis.title.y = ggplot2::element_text(size = 8)
        )
    }
    
    plot_categorical <- function(df, var) {
      ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
        ggplot2::geom_bar(fill = "#E69F00", color = "white", alpha = 0.8) +
        ggplot2::theme_minimal(base_size = 10) +
        ggplot2::labs(title = var, x = NULL, y = "Count") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 9, face = "bold", hjust = 0.5),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = ggplot2::element_text(size = 8),
          axis.title.y = ggplot2::element_text(size = 8)
        )
    }
    
    num_plots <- lapply(num_vars, function(v) plot_numeric(data, v))
    cat_plots <- lapply(cat_vars, function(v) plot_categorical(data, v))
    all_plots <- c(num_plots, cat_plots)
    
    p_dist <- patchwork::wrap_plots(all_plots, ncol = 4) +
      patchwork::plot_annotation(
        title = "Distributions of All Predictors",
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(size = 13, face = "bold", hjust = 0.5)
        )
      )
    
    dir.create(dirname(fig_dist_path), showWarnings = FALSE, recursive = TRUE)
    ggplot2::ggsave(fig_dist_path, plot = p_dist, width = 12, height = 8, dpi = 300)
  }
  
  # Return objects needed later
  list(
    raw = raw,
    data = data,
    fig_corr_path = fig_corr_path,
    fig_dist_path = fig_dist_path
  )
}