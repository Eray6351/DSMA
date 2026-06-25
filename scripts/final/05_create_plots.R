# ================================================================
# 05_create_plots.R
# Professor-style model comparison plots
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("05_create_plots.R started.")

metrics <- data.table::fread(METRICS_CSV_PATH)

failed_or_skipped <- metrics[status %in% c("failed", "skipped")]
if (nrow(failed_or_skipped) > 0) {
  log_message(
    "Plot excludes failed/skipped model rows. See final_model_failures.csv. Excluded rows: ",
    nrow(failed_or_skipped)
  )
}

plot_metrics <- metrics[
  status == "success" &
    model_name != "Unknown model" &
    is.finite(ROC_AUC) &
    is.finite(Balanced_Accuracy)
]

if (nrow(plot_metrics) == 0) {
  stop("No successful model results available for plotting.")
}

plot_metrics[, model_name := factor(model_name, levels = unique(model_name))]
plot_metrics[, feature_set := factor(feature_set, levels = c("Without Weather", "With Weather"))]

mode_label <- if (FAST_MODE) {
  "FAST_MODE technical test run"
} else {
  "Normal final stratified-sample analysis"
}

plot_theme <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.x = ggplot2::element_text(angle = 35, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = ggplot2::element_blank()
  )

roc_auc_plot <- ggplot2::ggplot(
  plot_metrics,
  ggplot2::aes(x = model_name, y = ROC_AUC, fill = feature_set)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.68) +
  ggplot2::scale_fill_manual(values = c("Without Weather" = "#4C78A8", "With Weather" = "#F58518")) +
  ggplot2::labs(
    title = "Final Model Comparison: ROC-AUC",
    subtitle = paste("Binary restaurant satisfaction prediction -", mode_label),
    x = NULL,
    y = "ROC-AUC",
    fill = "Feature set"
  ) +
  plot_theme

balanced_accuracy_plot <- ggplot2::ggplot(
  plot_metrics,
  ggplot2::aes(x = model_name, y = Balanced_Accuracy, fill = feature_set)
) +
  ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.75), width = 0.68) +
  ggplot2::scale_fill_manual(values = c("Without Weather" = "#4C78A8", "With Weather" = "#F58518")) +
  ggplot2::labs(
    title = "Final Model Comparison: Balanced Accuracy",
    subtitle = paste("Binary restaurant satisfaction prediction -", mode_label),
    x = NULL,
    y = "Balanced Accuracy",
    fill = "Feature set"
  ) +
  plot_theme

ggplot2::ggsave(ROC_AUC_PLOT_PATH, roc_auc_plot, width = 12, height = 7, dpi = 300)
ggplot2::ggsave(BALANCED_ACCURACY_PLOT_PATH, balanced_accuracy_plot, width = 12, height = 7, dpi = 300)

log_message("Saved ROC-AUC plot to: ", ROC_AUC_PLOT_PATH)
log_message("Saved Balanced Accuracy plot to: ", BALANCED_ACCURACY_PLOT_PATH)
log_message("05_create_plots.R completed.")
