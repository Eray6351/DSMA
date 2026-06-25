# ================================================================
# run_all.R
# Run the full final model-comparison pipeline
# ================================================================

get_script_dir <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)

  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)))
  }

  candidate <- file.path(getwd(), "scripts", "final")
  if (dir.exists(candidate)) {
    return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

SCRIPT_DIR <- get_script_dir()

source(file.path(SCRIPT_DIR, "00_config.R"))

generated_output_files <- c(
  METRICS_CSV_PATH,
  METRICS_XLSX_PATH,
  ROC_AUC_PLOT_PATH,
  BALANCED_ACCURACY_PLOT_PATH,
  FAILURES_CSV_PATH,
  LOG_PATH,
  PREPARED_DATASET_PATH,
  FINAL_MODEL_DATASET_PATH,
  FEATURE_DICTIONARY_PATH
)

invisible(suppressWarnings(unlink(generated_output_files, force = TRUE)))

log_message("run_all.R started.")
log_message("FAST_MODE: ", FAST_MODE)
log_message("Project root: ", PROJECT_ROOT)
log_message("Output directory: ", OUTPUT_DIR)

source(file.path(SCRIPT_DIR, "01_prepare_dataset.R"))
source(file.path(SCRIPT_DIR, "02_feature_engineering.R"))
source(file.path(SCRIPT_DIR, "03_model_functions.R"))
source(file.path(SCRIPT_DIR, "04_run_model_comparison.R"))
source(file.path(SCRIPT_DIR, "05_create_plots.R"))

log_message("run_all.R completed.")

cat("\nrun_all.R completed.\n")
cat("Pipeline completed.\n")
cat("Output folder:", OUTPUT_DIR, "\n\n")

if (exists("PIPELINE_RUN_SUMMARY")) {
  print_distribution <- function(title, distribution_table) {
    cat(title, "\n")
    print(distribution_table)
    cat("\n")
  }

  cat("Final run summary\n")
  cat("-----------------\n")
  cat("FAST_MODE:", PIPELINE_RUN_SUMMARY$fast_mode, "\n")
  cat("Total final model rows before sampling:", PIPELINE_RUN_SUMMARY$total_final_model_rows_before_sampling, "\n")
  cat("Train rows before sampling:", PIPELINE_RUN_SUMMARY$train_rows_before_sampling, "\n")
  cat("Test rows before sampling:", PIPELINE_RUN_SUMMARY$test_rows_before_sampling, "\n")
  cat("Sampled train rows used:", PIPELINE_RUN_SUMMARY$sampled_train_rows_used, "\n")
  cat("Sampled test rows used:", PIPELINE_RUN_SUMMARY$sampled_test_rows_used, "\n")
  print_distribution("Class distribution in full data:", PIPELINE_RUN_SUMMARY$full_class_distribution)
  print_distribution("Class distribution in sampled train data:", PIPELINE_RUN_SUMMARY$sampled_train_class_distribution)
  print_distribution("Class distribution in sampled test data:", PIPELINE_RUN_SUMMARY$sampled_test_class_distribution)
  cat("Number of successful model runs:", PIPELINE_RUN_SUMMARY$successful_model_runs, "\n")
  cat("Number of failed model runs:", PIPELINE_RUN_SUMMARY$failed_model_runs, "\n")
}
