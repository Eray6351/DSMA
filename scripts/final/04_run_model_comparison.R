# ================================================================
# 04_run_model_comparison.R
# Fixed stratified-sample model comparison
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

source(file.path(PROJECT_ROOT, "scripts", "final", "03_model_functions.R"))

log_message("04_run_model_comparison.R started.")

model_dataset <- readRDS(FINAL_MODEL_DATASET_PATH)
data.table::setDT(model_dataset)

feature_dictionary <- data.table::fread(FEATURE_DICTIONARY_PATH)

model_dataset <- model_dataset[!is.na(satisfied)]
model_dataset[, satisfied := as.integer(as.character(satisfied))]

total_final_model_rows_before_sampling <- nrow(model_dataset)
full_class_distribution <- model_dataset[, .(rows = .N, share = .N / nrow(model_dataset)), by = satisfied][order(satisfied)]

stratified_sample_dt <- function(data, target_n, seed) {
  data.table::setDT(data)

  if (nrow(data) <= target_n) {
    return(data.table::copy(data))
  }

  data <- data.table::copy(data)
  data[, sample_id_internal := .I]

  class_counts <- data[, .N, by = satisfied]
  class_counts[, raw_n := as.numeric(target_n) * as.numeric(N) / as.numeric(sum(N))]
  class_counts[, sample_n := floor(raw_n)]

  remainder <- as.integer(target_n - sum(class_counts$sample_n))
  if (remainder > 0) {
    class_counts[, fractional_part := raw_n - sample_n]
    data.table::setorder(class_counts, -fractional_part)
    class_counts[seq_len(remainder), sample_n := sample_n + 1L]
  }

  set.seed(seed)
  sampled_parts <- lapply(seq_len(nrow(class_counts)), function(i) {
    current_class <- class_counts$satisfied[i]
    current_n <- as.integer(min(class_counts$sample_n[i], class_counts$N[i]))
    data[satisfied == current_class][sample(.N, current_n)]
  })

  sampled <- data.table::rbindlist(sampled_parts, use.names = TRUE, fill = TRUE)

  if (nrow(sampled) < target_n) {
    remaining <- data[!(sample_id_internal %in% sampled$sample_id_internal)]
    fill_n <- min(nrow(remaining), target_n - nrow(sampled))
    if (fill_n > 0) {
      sampled <- data.table::rbindlist(
        list(sampled, remaining[sample(.N, fill_n)]),
        use.names = TRUE,
        fill = TRUE
      )
    }
  }

  if (nrow(sampled) > target_n) {
    sampled <- sampled[sample(.N, target_n)]
  }

  sampled[, sample_id_internal := NULL]
  sampled
}

base_features <- feature_dictionary[use_in_base == TRUE, feature]
base_features <- intersect(base_features, names(model_dataset))
base_features <- setdiff(base_features, c("stars", "satisfied"))

weather_features <- unique(c(base_features, intersect(c("PRCP", "TMAX", "TMIN"), names(model_dataset))))
weather_features <- setdiff(weather_features, c("stars", "satisfied"))

set.seed(SAMPLE_SEED)
train_ids <- sample(seq_len(nrow(model_dataset)), size = floor(TRAIN_SHARE * nrow(model_dataset)))

train_full <- model_dataset[train_ids]
test_full <- model_dataset[-train_ids]

train_rows_before_sampling <- nrow(train_full)
test_rows_before_sampling <- nrow(test_full)

if (FAST_MODE) {
  train_sample_n <- FAST_TRAIN_SAMPLE_N
  test_sample_n <- FAST_TEST_SAMPLE_N
  sample_type <- "fast stratified sample"
} else {
  train_sample_n <- NORMAL_TRAIN_SAMPLE_N
  test_sample_n <- NORMAL_TEST_SAMPLE_N
  sample_type <- "normal stratified sample"
}

train_data <- stratified_sample_dt(
  train_full,
  target_n = min(train_sample_n, nrow(train_full)),
  seed = SAMPLE_SEED + 1L
)

test_data <- stratified_sample_dt(
  test_full,
  target_n = min(test_sample_n, nrow(test_full)),
  seed = SAMPLE_SEED + 2L
)

sampled_train_class_distribution <- train_data[, .(rows = .N, share = .N / nrow(train_data)), by = satisfied][order(satisfied)]
sampled_test_class_distribution <- test_data[, .(rows = .N, share = .N / nrow(test_data)), by = satisfied][order(satisfied)]

log_message("FAST_MODE: ", FAST_MODE)
log_message("Sample type: ", sample_type)
log_message("Total final model rows before sampling: ", total_final_model_rows_before_sampling)
log_message("Train rows before sampling: ", train_rows_before_sampling)
log_message("Test rows before sampling: ", test_rows_before_sampling)
log_message("Sampled train rows used: ", nrow(train_data))
log_message("Sampled test rows used: ", nrow(test_data))
log_message("Base feature count: ", length(base_features))
log_message("Weather feature count: ", length(weather_features))
log_message("Full class distribution: ", paste(capture.output(print(full_class_distribution)), collapse = " | "))
log_message("Sampled train class distribution: ", paste(capture.output(print(sampled_train_class_distribution)), collapse = " | "))
log_message("Sampled test class distribution: ", paste(capture.output(print(sampled_test_class_distribution)), collapse = " | "))

feature_sets <- list(
  "Without Weather" = base_features,
  "With Weather" = weather_features
)

all_results <- list()
result_counter <- 1L

for (feature_set_name in names(feature_sets)) {
  current_features <- feature_sets[[feature_set_name]]

  log_message("Running feature set: ", feature_set_name)

  for (model_spec in MODEL_SPECS) {
    model_name <- model_spec$model_name
    model_function <- model_spec$fit

    result <- tryCatch(
      model_function(
        train_data,
        test_data,
        current_features,
        feature_set_name,
        sample_type
      ),
      error = function(e) {
        tibble::tibble(
          model_name = model_name,
          feature_set = feature_set_name,
          sample_type = sample_type,
          train_rows_used = nrow(train_data),
          test_rows_used = nrow(test_data),
          Accuracy = NA_real_,
          Sensitivity = NA_real_,
          Specificity = NA_real_,
          Balanced_Accuracy = NA_real_,
          ROC_AUC = NA_real_,
          runtime_seconds = NA_real_,
          status = "failed",
          notes = paste("Unexpected failure:", e$message)
        )
      }
    )

    all_results[[result_counter]] <- result
    result_counter <- result_counter + 1L

    log_message(
      "Finished: ",
      result$model_name[1],
      " | ",
      feature_set_name,
      " | sample_type: ",
      result$sample_type[1],
      " | status: ",
      result$status[1],
      " | notes: ",
      result$notes[1]
    )
  }
}

final_metrics <- dplyr::bind_rows(all_results)
final_failures <- final_metrics %>%
  dplyr::filter(status %in% c("failed", "skipped"))

successful_model_runs <- sum(final_metrics$status == "success", na.rm = TRUE)
failed_model_runs <- sum(final_metrics$status == "failed", na.rm = TRUE)

data.table::fwrite(final_metrics, METRICS_CSV_PATH)
data.table::fwrite(final_failures, FAILURES_CSV_PATH)

if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(
    list(
      metrics = final_metrics,
      failures_and_skips = final_failures
    ),
    METRICS_XLSX_PATH,
    overwrite = TRUE
  )
  log_message("Saved metrics Excel file to: ", METRICS_XLSX_PATH)
} else {
  log_message("WARNING: openxlsx unavailable. Metrics Excel file was not created.")
}

PIPELINE_RUN_SUMMARY <- list(
  fast_mode = FAST_MODE,
  total_final_model_rows_before_sampling = total_final_model_rows_before_sampling,
  train_rows_before_sampling = train_rows_before_sampling,
  test_rows_before_sampling = test_rows_before_sampling,
  sampled_train_rows_used = nrow(train_data),
  sampled_test_rows_used = nrow(test_data),
  full_class_distribution = full_class_distribution,
  sampled_train_class_distribution = sampled_train_class_distribution,
  sampled_test_class_distribution = sampled_test_class_distribution,
  successful_model_runs = successful_model_runs,
  failed_model_runs = failed_model_runs
)

log_message("Saved metrics CSV file to: ", METRICS_CSV_PATH)
log_message("Saved failed/skipped model file to: ", FAILURES_CSV_PATH)
log_message("Successful model runs: ", successful_model_runs)
log_message("Failed model runs: ", failed_model_runs)
log_message("04_run_model_comparison.R completed.")
