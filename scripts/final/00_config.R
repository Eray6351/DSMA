# ================================================================
# 00_config.R
# Final model-comparison pipeline configuration
# ================================================================

# This pipeline predicts binary customer satisfaction in Yelp
# restaurant reviews and compares several machine-learning models.

set.seed(123)

# FAST_MODE = TRUE:
# - small technical test run
# - verifies that the complete pipeline works
# - not used as final paper results
#
# FAST_MODE = FALSE:
# - normal final analysis mode
# - uses a statistically defensible stratified random sample
# - used for the final model comparison in the paper
FAST_MODE <- FALSE

FAST_TRAIN_SAMPLE_N <- 8000L
FAST_TEST_SAMPLE_N <- 2000L

NORMAL_TRAIN_SAMPLE_N <- 50000L
NORMAL_TEST_SAMPLE_N <- 10000L

SAMPLE_SEED <- 1234L

TRAIN_SHARE <- 0.80
CLASS_THRESHOLD <- 0.50

CORE_PACKAGES <- c(
  "data.table",
  "dplyr",
  "stringr",
  "lubridate",
  "ggplot2",
  "pROC",
  "tibble",
  "openxlsx"
)

OPTIONAL_MODEL_PACKAGES <- c(
  "class",
  "e1071",
  "rpart",
  "ranger",
  "gbm",
  "nnet"
)

PACKAGE_LIST <- unique(c(CORE_PACKAGES, OPTIONAL_MODEL_PACKAGES))

find_project_root <- function() {
  candidates <- unique(normalizePath(
    c(
      getwd(),
      file.path(getwd(), ".."),
      file.path(getwd(), "../.."),
      file.path(getwd(), "../../..")
    ),
    winslash = "/",
    mustWork = FALSE
  ))

  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "reviews_final.csv")) &&
        file.exists(file.path(candidate, "weather_philly_daily.csv"))
    ) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }

  stop("Project root not found. Run from the project folder or scripts/final/.")
}

PROJECT_ROOT <- find_project_root()

INPUT_REVIEWS_PATH <- file.path(PROJECT_ROOT, "reviews_final.csv")
INPUT_WEATHER_PATH <- file.path(PROJECT_ROOT, "weather_philly_daily.csv")

OUTPUT_DIR <- file.path(PROJECT_ROOT, "outputs", "final_model_comparison")
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

LOG_PATH <- file.path(OUTPUT_DIR, "final_model_comparison_log.txt")

PREPARED_DATASET_PATH <- file.path(OUTPUT_DIR, "prepared_dataset.rds")
FINAL_MODEL_DATASET_PATH <- file.path(OUTPUT_DIR, "final_model_dataset.rds")
FEATURE_DICTIONARY_PATH <- file.path(OUTPUT_DIR, "final_feature_dictionary.csv")
METRICS_CSV_PATH <- file.path(OUTPUT_DIR, "final_model_comparison_metrics.csv")
METRICS_XLSX_PATH <- file.path(OUTPUT_DIR, "final_model_comparison_metrics.xlsx")
FAILURES_CSV_PATH <- file.path(OUTPUT_DIR, "final_model_failures.csv")
ROC_AUC_PLOT_PATH <- file.path(OUTPUT_DIR, "final_model_comparison_plot_roc_auc.png")
BALANCED_ACCURACY_PLOT_PATH <- file.path(OUTPUT_DIR, "final_model_comparison_plot_balanced_accuracy.png")

load_core_packages <- function() {
  missing_packages <- CORE_PACKAGES[
    !vapply(CORE_PACKAGES, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Please install them before running the pipeline."
    )
  }

  suppressPackageStartupMessages({
    library(data.table)
    library(dplyr)
    library(stringr)
    library(lubridate)
    library(ggplot2)
    library(pROC)
    library(tibble)
    library(openxlsx)
  })
}

clean_names <- function(x) {
  x <- trimws(tolower(x))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  make.unique(x, sep = "_")
}

safe_name <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("&", "and", x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "missing")
}

log_message <- function(...) {
  line <- paste0(
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    " | ",
    paste(..., collapse = "")
  )

  cat(line, "\n")
  write(line, file = LOG_PATH, append = TRUE)
}

load_core_packages()
