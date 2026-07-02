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
    library(ranger)
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


# ================================================================
# 01_prepare_dataset.R
# Read raw inputs, create target, and match weather data
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("01_prepare_dataset.R started.")

reviews <- data.table::fread(INPUT_REVIEWS_PATH, showProgress = TRUE)
data.table::setnames(reviews, clean_names(names(reviews)))

weather <- data.table::fread(INPUT_WEATHER_PATH, showProgress = FALSE)
data.table::setnames(weather, clean_names(names(weather)))

if (!"review_date" %in% names(reviews)) {
  if ("date" %in% names(reviews)) {
    data.table::setnames(reviews, "date", "review_date")
  } else {
    stop("reviews_final.csv must contain review_date or date.")
  }
}

if (!"date" %in% names(weather)) {
  stop("weather_philly_daily.csv must contain DATE/date.")
}

reviews[, review_date := as.IDate(review_date)]
weather[, date := as.IDate(date)]

if (!"row_id" %in% names(reviews)) {
  reviews[, row_id := .I]
}

if (!"satisfied" %in% names(reviews)) {
  if (!"stars" %in% names(reviews)) {
    stop("Cannot create satisfied because stars is missing.")
  }
  reviews[, satisfied := as.integer(as.numeric(stars) > 3.5)]
  log_message("Created satisfied from stars: satisfied = 1 if stars > 3.5.")
} else {
  reviews[, satisfied := as.integer(satisfied)]
  log_message("Using existing satisfied column.")
}

if ("stars" %in% names(reviews)) {
  reviews[, stars := suppressWarnings(as.numeric(stars))]
}

weather_keep <- intersect(
  c("date", "prcp", "snow", "snwd", "tmax", "tmin", "tobs"),
  names(weather)
)
weather <- weather[, ..weather_keep]

weather_rename <- intersect(c("prcp", "snow", "snwd", "tmax", "tmin", "tobs"), names(weather))
data.table::setnames(weather, weather_rename, toupper(weather_rename))

for (weather_var in intersect(c("PRCP", "SNOW", "SNWD", "TMAX", "TMIN", "TOBS"), names(weather))) {
  weather[, (weather_var) := suppressWarnings(as.numeric(get(weather_var)))]
}

prepared_dataset <- merge(
  reviews,
  weather,
  by.x = "review_date",
  by.y = "date",
  all.x = TRUE,
  sort = FALSE
)

data.table::setorder(prepared_dataset, row_id)

log_message("Rows in reviews_final.csv: ", nrow(reviews))
log_message("Rows after weather merge: ", nrow(prepared_dataset))
log_message("Main weather specification uses PRCP, TMAX, and TMIN only.")
log_message("SNOW, SNWD, and TOBS are kept if available but are not used in the main model feature set.")
log_message("stars is kept for target/prior construction but must not be used as a predictor.")

saveRDS(prepared_dataset, PREPARED_DATASET_PATH)

log_message("Saved prepared dataset to: ", PREPARED_DATASET_PATH)
log_message("01_prepare_dataset.R completed.")



# ================================================================
# 02_feature_engineering.R
# Create modeling features without target leakage
# ================================================================
# FIX (vs. original): extract_attr_raw() now handles the double-escaped
# quote format (""Key"": ""Value"") produced when reviews_final.csv is
# read by data.table::fread(). The original regex matched only single-
# or double-quoted keys ("Key" / 'Key') and therefore returned NA for
# every attribute except Alcohol and WiFi, whose values happened to be
# caught by a secondary code path. All structured Yelp attributes are
# now correctly parsed.
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("02_feature_engineering.R started.")

# ── Load prepared dataset ────────────────────────────────────────────────────
prepared_dataset <- readRDS(PREPARED_DATASET_PATH)
data.table::setDT(prepared_dataset)

log_message("Available columns: ", paste(names(prepared_dataset), collapse = ", "))

# ── Feature dictionary ───────────────────────────────────────────────────────
feature_dictionary <- data.table::data.table(
  feature        = character(),
  feature_group  = character(),
  description    = character(),
  source         = character(),
  use_in_base    = logical(),
  use_in_weather = logical(),
  notes          = character()
)

register_feature <- function(feature, feature_group, description, source,
                             use_in_base = TRUE, notes = "") {
  if (feature %in% names(prepared_dataset)) {
    feature_dictionary <<- data.table::rbindlist(
      list(
        feature_dictionary,
        data.table::data.table(
          feature        = feature,
          feature_group  = feature_group,
          description    = description,
          source         = source,
          use_in_base    = use_in_base,
          use_in_weather = use_in_base,
          notes          = notes
        )
      ),
      fill = TRUE
    )
  }
}

# ── Helper: escape special regex characters in attribute key names ────────────
escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

# ── Helper: normalise extracted raw attribute values ─────────────────────────
normalize_attr_value <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "null", "None", "none")] <- NA_character_
  # Strip Python unicode prefix  u'value' -> value
  x <- gsub("^u'(.*)'$",  "\\1", x)
  x <- gsub("^u\"(.*)\"$", "\\1", x)
  # Strip surrounding single or double quotes
  x <- gsub("^'(.*)'$",   "\\1", x)
  x <- gsub("^\"(.*)\"$", "\\1", x)
  trimws(x)
}

# ── FIXED: extract_attr_raw() ─────────────────────────────────────────────────
# The attributes column in reviews_final.csv uses double-escaped quotes when
# read by fread(), so keys and values appear as:
#   {"\"Key\"": "\"Value\""}  ->  in R string: {""Key"": ""Value""}
#
# Three pattern families are tried in order:
#   1. Double-escaped quotes: ""Key"": ""Value""   <- the actual format
#   2. Single quotes:          'Key': 'Value'
#   3. Standard double quotes: "Key": "Value"
#
# Each pattern captures the value portion in group \1.
# The first pattern that produces a match for a given row wins.

extract_attr_raw <- function(attributes, key) {
  attributes  <- as.character(attributes)
  key_pattern <- escape_regex(key)
  
  patterns <- c(
    # 1. Double-escaped double quotes (""Key"": ""Value"") — primary fix
    paste0("\"\"", key_pattern, "\"\"\\s*:\\s*\"\"([^\"]*)\"\""),
    # 2. Single-quoted key and value ('Key': 'Value' or 'Key': 'u'value'')
    paste0("'", key_pattern, "'\\s*:\\s*'([^']*)'"),
    # 3. Standard double-quoted key and value ("Key": "Value")
    paste0("\"", key_pattern, "\"\\s*:\\s*\"([^\"]*)\""),
    # 4. Unquoted numeric value ("Key": 123 or ""Key"": 123)
    paste0("(?:\"|\"\"){1,2}", key_pattern, "(?:\"|\"\"){1,2}\\s*:\\s*([0-9]+(?:\\.[0-9]+)?)")
  )
  
  out <- rep(NA_character_, length(attributes))
  
  for (pattern in patterns) {
    still_missing <- is.na(out)
    if (!any(still_missing)) break
    
    hit     <- regexpr(pattern, attributes[still_missing], perl = TRUE)
    matched <- hit > 0 & !is.na(hit)
    
    if (any(matched)) {
      captured <- regmatches(
        attributes[still_missing],
        regexpr(pattern, attributes[still_missing], perl = TRUE)
      )
      # Extract capture group \1
      values <- sub(pattern, "\\1", captured, perl = TRUE)
      idx    <- which(still_missing)[matched]
      out[idx] <- values[matched]
    }
  }
  
  normalize_attr_value(out)
}

# ── Helper: boolean attribute (True/False/yes/no/free etc.) ──────────────────
attr_bool <- function(attributes, key) {
  value <- tolower(extract_attr_raw(attributes, key))
  out   <- rep(NA_real_, length(value))
  out[value %in% c("true",  "1", "yes", "free")] <- 1
  out[value %in% c("false", "0", "no",  "none")] <- 0
  out
}

# ── Helper: numeric attribute ────────────────────────────────────────────────
attr_numeric <- function(attributes, key) {
  suppressWarnings(as.numeric(extract_attr_raw(attributes, key)))
}

# ── Helper: row-wise mean ignoring NA ────────────────────────────────────────
row_mean_available <- function(dt, cols) {
  cols <- intersect(cols, names(dt))
  if (length(cols) == 0) return(rep(NA_real_, nrow(dt)))
  rowMeans(as.matrix(dt[, ..cols]), na.rm = TRUE)
}

# ── (Optional) Text-based proxies ────────────────────────────────────────────
text_column <- intersect(c("text", "review_text"), names(prepared_dataset))[1]

if (!is.na(text_column)) {
  log_message("Review text column found: ", text_column)
  
  review_text <- tolower(as.character(prepared_dataset[[text_column]]))
  review_text[is.na(review_text)] <- ""
  
  score_from_words <- function(words) {
    pattern <- paste0("\\b(", paste(words, collapse = "|"), ")\\b")
    as.numeric(stringr::str_count(review_text, pattern))
  }
  
  prepared_dataset[, food_quality_score := score_from_words(c(
    "food", "taste", "delicious", "fresh", "meal", "dish", "flavor",
    "burger", "pizza", "chicken", "coffee", "sushi", "steak"
  ))]
  prepared_dataset[, service_score := score_from_words(c(
    "service", "waiter", "waitress", "staff", "friendly", "rude",
    "slow", "fast", "manager", "server"
  ))]
  prepared_dataset[, ambience_score := score_from_words(c(
    "atmosphere", "ambience", "music", "noise", "loud", "quiet",
    "cozy", "clean", "dirty", "seating"
  ))]
  prepared_dataset[, price_score := score_from_words(c(
    "price", "cheap", "expensive", "value", "overpriced", "worth"
  ))]
  
  register_feature("food_quality_score", "text_proxy", "Food-quality word count proxy.",  text_column)
  register_feature("service_score",      "text_proxy", "Service word count proxy.",        text_column)
  register_feature("ambience_score",     "text_proxy", "Ambience word count proxy.",       text_column)
  register_feature("price_score",        "text_proxy", "Price/value word count proxy.",    text_column)
  
} else {
  log_message("WARNING: No text/review_text column found. Text-based proxies cannot be created.")
  log_message("WARNING: Food Quality cannot be directly measured without review text.")
  feature_dictionary <- data.table::rbindlist(
    list(
      feature_dictionary,
      data.table::data.table(
        feature        = "food_quality_score",
        feature_group  = "not_available",
        description    = paste0("Direct Food Quality proxy cannot be created because no ",
                                "text or review_text column is available."),
        source         = "reviews_final.csv",
        use_in_base    = FALSE,
        use_in_weather = FALSE,
        notes          = "Data limitation: Food Quality is not directly observed in the current dataset."
      )
    ),
    fill = TRUE
  )
}

# ── Structured Yelp attribute extraction ─────────────────────────────────────
if ("attributes" %in% names(prepared_dataset)) {
  log_message("Extracting structured Yelp attributes from attributes column.")
  
  # Price
  prepared_dataset[, attr_price_range := attr_numeric(attributes, "RestaurantsPriceRange2")]
  
  # Noise level: ordinal encoding  quiet=2, average=1, loud/very_loud=0
  noise_raw <- tolower(extract_attr_raw(prepared_dataset$attributes, "NoiseLevel"))
  prepared_dataset[, attr_noise_score := data.table::fcase(
    noise_raw %in% c("quiet"),                    2,
    noise_raw %in% c("average"),                  1,
    noise_raw %in% c("loud", "very_loud", "very loud"), 0,
    default = NA_real_
  )]
  
  # Atmosphere attributes
  prepared_dataset[, attr_outdoor_seating := attr_bool(attributes, "OutdoorSeating")]
  prepared_dataset[, attr_has_tv          := attr_bool(attributes, "HasTV")]
  
  # Alcohol: any non-none/false value -> present
  alcohol_raw <- tolower(extract_attr_raw(prepared_dataset$attributes, "Alcohol"))
  prepared_dataset[, attr_alcohol_present := as.numeric(
    !is.na(alcohol_raw) & !(alcohol_raw %in% c("none", "no", "false"))
  )]
  
  # WiFi: free or paid -> available
  wifi_raw <- tolower(extract_attr_raw(prepared_dataset$attributes, "WiFi"))
  prepared_dataset[, attr_wifi_available := as.numeric(
    wifi_raw %in% c("free", "paid", "yes", "true")
  )]
  
  # Service attributes
  prepared_dataset[, attr_takeout      := attr_bool(attributes, "RestaurantsTakeOut")]
  prepared_dataset[, attr_delivery     := attr_bool(attributes, "RestaurantsDelivery")]
  prepared_dataset[, attr_caters       := attr_bool(attributes, "Caters")]
  prepared_dataset[, attr_good_for_kids := attr_bool(attributes, "GoodForKids")]
  prepared_dataset[, attr_bike_parking := attr_bool(attributes, "BikeParking")]
  
  # Log extraction success rates
  attr_cols <- c(
    "attr_price_range", "attr_noise_score", "attr_outdoor_seating",
    "attr_has_tv", "attr_alcohol_present", "attr_wifi_available",
    "attr_takeout", "attr_delivery", "attr_caters",
    "attr_good_for_kids", "attr_bike_parking"
  )
  for (col in attr_cols) {
    n_non_na <- sum(!is.na(prepared_dataset[[col]]))
    log_message("  ", col, ": ", n_non_na, " non-NA values extracted.")
  }
  
  # Composite proxy scores (only created if text proxies are not already present)
  if (!"price_score" %in% names(prepared_dataset)) {
    prepared_dataset[, price_score := attr_price_range]
    register_feature("price_score", "structured_proxy",
                     "Price proxy from RestaurantsPriceRange2.", "attributes")
  }
  
  if (!"ambience_score" %in% names(prepared_dataset)) {
    ambience_cols <- c("attr_noise_score", "attr_outdoor_seating",
                       "attr_has_tv", "attr_alcohol_present", "attr_wifi_available")
    prepared_dataset[, ambience_score := row_mean_available(.SD, ambience_cols)]
    register_feature("ambience_score", "structured_proxy",
                     "Ambience proxy from NoiseLevel, OutdoorSeating, HasTV, Alcohol, and WiFi.",
                     "attributes")
  }
  
  if (!"service_score" %in% names(prepared_dataset)) {
    service_cols <- c("attr_takeout", "attr_delivery", "attr_caters",
                      "attr_good_for_kids", "attr_bike_parking")
    prepared_dataset[, service_score := row_mean_available(.SD, service_cols)]
    register_feature("service_score", "structured_proxy",
                     paste0("Service/convenience proxy from takeout, delivery, ",
                            "caters, kids, and bike parking attributes."),
                     "attributes")
  }
  
  # Register all individual structured attributes
  structured_controls <- c(
    "attr_price_range", "attr_noise_score", "attr_outdoor_seating",
    "attr_has_tv", "attr_alcohol_present", "attr_wifi_available",
    "attr_takeout", "attr_delivery", "attr_caters",
    "attr_good_for_kids", "attr_bike_parking"
  )
  for (control in structured_controls) {
    register_feature(control, "structured_attribute",
                     paste0("Structured Yelp attribute: ", control), "attributes")
  }
  
} else {
  log_message("WARNING: attributes column missing. Structured Yelp attribute proxies cannot be extracted.")
}

# ── Restaurant category dummies (top 15) ─────────────────────────────────────
if ("categories" %in% names(prepared_dataset)) {
  category_counter <- new.env(parent = emptyenv())
  category_values  <- prepared_dataset$categories
  category_values[is.na(category_values)] <- ""
  
  for (categories_string in category_values) {
    categories <- trimws(unlist(strsplit(categories_string, ",", fixed = TRUE)))
    categories <- categories[nzchar(categories)]
    for (category in categories) {
      if (tolower(category) == "restaurants") next
      current <- category_counter[[category]]
      if (is.null(current)) current <- 0L
      category_counter[[category]] <- current + 1L
    }
  }
  
  category_names  <- ls(category_counter)
  category_counts <- vapply(category_names, function(x) category_counter[[x]], integer(1))
  selected_categories <- names(sort(category_counts, decreasing = TRUE))[
    seq_len(min(15L, length(category_counts)))
  ]
  
  for (category in selected_categories) {
    feature_name <- paste0("cat_", safe_name(category))
    pattern      <- paste0("(^|,\\s*)", escape_regex(category), "(\\s*,|$)")
    prepared_dataset[, (feature_name) := as.integer(grepl(pattern, categories, perl = TRUE))]
    register_feature(feature_name, "category_control",
                     paste0("Frequent restaurant category: ", category), "categories")
  }
  
  log_message("Created category controls: ", paste(selected_categories, collapse = ", "))
}

# ── Prior business reputation features ───────────────────────────────────────
if (!all(c("business_id", "review_date", "stars") %in% names(prepared_dataset))) {
  stop("business_id, review_date, and stars are required for prior business features.")
}

prior_source <- prepared_dataset[
  !is.na(business_id) & !is.na(review_date) & !is.na(stars),
  .(business_id, review_date, stars)
]

daily_prior <- prior_source[
  ,
  .(date_review_count = .N,
    date_star_sum     = sum(stars, na.rm = TRUE)),
  by = .(business_id, review_date)
]

data.table::setorder(daily_prior, business_id, review_date)

daily_prior[
  ,
  business_review_count_before := shift(cumsum(date_review_count), 1L, fill = 0L),
  by = business_id
]
daily_prior[
  ,
  prior_star_sum_before := shift(cumsum(date_star_sum), 1L, fill = 0),
  by = business_id
]
daily_prior[
  ,
  prior_business_avg_stars := data.table::fifelse(
    business_review_count_before > 0,
    prior_star_sum_before / business_review_count_before,
    NA_real_
  )
]

prepared_dataset <- merge(
  prepared_dataset,
  daily_prior[, .(business_id, review_date,
                  prior_business_avg_stars, business_review_count_before)],
  by      = c("business_id", "review_date"),
  all.x   = TRUE,
  sort    = FALSE
)

data.table::setorder(prepared_dataset, row_id)
prepared_dataset[is.na(business_review_count_before), business_review_count_before := 0]

register_feature("prior_business_avg_stars",  "prior_reputation",
                 "Average stars from earlier reviews of the same business only.",
                 "reviews_final.csv")
register_feature("business_review_count_before", "prior_reputation",
                 "Number of earlier reviews of the same business.",
                 "reviews_final.csv")

# ── Weather features (registered but excluded from base feature set) ──────────
weather_features <- intersect(c("PRCP", "TMAX", "TMIN"), names(prepared_dataset))

for (weather_feature in weather_features) {
  feature_dictionary <- data.table::rbindlist(
    list(
      feature_dictionary,
      data.table::data.table(
        feature        = weather_feature,
        feature_group  = "weather",
        description    = paste0("External weather variable: ", weather_feature),
        source         = "weather_philly_daily.csv",
        use_in_base    = FALSE,
        use_in_weather = TRUE,
        notes          = "Main weather specification uses PRCP, TMAX, and TMIN."
      )
    ),
    fill = TRUE
  )
}

# ── Finalise feature sets ─────────────────────────────────────────────────────
base_features <- feature_dictionary[use_in_base == TRUE, unique(feature)]
base_features <- setdiff(base_features, c("stars", "satisfied"))
base_features <- intersect(base_features, names(prepared_dataset))

weather_feature_set <- unique(c(base_features, weather_features))

id_columns <- intersect(
  c("row_id", "review_id", "business_id", "review_date",
    "satisfied", "stars", "categories", "attributes"),
  names(prepared_dataset)
)

keep_columns <- unique(c(
  id_columns,
  base_features,
  weather_features,
  intersect(c("SNOW", "SNWD", "TOBS"), names(prepared_dataset))
))

final_model_dataset <- prepared_dataset[, ..keep_columns]
final_model_dataset <- final_model_dataset[!is.na(satisfied)]

attr(final_model_dataset, "base_features")    <- base_features
attr(final_model_dataset, "weather_features") <- weather_feature_set

feature_dictionary[, use_in_weather := feature %in% weather_feature_set]
feature_dictionary <- unique(feature_dictionary, by = "feature")

# ── Save outputs ──────────────────────────────────────────────────────────────
saveRDS(final_model_dataset, FINAL_MODEL_DATASET_PATH)
data.table::fwrite(feature_dictionary, FEATURE_DICTIONARY_PATH)

log_message("Final model rows: ",    nrow(final_model_dataset))
log_message("Base features: ",       paste(base_features, collapse = ", "))
log_message("Weather features: ",    paste(weather_feature_set, collapse = ", "))
log_message("Saved final model dataset to: ", FINAL_MODEL_DATASET_PATH)
log_message("Saved feature dictionary to: ",  FEATURE_DICTIONARY_PATH)
log_message("02_feature_engineering.R completed.")



# ================================================================
# 03_model_functions.R
# Reusable model fitting and evaluation functions
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

# Robust column selector.
# This avoids the data.table error where DT[, features] is interpreted
# as a literal column named "features".
select_feature_frame <- function(data, features) {
  data <- as.data.frame(data)
  features <- setdiff(features, c("satisfied", "stars"))
  missing_features <- setdiff(features, names(data))
  
  if (length(missing_features) > 0) {
    stop("Missing feature columns: ", paste(missing_features, collapse = ", "))
  }
  
  data[, features, drop = FALSE]
}

metric_row <- function(model_name, feature_set, accuracy, sensitivity, specificity,
                       balanced_accuracy, roc_auc, runtime_seconds,
                       status = "success", notes) {
  tibble::tibble(
    model_name = model_name,
    feature_set = feature_set,
    Accuracy = as.numeric(accuracy),
    Sensitivity = as.numeric(sensitivity),
    Specificity = as.numeric(specificity),
    Balanced_Accuracy = as.numeric(balanced_accuracy),
    ROC_AUC = as.numeric(roc_auc),
    runtime_seconds = as.numeric(runtime_seconds),
    status = as.character(status),
    notes = as.character(notes)
  )
}

skipped_model <- function(model_name, feature_set, start_time, notes) {
  metric_row(
    model_name = model_name,
    feature_set = feature_set,
    accuracy = NA_real_,
    sensitivity = NA_real_,
    specificity = NA_real_,
    balanced_accuracy = NA_real_,
    roc_auc = NA_real_,
    runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    status = "skipped",
    notes = notes
  )
}

failed_model <- function(model_name, feature_set, start_time, notes) {
  metric_row(
    model_name = model_name,
    feature_set = feature_set,
    accuracy = NA_real_,
    sensitivity = NA_real_,
    specificity = NA_real_,
    balanced_accuracy = NA_real_,
    roc_auc = NA_real_,
    runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    status = "failed",
    notes = notes
  )
}

evaluate_binary_classifier <- function(model_name, feature_set, actual, probability,
                                       runtime_seconds, notes = "ok") {
  actual <- as.integer(as.character(actual))
  probability <- as.numeric(probability)
  probability[!is.finite(probability)] <- NA_real_
  probability <- pmin(pmax(probability, 0), 1)
  
  predicted <- ifelse(probability >= CLASS_THRESHOLD, 1L, 0L)
  
  tp <- sum(actual == 1 & predicted == 1, na.rm = TRUE)
  tn <- sum(actual == 0 & predicted == 0, na.rm = TRUE)
  fp <- sum(actual == 0 & predicted == 1, na.rm = TRUE)
  fn <- sum(actual == 1 & predicted == 0, na.rm = TRUE)
  
  accuracy <- mean(predicted == actual, na.rm = TRUE)
  sensitivity <- ifelse((tp + fn) > 0, tp / (tp + fn), NA_real_)
  specificity <- ifelse((tn + fp) > 0, tn / (tn + fp), NA_real_)
  balanced_accuracy <- mean(c(sensitivity, specificity), na.rm = TRUE)
  
  roc_auc <- NA_real_
  if (requireNamespace("pROC", quietly = TRUE) && length(unique(actual[!is.na(actual)])) == 2) {
    roc_auc <- tryCatch(
      as.numeric(pROC::auc(pROC::roc(actual, probability, quiet = TRUE, direction = "<"))),
      error = function(e) NA_real_
    )
  }
  
  metric_row(
    model_name = model_name,
    feature_set = feature_set,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = balanced_accuracy,
    roc_auc = roc_auc,
    runtime_seconds = runtime_seconds,
    status = "success",
    notes = notes
  )
}

prepare_numeric_features <- function(train_data, test_data, features) {
  features <- intersect(features, names(train_data))
  features <- setdiff(features, c("satisfied", "stars"))
  
  if (length(features) == 0) {
    return(list(train_x = NULL, test_x = NULL, features = character()))
  }
  
  train_x <- select_feature_frame(train_data, features)
  test_x <- select_feature_frame(test_data, features)
  
  for (feature in features) {
    train_x[[feature]] <- suppressWarnings(as.numeric(train_x[[feature]]))
    test_x[[feature]] <- suppressWarnings(as.numeric(test_x[[feature]]))
    
    impute_value <- median(train_x[[feature]], na.rm = TRUE)
    if (!is.finite(impute_value) || is.na(impute_value)) {
      impute_value <- 0
    }
    
    train_x[[feature]][is.na(train_x[[feature]])] <- impute_value
    test_x[[feature]][is.na(test_x[[feature]])] <- impute_value
  }
  
  keep <- vapply(train_x, function(x) length(unique(x)) > 1, logical(1))
  train_x <- train_x[, keep, drop = FALSE]
  test_x <- test_x[, keep, drop = FALSE]
  
  list(train_x = train_x, test_x = test_x, features = names(train_x))
}

scale_train_test <- function(train_x, test_x) {
  center <- vapply(train_x, mean, numeric(1), na.rm = TRUE)
  scale_value <- vapply(train_x, stats::sd, numeric(1), na.rm = TRUE)
  scale_value[!is.finite(scale_value) | scale_value == 0] <- 1
  
  train_scaled <- sweep(as.matrix(train_x), 2, center, "-")
  train_scaled <- sweep(train_scaled, 2, scale_value, "/")
  
  test_scaled <- sweep(as.matrix(test_x), 2, center, "-")
  test_scaled <- sweep(test_scaled, 2, scale_value, "/")
  
  list(train = train_scaled, test = test_scaled)
}

fit_majority_baseline <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  positive_rate <- mean(as.integer(as.character(train_data$satisfied)) == 1, na.rm = TRUE)
  probability <- rep(positive_rate, nrow(test_data))
  
  evaluate_binary_classifier(
    "Majority Baseline",
    feature_set,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Predicts the training positive-class share for every test row."
  )
}

fit_logistic_regression <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("Logistic Regression", feature_set, start_time, "No usable features."))
  }
  
  train_df <- data.frame(satisfied = as.integer(as.character(train_data$satisfied)), prepared$train_x)
  test_df <- data.frame(prepared$test_x)
  
  fit <- tryCatch(
    stats::glm(satisfied ~ ., data = train_df, family = stats::binomial(), control = stats::glm.control(maxit = 50)),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model("Logistic Regression", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  probability <- tryCatch(
    as.numeric(stats::predict(fit, newdata = test_df, type = "response")),
    error = function(e) rep(NA_real_, nrow(test_data))
  )
  
  evaluate_binary_classifier(
    "Logistic Regression",
    feature_set,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Probability predictions from glm binomial."
  )
}

fit_knn <- function(train_data, test_data, features, feature_set, k = 15L) {
  start_time <- Sys.time()
  if (!FAST_MODE) {
    return(skipped_model(
      "KNN",
      feature_set,
      start_time,
      "Skipped in full mode because exact KNN on the full train/test data is not computationally feasible."
    ))
  }
  
  if (!requireNamespace("class", quietly = TRUE)) {
    return(skipped_model("KNN", feature_set, start_time, "Package class unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("KNN", feature_set, start_time, "No usable features."))
  }
  
  scaled <- scale_train_test(prepared$train_x, prepared$test_x)
  train_y <- factor(as.character(train_data$satisfied), levels = c("0", "1"))
  
  prediction <- tryCatch(
    class::knn(scaled$train, scaled$test, cl = train_y, k = k, prob = TRUE),
    error = function(e) e
  )
  
  if (inherits(prediction, "error")) {
    return(failed_model("KNN", feature_set, start_time, paste("Failed:", prediction$message)))
  }
  
  vote_share <- attr(prediction, "prob")
  probability <- ifelse(as.character(prediction) == "1", vote_share, 1 - vote_share)
  
  evaluate_binary_classifier(
    "KNN",
    feature_set,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    paste0("k = ", k, "; probability is neighbor vote share.")
  )
}

fit_naive_bayes <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  if (!requireNamespace("e1071", quietly = TRUE)) {
    return(skipped_model("Naive Bayes", feature_set, start_time, "Package e1071 unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("Naive Bayes", feature_set, start_time, "No usable features."))
  }
  
  train_df <- data.frame(satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")), prepared$train_x)
  
  fit <- tryCatch(e1071::naiveBayes(satisfied ~ ., data = train_df), error = function(e) e)
  if (inherits(fit, "error")) {
    return(failed_model("Naive Bayes", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  probability_matrix <- tryCatch(
    predict(fit, newdata = prepared$test_x, type = "raw"),
    error = function(e) NULL
  )
  
  if (is.null(probability_matrix) || !"1" %in% colnames(probability_matrix)) {
    return(failed_model("Naive Bayes", feature_set, start_time, "No probability column for class 1."))
  }
  
  evaluate_binary_classifier(
    "Naive Bayes",
    feature_set,
    test_data$satisfied,
    probability_matrix[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Probability predictions from e1071::naiveBayes."
  )
}

fit_svm <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  if (!FAST_MODE) {
    return(skipped_model(
      "SVM",
      feature_set,
      start_time,
      "Skipped in full mode because radial SVM on the full dataset is not computationally feasible."
    ))
  }
  
  if (!requireNamespace("e1071", quietly = TRUE)) {
    return(skipped_model("SVM", feature_set, start_time, "Package e1071 unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("SVM", feature_set, start_time, "No usable features."))
  }
  
  scaled <- scale_train_test(prepared$train_x, prepared$test_x)
  train_df <- data.frame(satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")), scaled$train)
  test_df <- data.frame(scaled$test)
  
  fit <- tryCatch(
    e1071::svm(satisfied ~ ., data = train_df, probability = TRUE, kernel = "radial"),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model("SVM", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  prediction <- tryCatch(predict(fit, newdata = test_df, probability = TRUE), error = function(e) e)
  if (inherits(prediction, "error")) {
    return(failed_model("SVM", feature_set, start_time, paste("Prediction failed:", prediction$message)))
  }
  
  probabilities <- attr(prediction, "probabilities")
  if (is.null(probabilities) || !"1" %in% colnames(probabilities)) {
    return(failed_model("SVM", feature_set, start_time, "SVM did not return probability column for class 1."))
  }
  
  evaluate_binary_classifier(
    "SVM",
    feature_set,
    test_data$satisfied,
    probabilities[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Probability predictions from radial SVM."
  )
}

fit_decision_tree <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  if (!requireNamespace("rpart", quietly = TRUE)) {
    return(skipped_model("Decision Tree", feature_set, start_time, "Package rpart unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("Decision Tree", feature_set, start_time, "No usable features."))
  }
  
  train_df <- data.frame(satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")), prepared$train_x)
  
  fit <- tryCatch(
    rpart::rpart(satisfied ~ ., data = train_df, method = "class", control = rpart::rpart.control(cp = 0.001)),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model("Decision Tree", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  probability_matrix <- predict(fit, newdata = prepared$test_x, type = "prob")
  
  evaluate_binary_classifier(
    "Decision Tree",
    feature_set,
    test_data$satisfied,
    probability_matrix[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Probability predictions from rpart."
  )
}

fit_bagging <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  if (!requireNamespace("ranger", quietly = TRUE)) {
    return(skipped_model("Bagging", feature_set, start_time, "Package ranger unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("Bagging", feature_set, start_time, "No usable features."))
  }
  
  train_df <- data.frame(satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")), prepared$train_x)
  
  fit <- tryCatch(
    ranger::ranger(
      satisfied ~ .,
      data = train_df,
      probability = TRUE,
      num.trees = ifelse(FAST_MODE, 100L, 300L),
      mtry = length(prepared$features),
      min.node.size = 20L,
      seed = 123
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model("Bagging", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  probability <- predict(fit, data = prepared$test_x)$predictions[, "1"]
  
  evaluate_binary_classifier(
    "Bagging",
    feature_set,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Bagging implemented as tree ensemble with mtry = all features."
  )
}

fit_boosting <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  if (!requireNamespace("gbm", quietly = TRUE)) {
    return(skipped_model("Boosting", feature_set, start_time, "Package gbm unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("Boosting", feature_set, start_time, "No usable features."))
  }
  
  n_trees <- ifelse(FAST_MODE, 100L, 300L)
  train_df <- data.frame(satisfied_num = as.integer(as.character(train_data$satisfied)), prepared$train_x)
  
  fit <- tryCatch(
    gbm::gbm(
      satisfied_num ~ .,
      data = train_df,
      distribution = "bernoulli",
      n.trees = n_trees,
      interaction.depth = 2,
      shrinkage = 0.05,
      bag.fraction = 0.8,
      train.fraction = 1,
      verbose = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model("Boosting", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  probability <- as.numeric(predict(fit, newdata = prepared$test_x, n.trees = n_trees, type = "response"))
  
  evaluate_binary_classifier(
    "Boosting",
    feature_set,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    paste0("Gradient boosting with ", n_trees, " trees.")
  )
}

fit_random_forest <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  if (!requireNamespace("ranger", quietly = TRUE)) {
    return(skipped_model("Random Forest", feature_set, start_time, "Package ranger unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("Random Forest", feature_set, start_time, "No usable features."))
  }
  
  train_df <- data.frame(satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")), prepared$train_x)
  
  fit <- tryCatch(
    ranger::ranger(
      satisfied ~ .,
      data = train_df,
      probability = TRUE,
      num.trees = ifelse(FAST_MODE, 100L, 300L),
      mtry = max(1L, floor(sqrt(length(prepared$features)))),
      min.node.size = 20L,
      seed = 123
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model("Random Forest", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  probability <- predict(fit, data = prepared$test_x)$predictions[, "1"]
  
  evaluate_binary_classifier(
    "Random Forest",
    feature_set,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Probability predictions from ranger random forest."
  )
}

fit_neural_network <- function(train_data, test_data, features, feature_set) {
  start_time <- Sys.time()
  if (!FAST_MODE) {
    return(skipped_model(
      "Neural Network",
      feature_set,
      start_time,
      "Skipped in full mode because nnet is not suitable for this full dataset size without dedicated tuning."
    ))
  }
  
  if (!requireNamespace("nnet", quietly = TRUE)) {
    return(skipped_model("Neural Network", feature_set, start_time, "Package nnet unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model("Neural Network", feature_set, start_time, "No usable features."))
  }
  
  scaled <- scale_train_test(prepared$train_x, prepared$test_x)
  y <- as.integer(as.character(train_data$satisfied))
  
  fit <- tryCatch(
    nnet::nnet(
      x = scaled$train,
      y = y,
      size = 5,
      decay = 0.01,
      maxit = ifelse(FAST_MODE, 100L, 200L),
      entropy = TRUE,
      trace = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model("Neural Network", feature_set, start_time, paste("Failed:", fit$message)))
  }
  
  probability <- as.numeric(predict(fit, scaled$test, type = "raw"))
  
  evaluate_binary_classifier(
    "Neural Network",
    feature_set,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    "Small feed-forward neural network from nnet."
  )
}

MODEL_FUNCTIONS <- list(
  fit_majority_baseline,
  fit_logistic_regression,
  fit_knn,
  fit_naive_bayes,
  fit_svm,
  fit_decision_tree,
  fit_bagging,
  fit_boosting,
  fit_random_forest,
  fit_neural_network
)

MODEL_SPECS <- list(
  list(model_name = "Majority Baseline", fit = fit_majority_baseline),
  list(model_name = "Logistic Regression", fit = fit_logistic_regression),
  list(model_name = "KNN", fit = fit_knn),
  list(model_name = "Naive Bayes", fit = fit_naive_bayes),
  list(model_name = "SVM", fit = fit_svm),
  list(model_name = "Decision Tree", fit = fit_decision_tree),
  list(model_name = "Bagging", fit = fit_bagging),
  list(model_name = "Boosting", fit = fit_boosting),
  list(model_name = "Random Forest", fit = fit_random_forest),
  list(model_name = "Neural Network", fit = fit_neural_network)
)

# ================================================================
# Final override definitions
# ================================================================
# The definitions below are intentionally placed at the end of this file.
# R uses the last definition, so these override earlier versions and provide
# the final sample-aware result schema required by the final pipeline.

HEAVY_MODEL_NAMES <- c("KNN", "SVM", "Neural Network")

FULL_DATA_MODEL_NAMES <- c(
  "Majority Baseline",
  "Logistic Regression",
  "Naive Bayes",
  "Decision Tree",
  "Bagging",
  "Random Forest"
)

select_feature_frame <- function(data, features) {
  data <- as.data.frame(data)
  features <- setdiff(features, c("satisfied", "stars"))
  missing_features <- setdiff(features, names(data))
  
  if (length(missing_features) > 0) {
    stop("Missing feature columns: ", paste(missing_features, collapse = ", "))
  }
  
  data[, features, drop = FALSE]
}

metric_row <- function(model_name, feature_set, sample_type, train_rows_used,
                       test_rows_used, accuracy, sensitivity, specificity,
                       balanced_accuracy, roc_auc, runtime_seconds,
                       status, notes) {
  tibble::tibble(
    model_name = model_name,
    feature_set = feature_set,
    sample_type = sample_type,
    train_rows_used = as.integer(train_rows_used),
    test_rows_used = as.integer(test_rows_used),
    Accuracy = as.numeric(accuracy),
    Sensitivity = as.numeric(sensitivity),
    Specificity = as.numeric(specificity),
    Balanced_Accuracy = as.numeric(balanced_accuracy),
    ROC_AUC = as.numeric(roc_auc),
    runtime_seconds = as.numeric(runtime_seconds),
    status = as.character(status),
    notes = as.character(notes)
  )
}

empty_model_result <- function(model_name, feature_set, sample_type,
                               train_data, test_data, start_time,
                               status, notes) {
  metric_row(
    model_name = model_name,
    feature_set = feature_set,
    sample_type = sample_type,
    train_rows_used = nrow(train_data),
    test_rows_used = nrow(test_data),
    accuracy = NA_real_,
    sensitivity = NA_real_,
    specificity = NA_real_,
    balanced_accuracy = NA_real_,
    roc_auc = NA_real_,
    runtime_seconds = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    status = status,
    notes = notes
  )
}

skipped_model <- function(model_name, feature_set, sample_type,
                          train_data, test_data, start_time, notes) {
  empty_model_result(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    start_time,
    "skipped",
    notes
  )
}

failed_model <- function(model_name, feature_set, sample_type,
                         train_data, test_data, start_time, notes) {
  empty_model_result(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    start_time,
    "failed",
    notes
  )
}

evaluate_binary_classifier <- function(model_name, feature_set, sample_type,
                                       train_data, test_data, actual,
                                       probability, runtime_seconds,
                                       notes = "ok") {
  actual <- as.integer(as.character(actual))
  probability <- as.numeric(probability)
  probability[!is.finite(probability)] <- NA_real_
  probability <- pmin(pmax(probability, 0), 1)
  
  predicted <- ifelse(probability >= CLASS_THRESHOLD, 1L, 0L)
  
  tp <- sum(actual == 1 & predicted == 1, na.rm = TRUE)
  tn <- sum(actual == 0 & predicted == 0, na.rm = TRUE)
  fp <- sum(actual == 0 & predicted == 1, na.rm = TRUE)
  fn <- sum(actual == 1 & predicted == 0, na.rm = TRUE)
  
  accuracy <- mean(predicted == actual, na.rm = TRUE)
  sensitivity <- ifelse((tp + fn) > 0, tp / (tp + fn), NA_real_)
  specificity <- ifelse((tn + fp) > 0, tn / (tn + fp), NA_real_)
  balanced_accuracy <- mean(c(sensitivity, specificity), na.rm = TRUE)
  
  roc_auc <- NA_real_
  if (requireNamespace("pROC", quietly = TRUE) && length(unique(actual[!is.na(actual)])) == 2) {
    roc_auc <- tryCatch(
      as.numeric(pROC::auc(pROC::roc(actual, probability, quiet = TRUE, direction = "<"))),
      error = function(e) NA_real_
    )
  }
  
  metric_row(
    model_name = model_name,
    feature_set = feature_set,
    sample_type = sample_type,
    train_rows_used = nrow(train_data),
    test_rows_used = nrow(test_data),
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    balanced_accuracy = balanced_accuracy,
    roc_auc = roc_auc,
    runtime_seconds = runtime_seconds,
    status = "success",
    notes = notes
  )
}

prepare_numeric_features <- function(train_data, test_data, features) {
  features <- setdiff(features, c("satisfied", "stars"))
  
  if (length(features) == 0) {
    return(list(train_x = NULL, test_x = NULL, features = character()))
  }
  
  train_x <- select_feature_frame(train_data, features)
  test_x <- select_feature_frame(test_data, features)
  
  for (feature in features) {
    train_x[[feature]] <- suppressWarnings(as.numeric(train_x[[feature]]))
    test_x[[feature]] <- suppressWarnings(as.numeric(test_x[[feature]]))
    
    impute_value <- median(train_x[[feature]], na.rm = TRUE)
    if (!is.finite(impute_value) || is.na(impute_value)) {
      impute_value <- 0
    }
    
    train_x[[feature]][is.na(train_x[[feature]])] <- impute_value
    test_x[[feature]][is.na(test_x[[feature]])] <- impute_value
  }
  
  keep <- vapply(train_x, function(x) length(unique(x)) > 1, logical(1))
  train_x <- train_x[, keep, drop = FALSE]
  test_x <- test_x[, keep, drop = FALSE]
  
  list(train_x = train_x, test_x = test_x, features = names(train_x))
}

scale_train_test <- function(train_x, test_x) {
  center <- vapply(train_x, mean, numeric(1), na.rm = TRUE)
  scale_value <- vapply(train_x, stats::sd, numeric(1), na.rm = TRUE)
  scale_value[!is.finite(scale_value) | scale_value == 0] <- 1
  
  train_scaled <- sweep(as.matrix(train_x), 2, center, "-")
  train_scaled <- sweep(train_scaled, 2, scale_value, "/")
  
  test_scaled <- sweep(as.matrix(test_x), 2, center, "-")
  test_scaled <- sweep(test_scaled, 2, scale_value, "/")
  
  list(train = train_scaled, test = test_scaled)
}

add_sample_note <- function(notes, sample_type) {
  if (identical(sample_type, "stratified sample")) {
    paste(notes, "Evaluated on stratified sample due to computational constraints.")
  } else {
    notes
  }
}

fit_majority_baseline <- function(train_data, test_data, features, feature_set,
                                  sample_type = "full data") {
  model_name <- "Majority Baseline"
  start_time <- Sys.time()
  positive_rate <- mean(as.integer(as.character(train_data$satisfied)) == 1, na.rm = TRUE)
  probability <- rep(positive_rate, nrow(test_data))
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Predicts the training positive-class share for every test row.", sample_type)
  )
}

fit_logistic_regression <- function(train_data, test_data, features, feature_set,
                                    sample_type = "full data") {
  model_name <- "Logistic Regression"
  start_time <- Sys.time()
  prepared <- prepare_numeric_features(train_data, test_data, features)
  
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  train_df <- data.frame(satisfied = as.integer(as.character(train_data$satisfied)), prepared$train_x)
  test_df <- data.frame(prepared$test_x)
  
  fit <- tryCatch(
    stats::glm(
      satisfied ~ .,
      data = train_df,
      family = stats::binomial(),
      control = stats::glm.control(maxit = 50)
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", fit$message)))
  }
  
  probability <- tryCatch(
    as.numeric(stats::predict(fit, newdata = test_df, type = "response")),
    error = function(e) e
  )
  
  if (inherits(probability, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Prediction failed:", probability$message)))
  }
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Probability predictions from glm binomial.", sample_type)
  )
}

fit_knn <- function(train_data, test_data, features, feature_set,
                    sample_type = "stratified sample", k = 15L) {
  model_name <- "KNN"
  start_time <- Sys.time()
  
  if (!requireNamespace("class", quietly = TRUE)) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "Package class unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  scaled <- scale_train_test(prepared$train_x, prepared$test_x)
  train_y <- factor(as.character(train_data$satisfied), levels = c("0", "1"))
  
  prediction <- tryCatch(
    class::knn(scaled$train, scaled$test, cl = train_y, k = k, prob = TRUE),
    error = function(e) e
  )
  
  if (inherits(prediction, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", prediction$message)))
  }
  
  vote_share <- attr(prediction, "prob")
  probability <- ifelse(as.character(prediction) == "1", vote_share, 1 - vote_share)
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note(paste0("k = ", k, "; probability is neighbor vote share."), sample_type)
  )
}

fit_naive_bayes <- function(train_data, test_data, features, feature_set,
                            sample_type = "full data") {
  model_name <- "Naive Bayes"
  start_time <- Sys.time()
  
  if (!requireNamespace("e1071", quietly = TRUE)) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "Package e1071 unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  train_df <- data.frame(
    satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")),
    prepared$train_x
  )
  
  fit <- tryCatch(e1071::naiveBayes(satisfied ~ ., data = train_df), error = function(e) e)
  if (inherits(fit, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", fit$message)))
  }
  
  probability_matrix <- tryCatch(
    predict(fit, newdata = prepared$test_x, type = "raw"),
    error = function(e) e
  )
  
  if (inherits(probability_matrix, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Prediction failed:", probability_matrix$message)))
  }
  
  if (is.null(probability_matrix) || !"1" %in% colnames(probability_matrix)) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No probability column for class 1."))
  }
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    probability_matrix[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Probability predictions from e1071::naiveBayes.", sample_type)
  )
}

fit_svm <- function(train_data, test_data, features, feature_set,
                    sample_type = "stratified sample") {
  model_name <- "SVM"
  start_time <- Sys.time()
  
  if (!requireNamespace("e1071", quietly = TRUE)) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "Package e1071 unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  scaled <- scale_train_test(prepared$train_x, prepared$test_x)
  train_df <- data.frame(
    satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")),
    scaled$train
  )
  test_df <- data.frame(scaled$test)
  
  fit <- tryCatch(
    e1071::svm(satisfied ~ ., data = train_df, probability = TRUE, kernel = "linear", scale = FALSE),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", fit$message)))
  }
  
  prediction <- tryCatch(predict(fit, newdata = test_df, probability = TRUE), error = function(e) e)
  if (inherits(prediction, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Prediction failed:", prediction$message)))
  }
  
  probabilities <- attr(prediction, "probabilities")
  if (is.null(probabilities) || !"1" %in% colnames(probabilities)) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "SVM did not return probability column for class 1."))
  }
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    probabilities[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Probability predictions from linear SVM.", sample_type)
  )
}

fit_decision_tree <- function(train_data, test_data, features, feature_set,
                              sample_type = "full data") {
  model_name <- "Decision Tree"
  start_time <- Sys.time()
  
  if (!requireNamespace("rpart", quietly = TRUE)) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "Package rpart unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  train_df <- data.frame(
    satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")),
    prepared$train_x
  )
  
  fit <- tryCatch(
    rpart::rpart(
      satisfied ~ .,
      data = train_df,
      method = "class",
      control = rpart::rpart.control(cp = 0.001)
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", fit$message)))
  }
  
  probability_matrix <- tryCatch(
    predict(fit, newdata = prepared$test_x, type = "prob"),
    error = function(e) e
  )
  
  if (inherits(probability_matrix, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Prediction failed:", probability_matrix$message)))
  }
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    probability_matrix[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Probability predictions from rpart.", sample_type)
  )
}

fit_bagging <- function(train_data, test_data, features, feature_set,
                        sample_type = "full data") {
  model_name <- "Bagging"
  start_time <- Sys.time()
  
  if (!requireNamespace("ranger", quietly = TRUE)) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "Package ranger unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  train_df <- data.frame(
    satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")),
    prepared$train_x
  )
  
  fit <- tryCatch(
    ranger::ranger(
      satisfied ~ .,
      data = train_df,
      probability = TRUE,
      num.trees = ifelse(FAST_MODE, 100L, 200L),
      mtry = length(prepared$features),
      min.node.size = 50L,
      seed = 123,
      num.threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", fit$message)))
  }
  
  prediction <- tryCatch(predict(fit, data = prepared$test_x), error = function(e) e)
  if (inherits(prediction, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Prediction failed:", prediction$message)))
  }
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    prediction$predictions[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Bagging implemented as tree ensemble with mtry = all features.", sample_type)
  )
}

fit_random_forest <- function(train_data, test_data, features, feature_set,
                              sample_type = "full data") {
  model_name <- "Random Forest"
  start_time <- Sys.time()
  
  if (!requireNamespace("ranger", quietly = TRUE)) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "Package ranger unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  train_df <- data.frame(
    satisfied = factor(as.character(train_data$satisfied), levels = c("0", "1")),
    prepared$train_x
  )
  
  fit <- tryCatch(
    ranger::ranger(
      satisfied ~ .,
      data = train_df,
      probability = TRUE,
      num.trees = ifelse(FAST_MODE, 100L, 200L),
      mtry = max(1L, floor(sqrt(length(prepared$features)))),
      min.node.size = 50L,
      seed = 123,
      num.threads = max(1L, parallel::detectCores(logical = TRUE) - 1L)
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", fit$message)))
  }
  
  prediction <- tryCatch(predict(fit, data = prepared$test_x), error = function(e) e)
  if (inherits(prediction, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Prediction failed:", prediction$message)))
  }
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    prediction$predictions[, "1"],
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Probability predictions from ranger random forest.", sample_type)
  )
}

fit_neural_network <- function(train_data, test_data, features, feature_set,
                               sample_type = "stratified sample") {
  model_name <- "Neural Network"
  start_time <- Sys.time()
  
  if (!requireNamespace("nnet", quietly = TRUE)) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "Package nnet unavailable."))
  }
  
  prepared <- prepare_numeric_features(train_data, test_data, features)
  if (length(prepared$features) == 0) {
    return(skipped_model(model_name, feature_set, sample_type, train_data, test_data, start_time, "No usable features."))
  }
  
  scaled <- scale_train_test(prepared$train_x, prepared$test_x)
  y <- as.integer(as.character(train_data$satisfied))
  
  fit <- tryCatch(
    nnet::nnet(
      x = scaled$train,
      y = y,
      size = 5,
      decay = 0.01,
      maxit = 100L,
      entropy = TRUE,
      trace = FALSE
    ),
    error = function(e) e
  )
  
  if (inherits(fit, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Failed:", fit$message)))
  }
  
  probability <- tryCatch(as.numeric(predict(fit, scaled$test, type = "raw")), error = function(e) e)
  if (inherits(probability, "error")) {
    return(failed_model(model_name, feature_set, sample_type, train_data, test_data, start_time, paste("Prediction failed:", probability$message)))
  }
  
  evaluate_binary_classifier(
    model_name,
    feature_set,
    sample_type,
    train_data,
    test_data,
    test_data$satisfied,
    probability,
    as.numeric(difftime(Sys.time(), start_time, units = "secs")),
    add_sample_note("Small feed-forward neural network from nnet.", sample_type)
  )
}

MODEL_SPECS <- list(
  list(model_name = "Majority Baseline", model_group = "full", fit = fit_majority_baseline),
  list(model_name = "Logistic Regression", model_group = "full", fit = fit_logistic_regression),
  list(model_name = "Naive Bayes", model_group = "full", fit = fit_naive_bayes),
  list(model_name = "Decision Tree", model_group = "full", fit = fit_decision_tree),
  list(model_name = "Bagging", model_group = "full", fit = fit_bagging),
  list(model_name = "Random Forest", model_group = "full", fit = fit_random_forest),
  list(model_name = "KNN", model_group = "heavy", fit = fit_knn),
  list(model_name = "SVM", model_group = "heavy", fit = fit_svm),
  list(model_name = "Neural Network", model_group = "heavy", fit = fit_neural_network)
)



# ================================================================
# 04_run_model_comparison.R
# Fixed stratified-sample model comparison
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

#source(file.path(PROJECT_ROOT, "scripts", "final", "03_model_functions.R"))

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