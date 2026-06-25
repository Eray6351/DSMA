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

