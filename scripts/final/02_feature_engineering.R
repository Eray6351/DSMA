# ================================================================
# 02_feature_engineering.R
# Create modeling features without target leakage
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("02_feature_engineering.R started.")

prepared_dataset <- readRDS(PREPARED_DATASET_PATH)
data.table::setDT(prepared_dataset)

log_message("Available columns: ", paste(names(prepared_dataset), collapse = ", "))

feature_dictionary <- data.table::data.table(
  feature = character(),
  feature_group = character(),
  description = character(),
  source = character(),
  use_in_base = logical(),
  use_in_weather = logical(),
  notes = character()
)

register_feature <- function(feature, feature_group, description, source, use_in_base = TRUE, notes = "") {
  if (feature %in% names(prepared_dataset)) {
    feature_dictionary <<- data.table::rbindlist(
      list(
        feature_dictionary,
        data.table::data.table(
          feature = feature,
          feature_group = feature_group,
          description = description,
          source = source,
          use_in_base = use_in_base,
          use_in_weather = use_in_base,
          notes = notes
        )
      ),
      fill = TRUE
    )
  }
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

normalize_attr_value <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NaN", "NULL", "null", "None", "none")] <- NA_character_
  x <- gsub("^u'", "'", x)
  x <- gsub("^u\"", "\"", x)
  x <- gsub("^'(.*)'$", "\\1", x)
  x <- gsub("^\"(.*)\"$", "\\1", x)
  trimws(x)
}

extract_attr_raw <- function(attributes, key) {
  attributes <- as.character(attributes)
  key_pattern <- escape_regex(key)
  patterns <- c(
    paste0("['\"]", key_pattern, "['\"]\\s*:\\s*['\"]([^'\"]*)['\"]"),
    paste0("['\"]", key_pattern, "['\"]\\s*:\\s*([^,}\\]]+)")
  )

  out <- rep(NA_character_, length(attributes))

  for (pattern in patterns) {
    hit <- regexpr(pattern, attributes, perl = TRUE)
    matched <- is.na(out) & !is.na(hit) & hit > 0
    if (any(matched)) {
      out[matched] <- sub(pattern, "\\1", regmatches(attributes, hit)[matched], perl = TRUE)
    }
  }

  normalize_attr_value(out)
}

attr_bool <- function(attributes, key) {
  value <- tolower(extract_attr_raw(attributes, key))
  out <- rep(NA_real_, length(value))
  out[value %in% c("true", "1", "yes", "free")] <- 1
  out[value %in% c("false", "0", "no", "none")] <- 0
  out
}

attr_numeric <- function(attributes, key) {
  suppressWarnings(as.numeric(extract_attr_raw(attributes, key)))
}

row_mean_available <- function(dt, cols) {
  cols <- intersect(cols, names(dt))
  if (length(cols) == 0) {
    return(rep(NA_real_, nrow(dt)))
  }
  rowMeans(as.matrix(dt[, ..cols]), na.rm = TRUE)
}

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

  register_feature("food_quality_score", "text_proxy", "Food-quality word count proxy.", text_column)
  register_feature("service_score", "text_proxy", "Service word count proxy.", text_column)
  register_feature("ambience_score", "text_proxy", "Ambience word count proxy.", text_column)
  register_feature("price_score", "text_proxy", "Price/value word count proxy.", text_column)
} else {
  log_message("WARNING: No text/review_text column found. Text-based proxies cannot be created.")
  log_message("WARNING: Food Quality cannot be directly measured without review text.")
  feature_dictionary <- data.table::rbindlist(
    list(
      feature_dictionary,
      data.table::data.table(
        feature = "food_quality_score",
        feature_group = "not_available",
        description = "Direct Food Quality proxy cannot be created because no text or review_text column is available.",
        source = "reviews_final.csv",
        use_in_base = FALSE,
        use_in_weather = FALSE,
        notes = "Data limitation: Food Quality is not directly observed in the current dataset."
      )
    ),
    fill = TRUE
  )
}

if ("attributes" %in% names(prepared_dataset)) {
  log_message("Extracting structured Yelp attributes from attributes column.")

  prepared_dataset[, attr_price_range := attr_numeric(attributes, "RestaurantsPriceRange2")]

  noise_raw <- tolower(extract_attr_raw(prepared_dataset$attributes, "NoiseLevel"))
  prepared_dataset[, attr_noise_score := data.table::fcase(
    noise_raw %in% c("quiet"), 2,
    noise_raw %in% c("average"), 1,
    noise_raw %in% c("loud", "very_loud", "very loud"), 0,
    default = NA_real_
  )]

  prepared_dataset[, attr_outdoor_seating := attr_bool(attributes, "OutdoorSeating")]
  prepared_dataset[, attr_has_tv := attr_bool(attributes, "HasTV")]
  alcohol_raw <- tolower(extract_attr_raw(prepared_dataset$attributes, "Alcohol"))
  prepared_dataset[, attr_alcohol_present := as.numeric(!is.na(alcohol_raw) &
    !(alcohol_raw %in% c("none", "no", "false")))]
  prepared_dataset[, attr_wifi_available := as.numeric(tolower(extract_attr_raw(attributes, "WiFi")) %in% c("free", "paid", "yes", "true"))]

  prepared_dataset[, attr_takeout := attr_bool(attributes, "RestaurantsTakeOut")]
  prepared_dataset[, attr_delivery := attr_bool(attributes, "RestaurantsDelivery")]
  prepared_dataset[, attr_caters := attr_bool(attributes, "Caters")]
  prepared_dataset[, attr_good_for_kids := attr_bool(attributes, "GoodForKids")]
  prepared_dataset[, attr_bike_parking := attr_bool(attributes, "BikeParking")]

  if (!"price_score" %in% names(prepared_dataset)) {
    prepared_dataset[, price_score := attr_price_range]
    register_feature("price_score", "structured_proxy", "Price proxy from RestaurantsPriceRange2.", "attributes")
  }

  if (!"ambience_score" %in% names(prepared_dataset)) {
    ambience_cols <- c(
      "attr_noise_score",
      "attr_outdoor_seating",
      "attr_has_tv",
      "attr_alcohol_present",
      "attr_wifi_available"
    )
    prepared_dataset[, ambience_score := row_mean_available(.SD, ambience_cols)]
    register_feature("ambience_score", "structured_proxy", "Ambience proxy from NoiseLevel, OutdoorSeating, HasTV, Alcohol, and WiFi.", "attributes")
  }

  if (!"service_score" %in% names(prepared_dataset)) {
    service_cols <- c(
      "attr_takeout",
      "attr_delivery",
      "attr_caters",
      "attr_good_for_kids",
      "attr_bike_parking"
    )
    prepared_dataset[, service_score := row_mean_available(.SD, service_cols)]
    register_feature("service_score", "structured_proxy", "Service/convenience proxy from takeout, delivery, caters, kids, and bike parking attributes.", "attributes")
  }

  structured_controls <- c(
    "attr_price_range",
    "attr_noise_score",
    "attr_outdoor_seating",
    "attr_has_tv",
    "attr_alcohol_present",
    "attr_wifi_available",
    "attr_takeout",
    "attr_delivery",
    "attr_caters",
    "attr_good_for_kids",
    "attr_bike_parking"
  )

  for (control in structured_controls) {
    register_feature(control, "structured_attribute", paste0("Structured Yelp attribute: ", control), "attributes")
  }
} else {
  log_message("WARNING: attributes column missing. Structured Yelp attribute proxies cannot be extracted.")
}

if ("categories" %in% names(prepared_dataset)) {
  category_counter <- new.env(parent = emptyenv())
  category_values <- prepared_dataset$categories
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

  category_names <- ls(category_counter)
  category_counts <- vapply(category_names, function(x) category_counter[[x]], integer(1))
  selected_categories <- names(sort(category_counts, decreasing = TRUE))[seq_len(min(15L, length(category_counts)))]

  for (category in selected_categories) {
    feature_name <- paste0("cat_", safe_name(category))
    pattern <- paste0("(^|,\\s*)", escape_regex(category), "(\\s*,|$)")
    prepared_dataset[, (feature_name) := as.integer(grepl(pattern, categories, perl = TRUE))]
    register_feature(feature_name, "category_control", paste0("Frequent restaurant category: ", category), "categories")
  }

  log_message("Created category controls: ", paste(selected_categories, collapse = ", "))
}

if (!all(c("business_id", "review_date", "stars") %in% names(prepared_dataset))) {
  stop("business_id, review_date, and stars are required for prior business features.")
}

prior_source <- prepared_dataset[
  !is.na(business_id) & !is.na(review_date) & !is.na(stars),
  .(business_id, review_date, stars)
]

daily_prior <- prior_source[
  ,
  .(
    date_review_count = .N,
    date_star_sum = sum(stars, na.rm = TRUE)
  ),
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
  daily_prior[, .(
    business_id,
    review_date,
    prior_business_avg_stars,
    business_review_count_before
  )],
  by = c("business_id", "review_date"),
  all.x = TRUE,
  sort = FALSE
)

data.table::setorder(prepared_dataset, row_id)

prepared_dataset[is.na(business_review_count_before), business_review_count_before := 0]

register_feature("prior_business_avg_stars", "prior_reputation", "Average stars from earlier reviews of the same business only.", "reviews_final.csv")
register_feature("business_review_count_before", "prior_reputation", "Number of earlier reviews of the same business.", "reviews_final.csv")

weather_features <- intersect(c("PRCP", "TMAX", "TMIN"), names(prepared_dataset))

for (weather_feature in weather_features) {
  feature_dictionary <- data.table::rbindlist(
    list(
      feature_dictionary,
      data.table::data.table(
        feature = weather_feature,
        feature_group = "weather",
        description = paste0("External weather variable: ", weather_feature),
        source = "weather_philly_daily.csv",
        use_in_base = FALSE,
        use_in_weather = TRUE,
        notes = "Main weather specification uses PRCP, TMAX, and TMIN."
      )
    ),
    fill = TRUE
  )
}

base_features <- feature_dictionary[use_in_base == TRUE, unique(feature)]
base_features <- setdiff(base_features, c("stars", "satisfied"))
base_features <- intersect(base_features, names(prepared_dataset))

weather_feature_set <- unique(c(base_features, weather_features))

id_columns <- intersect(
  c("row_id", "review_id", "business_id", "review_date", "satisfied", "stars", "categories", "attributes"),
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

attr(final_model_dataset, "base_features") <- base_features
attr(final_model_dataset, "weather_features") <- weather_feature_set

feature_dictionary[, use_in_weather := feature %in% weather_feature_set]
feature_dictionary <- unique(feature_dictionary, by = "feature")

saveRDS(final_model_dataset, FINAL_MODEL_DATASET_PATH)
data.table::fwrite(feature_dictionary, FEATURE_DICTIONARY_PATH)

log_message("Final model rows: ", nrow(final_model_dataset))
log_message("Base features: ", paste(base_features, collapse = ", "))
log_message("Weather features: ", paste(weather_feature_set, collapse = ", "))
log_message("Saved final model dataset to: ", FINAL_MODEL_DATASET_PATH)
log_message("Saved feature dictionary to: ", FEATURE_DICTIONARY_PATH)
log_message("02_feature_engineering.R completed.")
