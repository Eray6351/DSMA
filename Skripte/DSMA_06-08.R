# ================================================================
# 06_plot_star_distribution.R
# Descriptive statistics: star rating distribution
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("06_plot_star_distribution.R started.")

# ── Load data ────────────────────────────────────────────────────────────────
model_dataset <- readRDS(FINAL_MODEL_DATASET_PATH)
data.table::setDT(model_dataset)

if (!"stars" %in% names(model_dataset)) {
  stop("stars column not found in final model dataset. Cannot create star distribution plot.")
}

# ── Output path ──────────────────────────────────────────────────────────────
STAR_DIST_PLOT_PATH <- file.path(OUTPUT_DIR, "descriptive_star_distribution.png")

# ── Compute counts and shares ────────────────────────────────────────────────
star_counts <- model_dataset[
  !is.na(stars),
  .(count = .N),
  by = .(stars = as.integer(stars))
][order(stars)]

star_counts[, share := count / sum(count) * 100]

# Assign satisfaction group label for fill colour
star_counts[, group := data.table::fifelse(
  stars > 3.5,
  "Satisfied (> 3.5 stars)",
  "Not Satisfied (\u2264 3.5 stars)"
)]

# Force factor order so 1-star is on the left
star_counts[, stars := factor(stars, levels = 1:5)]
star_counts[, group := factor(group, levels = c(
  "Not Satisfied (\u2264 3.5 stars)",
  "Satisfied (> 3.5 stars)"
))]

# ── Plot ─────────────────────────────────────────────────────────────────────
star_plot <- ggplot2::ggplot(
  star_counts,
  ggplot2::aes(x = stars, y = share, fill = group)
) +
  ggplot2::geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
  ggplot2::geom_text(
    ggplot2::aes(label = sprintf("%.1f%%", share)),
    vjust = -0.5,
    size = 3.8,
    fontface = "bold",
    colour = "#333333"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Not Satisfied (\u2264 3.5 stars)" = "#4C78A8",
      "Satisfied (> 3.5 stars)"          = "#F58518"
    )
  ) +
  ggplot2::scale_y_continuous(
    limits = c(0, max(star_counts$share) * 1.15),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  ggplot2::labs(
    title    = "Star Rating Distribution",
    subtitle = paste0(
      "Individual Yelp restaurant reviews - Philadelphia, PA",
      " (n = ", format(sum(star_counts$count), big.mark = ","), ")"
    ),
    x    = "Star Rating",
    y    = "Share of Reviews (%)",
    fill = "Satisfaction Group"
  ) +
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title      = ggplot2::element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor   = ggplot2::element_blank()
  )

ggplot2::ggsave(STAR_DIST_PLOT_PATH, star_plot, width = 8, height = 5, dpi = 300)

log_message("Saved star distribution plot to: ", STAR_DIST_PLOT_PATH)
log_message("06_plot_star_distribution.R completed.")


# ================================================================
# 07_plot_seasonal_weather.R
# Descriptive statistics: seasonal weather patterns in the dataset
# Dual-axis chart: monthly avg TMAX/TMIN (lines) + avg daily PRCP (bars)
# Style matches the Philadelphia climate illustration in the paper.
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("07_plot_seasonal_weather.R started.")

# ── Load data ────────────────────────────────────────────────────────────────
model_dataset <- readRDS(FINAL_MODEL_DATASET_PATH)
data.table::setDT(model_dataset)

weather_cols <- intersect(c("PRCP", "TMAX", "TMIN", "review_date"), names(model_dataset))
missing_cols <- setdiff(c("PRCP", "TMAX", "TMIN", "review_date"), names(model_dataset))
if (length(missing_cols) > 0) {
  stop("Missing required weather columns: ", paste(missing_cols, collapse = ", "))
}

# ── Output path ──────────────────────────────────────────────────────────────
SEASONAL_WEATHER_PLOT_PATH <- file.path(OUTPUT_DIR, "descriptive_seasonal_weather.png")

# ── Aggregate to monthly averages ────────────────────────────────────────────
# PRCP is a daily value repeated across all reviews on the same date.
# To avoid inflating precipitation by the number of reviews per day,
# we first deduplicate to one row per date, then aggregate.

# Step 1: one row per unique date (deduplicate PRCP, TMAX, TMIN by date)
daily_weather <- unique(
  model_dataset[
    !is.na(TMAX) & !is.na(TMIN) & !is.na(PRCP),
    .(review_date, PRCP, TMAX, TMIN)
  ],
  by = "review_date"
)

daily_weather[, year  := lubridate::year(as.Date(review_date))]
daily_weather[, month := lubridate::month(as.Date(review_date))]

# Step 2: sum daily PRCP within each (year, month) to get monthly totals
monthly_by_year <- daily_weather[
  ,
  .(
    monthly_prcp = sum(PRCP, na.rm = TRUE),
    avg_tmax     = mean(TMAX, na.rm = TRUE),
    avg_tmin     = mean(TMIN, na.rm = TRUE)
  ),
  by = .(year, month)
]

# Step 3: average monthly totals across years to get a representative month profile
weather_data <- monthly_by_year[
  ,
  .(
    avg_prcp   = mean(monthly_prcp, na.rm = TRUE),
    avg_tmax   = mean(avg_tmax, na.rm = TRUE),
    avg_tmin   = mean(avg_tmin, na.rm = TRUE),
    n_years    = .N
  ),
  by = month
][order(month)]

# Month labels
weather_data[, month_label := factor(
  month.abb[month],
  levels = month.abb
)]

log_message("Monthly weather summary (avg monthly totals across all years in dataset):")
log_message(paste(capture.output(print(weather_data[, .(month_label, avg_tmax, avg_tmin, avg_prcp, n_years)])), collapse = " | "))

# ── Dual-axis scaling ─────────────────────────────────────────────────────────
# Primary y-axis: temperature (°C)
# Secondary y-axis: precipitation (mm)
# We scale PRCP to temperature axis for overlay, then label the secondary axis.

temp_min  <- floor(min(weather_data$avg_tmin) / 5) * 5 - 5
temp_max  <- ceiling(max(weather_data$avg_tmax) / 5) * 5 + 5
prcp_max  <- ceiling(max(weather_data$avg_prcp) * 1.3 / 10) * 10

# Linear mapping: prcp -> temp axis
# prcp = 0        maps to temp_min
# prcp = prcp_max maps to temp_max
scale_prcp <- function(x) temp_min + x * (temp_max - temp_min) / prcp_max
unscale_prcp <- function(y) (y - temp_min) * prcp_max / (temp_max - temp_min)

weather_data[, prcp_scaled := scale_prcp(avg_prcp)]

# ── Plot ─────────────────────────────────────────────────────────────────────
seasonal_plot <- ggplot2::ggplot(weather_data, ggplot2::aes(x = month_label)) +
  
  # Precipitation bars (scaled to primary axis)
  ggplot2::geom_col(
    ggplot2::aes(y = prcp_scaled, group = 1),
    fill   = "#6ECAC8",
    colour = "white",
    linewidth = 0.2,
    width  = 0.65
  ) +
  
  # TMAX line
  ggplot2::geom_line(
    ggplot2::aes(y = avg_tmax, group = 1),
    colour    = "#D94F3D",
    linewidth = 1.3
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = avg_tmax),
    colour = "#D94F3D",
    size   = 3,
    shape  = 21,
    fill   = "white",
    stroke = 1.5
  ) +
  
  # TMIN line
  ggplot2::geom_line(
    ggplot2::aes(y = avg_tmin, group = 1),
    colour    = "#3A6FA8",
    linewidth = 1.3
  ) +
  ggplot2::geom_point(
    ggplot2::aes(y = avg_tmin),
    colour = "#3A6FA8",
    size   = 3,
    shape  = 21,
    fill   = "white",
    stroke = 1.5
  ) +
  
  # Primary y-axis: temperature
  ggplot2::scale_y_continuous(
    name   = "Temperature (°C)",
    limits = c(temp_min, temp_max),
    breaks = seq(temp_min, temp_max, by = 5),
    expand = c(0, 0),
    # Secondary axis: precipitation (back-transform scale)
    sec.axis = ggplot2::sec_axis(
      transform = unscale_prcp,
      name   = "Monthly Precipitation (mm)",
      breaks = seq(0, prcp_max, by = max(1, round(prcp_max / 5)))
    )
  ) +
  
  ggplot2::scale_x_discrete(name = "Month") +
  
  ggplot2::labs(
    title    = "Philadelphia, Pennsylvania",
    subtitle = "Monthly average temperature and total precipitation across review dates in the dataset"
  ) +
  
  ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle    = ggplot2::element_text(hjust = 0.5, size = 10, colour = "#555555"),
    axis.title.y.left  = ggplot2::element_text(colour = "#3A6FA8"),
    axis.title.y.right = ggplot2::element_text(colour = "#6ECAC8"),
    axis.text.y.left   = ggplot2::element_text(colour = "#3A6FA8"),
    axis.text.y.right  = ggplot2::element_text(colour = "#6ECAC8"),
    panel.grid.minor   = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
    panel.background = ggplot2::element_rect(fill = "white", colour = NA),
    legend.position    = "none"
  ) +
  
  # Manual legend annotation via caption
  ggplot2::labs(
    caption = "\u2014  Max. Temperature (°C)       \u2014  Min. Temperature (°C)       \u2588  Monthly Precipitation (mm)"
  ) +
  ggplot2::theme(
    plot.caption = ggplot2::element_text(hjust = 0.5, size = 9, colour = "#555555")
  )

ggplot2::ggsave(
  SEASONAL_WEATHER_PLOT_PATH,
  seasonal_plot,
  width  = 9,
  height = 5.5,
  dpi    = 300
)

log_message("Saved seasonal weather plot to: ", SEASONAL_WEATHER_PLOT_PATH)
log_message("07_plot_seasonal_weather.R completed.")



# ================================================================
# 08_logistic_regression_coefficients.R
# Extract and save Logistic Regression coefficients table
# for both feature sets (Without Weather and With Weather)
# to support evaluation of H1 and H2.
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("08_logistic_regression_coefficients.R started.")

# ── Load data ────────────────────────────────────────────────────────────────
model_dataset <- readRDS(FINAL_MODEL_DATASET_PATH)
data.table::setDT(model_dataset)

feature_dictionary <- data.table::fread(FEATURE_DICTIONARY_PATH)

model_dataset <- model_dataset[!is.na(satisfied)]
model_dataset[, satisfied := as.integer(as.character(satisfied))]

# ── Reproduce the exact training sample from script 04 ───────────────────────
set.seed(SAMPLE_SEED)
train_ids  <- sample(seq_len(nrow(model_dataset)), size = floor(TRAIN_SHARE * nrow(model_dataset)))
train_full <- model_dataset[train_ids]

stratified_sample_dt <- function(data, target_n, seed) {
  data.table::setDT(data)
  if (nrow(data) <= target_n) return(data.table::copy(data))
  data <- data.table::copy(data)
  data[, sample_id_internal := .I]
  class_counts <- data[, .N, by = satisfied]
  class_counts[, raw_n    := as.numeric(target_n) * as.numeric(N) / as.numeric(sum(N))]
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
    current_n     <- as.integer(min(class_counts$sample_n[i], class_counts$N[i]))
    data[satisfied == current_class][sample(.N, current_n)]
  })
  sampled <- data.table::rbindlist(sampled_parts, use.names = TRUE, fill = TRUE)
  sampled[, sample_id_internal := NULL]
  sampled
}

train_data <- stratified_sample_dt(
  train_full,
  target_n = min(NORMAL_TRAIN_SAMPLE_N, nrow(train_full)),
  seed     = SAMPLE_SEED + 1L
)

# ── Feature sets ──────────────────────────────────────────────────────────────
base_features <- feature_dictionary[use_in_base == TRUE, feature]
base_features <- intersect(base_features, names(model_dataset))
base_features <- setdiff(base_features, c("stars", "satisfied"))

weather_features <- unique(c(
  base_features,
  intersect(c("PRCP", "TMAX", "TMIN"), names(model_dataset))
))

feature_sets <- list(
  "Without Weather" = base_features,
  "With Weather"    = weather_features
)

# ── Helper: prepare features and fit glm ─────────────────────────────────────
fit_logistic <- function(train_data, features, label) {
  train_x <- as.data.frame(train_data)[, features, drop = FALSE]
  
  for (feat in features) {
    train_x[[feat]] <- suppressWarnings(as.numeric(train_x[[feat]]))
    impute_val <- median(train_x[[feat]], na.rm = TRUE)
    if (!is.finite(impute_val)) impute_val <- 0
    train_x[[feat]][is.na(train_x[[feat]])] <- impute_val
  }
  
  keep <- vapply(train_x, function(x) length(unique(x)) > 1, logical(1))
  train_x <- train_x[, keep, drop = FALSE]
  
  train_df <- data.frame(satisfied = train_data$satisfied, train_x)
  
  log_message("Fitting Logistic Regression: ", label, " (", ncol(train_x), " features)")
  
  fit <- stats::glm(
    satisfied ~ .,
    data    = train_df,
    family  = stats::binomial(),
    control = stats::glm.control(maxit = 50)
  )
  
  log_message("Done: ", label)
  fit
}

# ── Helper: extract formatted coefficients from a fitted glm ─────────────────
extract_coefs <- function(fit, feature_set_label) {
  ct <- as.data.frame(summary(fit)$coefficients)
  ct <- tibble::rownames_to_column(ct, var = "feature")
  names(ct) <- c("feature", "coefficient", "std_error", "z_value", "p_value")
  
  ct$feature_set <- feature_set_label
  ct$odds_ratio  <- exp(ct$coefficient)
  
  ct$significance <- dplyr::case_when(
    ct$p_value < 0.001 ~ "***",
    ct$p_value < 0.01  ~ "**",
    ct$p_value < 0.05  ~ "*",
    ct$p_value < 0.1   ~ ".",
    TRUE               ~ ""
  )
  
  ct$direction <- dplyr::case_when(
    ct$feature == "(Intercept)" ~ "-",
    ct$coefficient > 0          ~ "positive",
    ct$coefficient < 0          ~ "negative",
    TRUE                        ~ "zero"
  )
  
  ct$coefficient <- round(ct$coefficient, 4)
  ct$std_error   <- round(ct$std_error,   4)
  ct$z_value     <- round(ct$z_value,     3)
  ct$p_value     <- round(ct$p_value,     4)
  ct$odds_ratio  <- round(ct$odds_ratio,  4)
  
  ct
}

# ── Fit both models and extract coefficients ──────────────────────────────────
fit_base    <- fit_logistic(train_data, base_features,    "Without Weather")
fit_weather <- fit_logistic(train_data, weather_features, "With Weather")

coef_base    <- extract_coefs(fit_base,    "Without Weather")
coef_weather <- extract_coefs(fit_weather, "With Weather")

# ── Combined long-format table (all rows from both models) ────────────────────
coef_combined <- rbind(coef_base, coef_weather)

# Sort within each feature set: intercept first, then by |coefficient| desc
sort_coefs <- function(ct) {
  intercept_row <- ct[ct$feature == "(Intercept)", ]
  other_rows    <- ct[ct$feature != "(Intercept)", ]
  other_rows    <- other_rows[order(abs(other_rows$coefficient), decreasing = TRUE), ]
  rbind(intercept_row, other_rows)
}

coef_base    <- sort_coefs(coef_base)
coef_weather <- sort_coefs(coef_weather)
coef_combined <- rbind(coef_base, coef_weather)

# ── Wide-format comparison table (one row per feature, two sets of columns) ───
# Base columns
base_wide <- coef_base[, c("feature", "coefficient", "std_error", "p_value",
                           "odds_ratio", "significance", "direction")]
names(base_wide)[-1] <- paste0(names(base_wide)[-1], "_base")

# Weather columns
weather_wide <- coef_weather[, c("feature", "coefficient", "std_error", "p_value",
                                 "odds_ratio", "significance", "direction")]
names(weather_wide)[-1] <- paste0(names(weather_wide)[-1], "_weather")

coef_wide <- merge(base_wide, weather_wide, by = "feature", all = TRUE)

# Sort wide table: intercept first, then by |base coefficient| desc
intercept_row <- coef_wide[coef_wide$feature == "(Intercept)", ]
other_rows    <- coef_wide[coef_wide$feature != "(Intercept)", ]
other_rows    <- other_rows[order(abs(other_rows$coefficient_base), decreasing = TRUE,
                                  na.last = TRUE), ]
coef_wide <- rbind(intercept_row, other_rows)

# ── Output paths ───────────────────────────────────────────────────────────────
COEF_LONG_CSV_PATH  <- file.path(OUTPUT_DIR, "logistic_regression_coefficients_long.csv")
COEF_WIDE_CSV_PATH  <- file.path(OUTPUT_DIR, "logistic_regression_coefficients_wide.csv")
COEF_XLSX_PATH      <- file.path(OUTPUT_DIR, "logistic_regression_coefficients.xlsx")

data.table::fwrite(coef_combined, COEF_LONG_CSV_PATH)
data.table::fwrite(coef_wide,     COEF_WIDE_CSV_PATH)
log_message("Saved long-format coefficients to: ", COEF_LONG_CSV_PATH)
log_message("Saved wide-format coefficients to: ", COEF_WIDE_CSV_PATH)

if (requireNamespace("openxlsx", quietly = TRUE)) {
  openxlsx::write.xlsx(
    list(
      "Wide (side-by-side)"    = coef_wide,
      "Without Weather"        = coef_base,
      "With Weather"           = coef_weather
    ),
    COEF_XLSX_PATH,
    overwrite = TRUE
  )
  log_message("Saved Excel workbook to: ", COEF_XLSX_PATH)
}

# ── Summary log ───────────────────────────────────────────────────────────────
log_message("--- Without Weather ---")
log_message("Features (excl. intercept): ", nrow(coef_base) - 1L)
log_message("Significant at p < 0.05:   ", sum(coef_base$p_value < 0.05, na.rm = TRUE))

log_message("--- With Weather ---")
log_message("Features (excl. intercept): ", nrow(coef_weather) - 1L)
log_message("Weather coefficients (PRCP, TMAX, TMIN):")
weather_vars <- coef_weather[coef_weather$feature %in% c("PRCP", "TMAX", "TMIN"), ]
for (i in seq_len(nrow(weather_vars))) {
  log_message(
    "  ", weather_vars$feature[i],
    ": coef = ", weather_vars$coefficient[i],
    ", OR = ",   weather_vars$odds_ratio[i],
    ", p = ",    weather_vars$p_value[i],
    " ",         weather_vars$significance[i]
  )
}

log_message("08_logistic_regression_coefficients.R completed.")