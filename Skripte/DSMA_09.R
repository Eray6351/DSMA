# ================================================================
# 09_bivariate_descriptive_plots.R
# Bivariate descriptive analysis:
#   Plot A: Satisfaction rate by precipitation category
#   Plot B: Satisfaction rate by temperature quartile
#   Plot C: Satisfaction rate by restaurant price range
#   Plot D: Satisfaction rate by top restaurant categories
#   Plot E: Review volume over time (by year)
# ================================================================

if (!exists("PROJECT_ROOT")) {
  source(file.path("scripts", "final", "00_config.R"))
}

log_message("09_bivariate_descriptive_plots.R started.")

# ── Load data ────────────────────────────────────────────────────────────────
model_dataset <- readRDS(FINAL_MODEL_DATASET_PATH)
data.table::setDT(model_dataset)
model_dataset[, satisfied := as.integer(as.character(satisfied))]

# ── Output paths ─────────────────────────────────────────────────────────────
PLOT_PRECIP_PATH    <- file.path(OUTPUT_DIR, "descriptive_satisfaction_by_precipitation.png")
PLOT_TEMP_PATH      <- file.path(OUTPUT_DIR, "descriptive_satisfaction_by_temperature.png")
PLOT_PRICE_PATH     <- file.path(OUTPUT_DIR, "descriptive_satisfaction_by_price.png")
PLOT_CATEGORY_PATH  <- file.path(OUTPUT_DIR, "descriptive_satisfaction_by_category.png")
PLOT_VOLUME_PATH    <- file.path(OUTPUT_DIR, "descriptive_review_volume_over_time.png")

# ── Shared theme ──────────────────────────────────────────────────────────────
plot_theme <- ggplot2::theme_minimal(base_size = 12) +
  ggplot2::theme(
    plot.title       = ggplot2::element_text(face = "bold"),
    plot.subtitle    = ggplot2::element_text(colour = "#555555", size = 10),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    legend.position  = "none"
  )

FILL_SAT   <- "#F58518"   # orange  — satisfied
FILL_UNSAT <- "#4C78A8"   # blue    — not satisfied (used for reference lines)

# ================================================================
# PLOT A: Satisfaction rate by precipitation category
# ================================================================
log_message("Creating Plot A: satisfaction rate by precipitation category.")

if ("PRCP" %in% names(model_dataset) & "satisfied" %in% names(model_dataset)) {
  
  prcp_data <- model_dataset[!is.na(PRCP) & !is.na(satisfied)]
  
  prcp_data[, prcp_category := data.table::fcase(
    PRCP == 0,              "No Rain\n(0 mm)",
    PRCP > 0 & PRCP <= 5,  "Light Rain\n(0–5 mm)",
    PRCP > 5 & PRCP <= 15, "Moderate Rain\n(5–15 mm)",
    PRCP > 15,             "Heavy Rain\n(> 15 mm)",
    default = NA_character_
  )]
  
  prcp_summary <- prcp_data[
    !is.na(prcp_category),
    .(
      sat_rate = mean(satisfied, na.rm = TRUE) * 100,
      n        = .N
    ),
    by = prcp_category
  ]
  
  # Fix factor order from dry to wet
  prcp_summary[, prcp_category := factor(prcp_category, levels = c(
    "No Rain\n(0 mm)", "Light Rain\n(0–5 mm)",
    "Moderate Rain\n(5–15 mm)", "Heavy Rain\n(> 15 mm)"
  ))]
  
  overall_sat <- mean(model_dataset$satisfied, na.rm = TRUE) * 100
  
  plot_a <- ggplot2::ggplot(
    prcp_summary,
    ggplot2::aes(x = prcp_category, y = sat_rate)
  ) +
    ggplot2::geom_col(fill = FILL_SAT, width = 0.55) +
    ggplot2::geom_hline(
      yintercept = overall_sat,
      linetype   = "dashed",
      colour     = "#333333",
      linewidth  = 0.7
    ) +
    ggplot2::annotate(
      "text", x = 0.6, y = overall_sat + 0.8,
      label  = sprintf("Overall: %.1f%%", overall_sat),
      hjust  = 0, size = 3.5, colour = "#333333"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%\n(n=%s)", sat_rate, format(n, big.mark = ","))),
      vjust  = -0.4, size = 3.5, fontface = "bold", colour = "#333333"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title    = "Satisfaction Rate by Precipitation Category",
      subtitle = "Share of reviews rated as satisfied (stars > 3.5) by daily precipitation level",
      x        = "Precipitation Category",
      y        = "Satisfaction Rate (%)"
    ) +
    plot_theme
  
  ggplot2::ggsave(PLOT_PRECIP_PATH, plot_a, width = 8, height = 5, dpi = 300)
  log_message("Saved Plot A to: ", PLOT_PRECIP_PATH)
  
} else {
  log_message("WARNING: PRCP or satisfied column missing. Skipping Plot A.")
}

# ================================================================
# PLOT B: Satisfaction rate by temperature quartile (TMAX)
# ================================================================
log_message("Creating Plot B: satisfaction rate by temperature quartile.")

if ("TMAX" %in% names(model_dataset) & "satisfied" %in% names(model_dataset)) {
  
  temp_data <- model_dataset[!is.na(TMAX) & !is.na(satisfied)]
  
  # Compute quartile breaks from the data
  q_breaks <- quantile(temp_data$TMAX, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  temp_data[, temp_quartile := cut(
    TMAX,
    breaks         = q_breaks,
    include.lowest = TRUE,
    labels         = c(
      sprintf("Q1\n(≤ %.1f°C)", q_breaks[2]),
      sprintf("Q2\n(%.1f – %.1f°C)", q_breaks[2], q_breaks[3]),
      sprintf("Q3\n(%.1f – %.1f°C)", q_breaks[3], q_breaks[4]),
      sprintf("Q4\n(> %.1f°C)", q_breaks[4])
    )
  )]
  
  temp_summary <- temp_data[
    !is.na(temp_quartile),
    .(
      sat_rate = mean(satisfied, na.rm = TRUE) * 100,
      n        = .N
    ),
    by = temp_quartile
  ][order(temp_quartile)]
  
  overall_sat <- mean(model_dataset$satisfied, na.rm = TRUE) * 100
  
  plot_b <- ggplot2::ggplot(
    temp_summary,
    ggplot2::aes(x = temp_quartile, y = sat_rate)
  ) +
    ggplot2::geom_col(fill = FILL_SAT, width = 0.55) +
    ggplot2::geom_hline(
      yintercept = overall_sat,
      linetype   = "dashed",
      colour     = "#333333",
      linewidth  = 0.7
    ) +
    ggplot2::annotate(
      "text", x = 0.6, y = overall_sat + 0.8,
      label  = sprintf("Overall: %.1f%%", overall_sat),
      hjust  = 0, size = 3.5, colour = "#333333"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%\n(n=%s)", sat_rate, format(n, big.mark = ","))),
      vjust  = -0.4, size = 3.5, fontface = "bold", colour = "#333333"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title    = "Satisfaction Rate by Temperature Quartile",
      subtitle = "Share of reviews rated as satisfied (stars > 3.5) by daily maximum temperature (TMAX)",
      x        = "Temperature Quartile",
      y        = "Satisfaction Rate (%)"
    ) +
    plot_theme
  
  ggplot2::ggsave(PLOT_TEMP_PATH, plot_b, width = 8, height = 5, dpi = 300)
  log_message("Saved Plot B to: ", PLOT_TEMP_PATH)
  
} else {
  log_message("WARNING: TMAX or satisfied column missing. Skipping Plot B.")
}

# ================================================================
# PLOT C: Satisfaction rate by restaurant price range
# ================================================================
log_message("Creating Plot C: satisfaction rate by price range.")

if ("attr_price_range" %in% names(model_dataset) & "satisfied" %in% names(model_dataset)) {
  
  price_data <- model_dataset[!is.na(attr_price_range) & !is.na(satisfied)]
  
  price_data[, price_label := data.table::fcase(
    attr_price_range == 1, "$ (Budget)",
    attr_price_range == 2, "$$ (Mid-range)",
    attr_price_range == 3, "$$$ (Upscale)",
    attr_price_range == 4, "$$$$ (Fine Dining)",
    default = NA_character_
  )]
  
  price_summary <- price_data[
    !is.na(price_label),
    .(
      sat_rate = mean(satisfied, na.rm = TRUE) * 100,
      n        = .N
    ),
    by = .(price_label, attr_price_range)
  ][order(attr_price_range)]
  
  price_summary[, price_label := factor(price_label, levels = price_summary$price_label)]
  
  overall_sat <- mean(model_dataset$satisfied, na.rm = TRUE) * 100
  
  plot_c <- ggplot2::ggplot(
    price_summary,
    ggplot2::aes(x = price_label, y = sat_rate)
  ) +
    ggplot2::geom_col(fill = FILL_SAT, width = 0.55) +
    ggplot2::geom_hline(
      yintercept = overall_sat,
      linetype   = "dashed",
      colour     = "#333333",
      linewidth  = 0.7
    ) +
    ggplot2::annotate(
      "text", x = 0.6, y = overall_sat + 0.8,
      label  = sprintf("Overall: %.1f%%", overall_sat),
      hjust  = 0, size = 3.5, colour = "#333333"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.1f%%\n(n=%s)", sat_rate, format(n, big.mark = ","))),
      vjust  = -0.4, size = 3.5, fontface = "bold", colour = "#333333"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 100),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title    = "Satisfaction Rate by Price Range",
      subtitle = "Share of reviews rated as satisfied (stars > 3.5) by restaurant price tier",
      x        = "Price Range",
      y        = "Satisfaction Rate (%)"
    ) +
    plot_theme
  
  ggplot2::ggsave(PLOT_PRICE_PATH, plot_c, width = 8, height = 5, dpi = 300)
  log_message("Saved Plot C to: ", PLOT_PRICE_PATH)
  
} else {
  log_message("WARNING: attr_price_range or satisfied column missing. Skipping Plot C.")
}

# ================================================================
# PLOT D: Satisfaction rate by top restaurant categories
# ================================================================
log_message("Creating Plot D: satisfaction rate by restaurant category.")

if ("categories" %in% names(model_dataset) & "satisfied" %in% names(model_dataset)) {
  
  # Identify top 12 categories by review count (excluding generic "Restaurants")
  category_counter <- new.env(parent = emptyenv())
  cat_values <- model_dataset$categories
  cat_values[is.na(cat_values)] <- ""
  
  for (cat_string in cat_values) {
    cats <- trimws(unlist(strsplit(cat_string, ",", fixed = TRUE)))
    cats <- cats[nzchar(cats) & tolower(cats) != "restaurants"]
    for (cat in cats) {
      current <- category_counter[[cat]]
      if (is.null(current)) current <- 0L
      category_counter[[cat]] <- current + 1L
    }
  }
  
  cat_names  <- ls(category_counter)
  cat_counts <- vapply(cat_names, function(x) category_counter[[x]], integer(1))
  top_cats   <- names(sort(cat_counts, decreasing = TRUE))[seq_len(min(12L, length(cat_counts)))]
  
  # For each top category, compute satisfaction rate
  cat_results <- lapply(top_cats, function(cat) {
    pattern  <- paste0("(^|,\\s*)", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", cat), "(\\s*,|$)")
    in_cat   <- grepl(pattern, model_dataset$categories, perl = TRUE)
    sat_rate <- mean(model_dataset$satisfied[in_cat], na.rm = TRUE) * 100
    n        <- sum(in_cat)
    data.table::data.table(category = cat, sat_rate = sat_rate, n = n)
  })
  
  cat_summary <- data.table::rbindlist(cat_results)
  data.table::setorder(cat_summary, -sat_rate)
  cat_summary[, category := factor(category, levels = rev(category))]
  
  overall_sat <- mean(model_dataset$satisfied, na.rm = TRUE) * 100
  
  plot_d <- ggplot2::ggplot(
    cat_summary,
    ggplot2::aes(x = category, y = sat_rate)
  ) +
    ggplot2::geom_col(fill = FILL_SAT, width = 0.65) +
    ggplot2::geom_vline(
      xintercept = NA,  # placeholder; hline below handles reference
      linetype   = "dashed"
    ) +
    ggplot2::geom_hline(
      yintercept = overall_sat,
      linetype   = "dashed",
      colour     = "#333333",
      linewidth  = 0.7
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = sprintf("%.1f%%  (n=%s)", sat_rate, format(n, big.mark = ","))
      ),
      hjust  = -0.05, size = 3.3, colour = "#333333"
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 105),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title    = "Satisfaction Rate by Restaurant Category",
      subtitle = "Share of reviews rated as satisfied (stars > 3.5) for the 12 most frequent categories",
      x        = NULL,
      y        = "Satisfaction Rate (%)"
    ) +
    plot_theme +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_line(colour = "#EEEEEE"),
                   panel.grid.major.y = ggplot2::element_blank())
  
  ggplot2::ggsave(PLOT_CATEGORY_PATH, plot_d, width = 9, height = 6, dpi = 300)
  log_message("Saved Plot D to: ", PLOT_CATEGORY_PATH)
  
} else {
  log_message("WARNING: categories or satisfied column missing. Skipping Plot D.")
}

# ================================================================
# PLOT E: Review volume over time (by year)
# ================================================================
log_message("Creating Plot E: review volume over time.")

if ("review_date" %in% names(model_dataset) & "satisfied" %in% names(model_dataset)) {
  
  time_data <- model_dataset[!is.na(review_date) & !is.na(satisfied)]
  time_data[, year := lubridate::year(as.Date(review_date))]
  
  time_summary <- time_data[
    ,
    .(
      total    = .N,
      n_sat    = sum(satisfied == 1L, na.rm = TRUE),
      n_unsat  = sum(satisfied == 0L, na.rm = TRUE),
      sat_rate = mean(satisfied, na.rm = TRUE) * 100
    ),
    by = year
  ][order(year)]
  
  # Reshape to long for stacked bar
  time_long <- data.table::melt(
    time_summary,
    id.vars    = "year",
    measure.vars = c("n_unsat", "n_sat"),
    variable.name = "group",
    value.name    = "count"
  )
  time_long[, group := data.table::fifelse(
    group == "n_sat", "Satisfied", "Not Satisfied"
  )]
  time_long[, group := factor(group, levels = c("Not Satisfied", "Satisfied"))]
  
  plot_e <- ggplot2::ggplot(
    time_long,
    ggplot2::aes(x = factor(year), y = count, fill = group)
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_fill_manual(
      values = c("Not Satisfied" = FILL_UNSAT, "Satisfied" = FILL_SAT)
    ) +
    ggplot2::scale_y_continuous(
      labels = function(x) format(x, big.mark = ","),
      expand = c(0, 0)
    ) +
    ggplot2::labs(
      title    = "Review Volume Over Time",
      subtitle = "Number of restaurant reviews per year, split by satisfaction outcome",
      x        = "Year",
      y        = "Number of Reviews",
      fill     = "Satisfaction"
    ) +
    plot_theme +
    ggplot2::theme(legend.position = "bottom")
  
  ggplot2::ggsave(PLOT_VOLUME_PATH, plot_e, width = 10, height = 5, dpi = 300)
  log_message("Saved Plot E to: ", PLOT_VOLUME_PATH)
  
} else {
  log_message("WARNING: review_date or satisfied column missing. Skipping Plot E.")
}

log_message("09_bivariate_descriptive_plots.R completed.")

# ================================================================
# COMBINED: 2x2 grid of Plots A, B, C, D
# ================================================================
log_message("Creating combined 2x2 grid plot.")

if (exists("plot_a") && exists("plot_b") && exists("plot_c") && exists("plot_d")) {
  
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    install.packages("patchwork", repos = "https://cloud.r-project.org")
  }
  
  library(patchwork)
  
  # Strip individual subtitles for the combined version
  strip_subtitle <- function(plot) {
    plot + ggplot2::labs(subtitle = NULL)
  }
  
  combined_plot <- (strip_subtitle(plot_a) | strip_subtitle(plot_b)) /
    (strip_subtitle(plot_c) | strip_subtitle(plot_d)) +
    patchwork::plot_annotation(
      title    = "Bivariate Descriptive Analysis: Satisfaction Rate by Key Variables",
      subtitle = "Share of reviews rated as satisfied (stars > 3.5) across weather conditions, price, and restaurant category",
      tag_levels = "A",
      theme = ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold", size = 14),
        plot.subtitle = ggplot2::element_text(colour = "#555555", size = 10)
      )
    )
  
  PLOT_GRID_PATH <- file.path(OUTPUT_DIR, "descriptive_bivariate_grid.png")
  ggplot2::ggsave(PLOT_GRID_PATH, combined_plot, width = 16, height = 12, dpi = 300)
  log_message("Saved combined 2x2 grid to: ", PLOT_GRID_PATH)
  
} else {
  log_message("WARNING: One or more of plots A-D were not created. Skipping combined grid.")
}