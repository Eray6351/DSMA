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
