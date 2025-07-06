####################################################
# R CODE 5: Predictive Modeling & Analysis
####################################################

# ==================================================
# 0. Setup: Package Installation and Loading
# ==================================================
# Define a list of required packages for the analysis
# DALEX is added for model-agnostic permutation importance
packages <- c("caret", "ranger", "xgboost", "pROC", "ggplot2", "yardstick",
              "purrr", "readr", "dplyr", "tidyr", "doParallel",
              "tibble", "kableExtra", "knitr", "patchwork", "forcats",
              "gridExtra","tidytext", "ComplexUpset", "ggforce", "purrr", "scales",
              "DALEX") # Added DALEX for advanced feature importance

# Install any packages not already installed
installed <- rownames(installed.packages())
for (pkg in packages) {
  if (!(pkg %in% installed)) install.packages(pkg, dependencies = TRUE)
}

# Load all packages into the R session
lapply(packages, library, character.only = TRUE)

# ==================================================
# 0.1 Enable Parallel Processing (for desktop use)
# ==================================================
# Detect number of CPU cores available
num_cores <- parallel::detectCores()

# If more than one core is available, enable parallel backend for future use
if (num_cores > 1) {
  cl <- makePSOCKcluster(num_cores - 1)  # Use all but one core
  registerDoParallel(cl)
}

# ==================================================
# 0.2 Timestamp & Folder Setup
# ==================================================
# Generate a timestamp to label outputs uniquely
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")

# Create an output folder if it does not exist
# Ensure this path is correct for your environment
setwd("/cloud/project/outputs")
if (!dir.exists("outputs")) dir.create("outputs")

# ==================================================
# 1. Load Datasets
# ==================================================
# Load the VIF (Variance Inflation Factor) and no-VIF datasets.
# Rename the outcome variable to 'adherence' and convert it to a factor
# with levels "X0" and "X1" as required by caret for binary classification.
vif_data <- read_csv("wideMonthlyAE_ForModeling_vif.csv") %>%
  rename(adherence = adherence_binary) %>%
  mutate(adherence = factor(adherence, levels = c(0,1), labels = c("X0", "X1")))

novif_data <- read_csv("wideMonthlyAE_ForModeling_novif.csv") %>%
  rename(adherence = adherence_binary) %>%
  mutate(adherence = factor(adherence, levels = c(0,1), labels = c("X0", "X1")))

# ==================================================
# 2. Data Partition
# ==================================================
# Split each dataset into 80% training and 20% test set using stratified sampling
# to ensure similar proportions of the outcome variable in both sets.
set.seed(123) # For reproducibility of the partition
vif_index <- createDataPartition(vif_data$adherence, p = 0.8, list = FALSE)
vif_train <- vif_data[vif_index, ]
vif_test  <- vif_data[-vif_index, ]

novif_index <- createDataPartition(novif_data$adherence, p = 0.8, list = FALSE)
novif_train <- novif_data[novif_index, ]
novif_test  <- novif_data[-novif_index, ]

# ==================================================
# 3. Training Control
# ==================================================
# Set up model training control using repeated 5-fold cross-validation (3 repeats).
# This provides robust estimates of model performance.
# `classProbs = TRUE` is essential for calculating AUC.
# `summaryFunction = twoClassSummary` specifies AUC (ROC) as the primary performance metric.
ctrl <- trainControl(
  method = "repeatedcv", number = 5, repeats = 3,
  classProbs = TRUE, summaryFunction = twoClassSummary, # uses ROC as performance
  savePredictions = "final", allowParallel = TRUE # Allow parallel processing
)

# ==================================================
# 4. Tuning Grids
# ==================================================
# Set tuning parameters for Random Forest (ranger package).
# `mtry` is set to the square root of the number of features, a common heuristic.
rf_grid <- expand.grid(
  mtry = floor(sqrt(ncol(vif_train) - 1)),
  splitrule = "gini",
  min.node.size = 10
)

# Set tuning parameters for XGBoost.
# These are typical default values for a shallow tree-based ensemble.
xgb_grid <- expand.grid(
  nrounds = 100,
  max_depth = 3,
  eta = 0.1,
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

# ==================================================
# 5. Train Models - VIF Dataset
# ==================================================
# Fit Generalized Linear Model (Logistic Regression) to the VIF dataset.
model_glm_vif <- train(adherence ~ ., data = vif_train, method = "glm",
                       family = binomial(), trControl = ctrl, metric = "ROC")
saveRDS(model_glm_vif, paste0("outputs/model_glm_vif_", timestamp, ".rds"))

# Fit Random Forest model to the VIF dataset.
# `importance = 'impurity'` ensures feature importance is calculated during training.
model_rf_vif <- train(adherence ~ ., data = vif_train, method = "ranger",
                      trControl = ctrl, tuneGrid = rf_grid, metric = "ROC", importance = 'impurity')
saveRDS(model_rf_vif, paste0("outputs/model_rf_vif_", timestamp, ".rds"))

# Fit XGBoost model to the VIF dataset.
model_xgb_vif <- train(adherence ~ ., data = vif_train, method = "xgbTree",
                       trControl = ctrl, tuneGrid = xgb_grid, metric = "ROC")
saveRDS(model_xgb_vif, paste0("outputs/model_xgb_vif_", timestamp, ".rds"))

# ==================================================
# 6. Train Models - No-VIF Dataset
# ==================================================
# Repeat the entire model training process using the No-VIF dataset.
# This dataset has features with lower multicollinearity.
model_glm_novif <- train(adherence ~ ., data = novif_train, method = "glm",
                         family = binomial(), trControl = ctrl, metric = "ROC")
saveRDS(model_glm_novif, paste0("outputs/model_glm_novif_", timestamp, ".rds"))

model_rf_novif <- train(adherence ~ ., data = novif_train, method = "ranger",
                        trControl = ctrl, tuneGrid = rf_grid, metric = "ROC", importance = 'impurity')
saveRDS(model_rf_novif, paste0("outputs/model_rf_novif_", timestamp, ".rds"))

model_xgb_novif <- train(adherence ~ ., data = novif_train, method = "xgbTree",
                         trControl = ctrl, tuneGrid = xgb_grid, metric = "ROC")
saveRDS(model_xgb_novif, paste0("outputs/model_xgb_novif_", timestamp, ".rds"))

# ==================================================
# 7. Evaluation Function
# ==================================================
# Custom function to calculate a comprehensive set of test metrics
# including Accuracy, Precision, Recall, F1-score, and AUC.

evaluate_model <- function(model, test_data) {
  pred <- predict(model, test_data)                         # predicted class
  prob <- predict(model, test_data, type = "prob")[, "X1"]  # predicted probability for "X1"
  obs <- test_data$adherence                                # actual class
  auc_val <- tryCatch(as.numeric(auc(roc(obs, prob))), error = function(e) NA)
  acc <- mean(pred == obs)
  eval_df <- tibble(truth = obs, estimate = pred)
  prec <- precision(eval_df, truth = truth, estimate = estimate, event_level = "second")$.estimate
  rec  <- recall(eval_df, truth = truth, estimate = estimate, event_level = "second")$.estimate
  f1   <- f_meas(eval_df, truth = truth, estimate = estimate, event_level = "second")$.estimate
  tibble(Accuracy = acc, Precision = prec, Recall = rec, F1 = f1, AUC = auc_val)
}


# ==================================================
# 8. Collect Metrics
# ==================================================
# Evaluate all trained models on their respective test sets (VIF and No-VIF).
# Combine results into a single tibble for easy comparison and export.
metrics_vif <- bind_rows(
  evaluate_model(model_glm_vif, vif_test) %>% mutate(Model = "GLM"),
  evaluate_model(model_rf_vif, vif_test) %>% mutate(Model = "RF"),
  evaluate_model(model_xgb_vif, vif_test) %>% mutate(Model = "XGB")
) %>% mutate(Dataset = "VIF") %>% relocate(Dataset, Model)

metrics_novif <- bind_rows(
  evaluate_model(model_glm_novif, novif_test) %>% mutate(Model = "GLM"),
  evaluate_model(model_rf_novif, novif_test) %>% mutate(Model = "RF"),
  evaluate_model(model_xgb_novif, novif_test) %>% mutate(Model = "XGB")
) %>% mutate(Dataset = "No-VIF") %>% relocate(Dataset, Model)

# Combine results for both datasets and write to disk.
metrics_all <- bind_rows(metrics_vif, metrics_novif)
write_csv(metrics_all, paste0("outputs/Model_Comparison_", timestamp, ".csv"))
# Using `print()` directly is often sufficient instead of `sink()` for simple text output
capture.output(print(metrics_all), file = paste0("outputs/Model_Evaluation_Summary_", timestamp, ".txt"))

# Print to console for immediate review
print(metrics_all)

# Display in Viewer Pane as a copy-paste-friendly HTML table
metrics_all %>%
  kable(format = "html", digits = 3, caption = "Evaluation Metrics: VIF and No-VIF Datasets") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = F, position = "center")

# ==================================================
# 9. Final Model Evaluation Visualizations – No-VIF Dataset Only
# ==================================================
# Focus visualizations on the No-VIF dataset for clarity, assuming it's the preferred choice.

# Shared color palette for consistent model representation
model_colors <- c("GLM" = "#F4A6A6", "RF" = "#8FD694", "XGB" = "#A4B9E5")

# Filter for No-VIF metrics and reshape to long format for ggplot2.
# Ensure consistent ordering of models for plotting.
metrics_long_novif <- metrics_all %>%
  filter(Dataset == "No-VIF") %>%
  pivot_longer(cols = c(Accuracy, Precision, Recall, F1, AUC),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Model = factor(Model, levels = c("RF", "XGB", "GLM"))) # Order for plotting

# ==================================================
# 9A. Fully Overlapping Plot – GLM in Front
# ==================================================
# This plot visually compares models with GLM highlighted.
p_all_full <- ggplot(metrics_long_novif, aes(x = Metric, y = Value)) +
  geom_col(data = metrics_long_novif %>% filter(Model != "GLM"),
           aes(fill = Model), alpha = 0.25, width = 0.6, color = "gray40", position = "identity") +
  geom_col(data = metrics_long_novif %>% filter(Model == "GLM"),
           aes(fill = Model), alpha = 1, width = 0.6, color = "black", position = "identity") +
  geom_text(data = metrics_long_novif %>% filter(Model == "GLM"),
            aes(label = round(Value, 3)), vjust = -1.1, size = 3.2, color = "black") +
  scale_fill_manual(values = model_colors) +
  geom_hline(yintercept = 0.5, linetype = "dotted", color = "gray40") +
  labs(title = "Fully Overlapping Evaluation Scores",
       subtitle = "GLM Highlighted in Front",
       x = "Metric", y = "Score", fill = "Model") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

# Zoomed-in version for clearer visualization of smaller differences.
p_all_zoom <- p_all_full +
  coord_cartesian(ylim = c(0.7, 1)) + # Zoom to relevant performance range
  labs(title = "Zoomed-In (0.7–1.0)",
       subtitle = "Marginal Differences Across Models")

# Print and save plots
print(p_all_full)
print(p_all_zoom)

ggsave(paste0("outputs/FinalModel_Eval_Overlay_NoVIF_Full_", timestamp, ".png"),
       plot = p_all_full, width = 14, height = 6, dpi = 300)

ggsave(paste0("outputs/FinalModel_Eval_Overlay_NoVIF_Zoom_", timestamp, ".png"),
       plot = p_all_zoom, width = 14, height = 6, dpi = 300)

# ==================================================
# 9B. Dodged Bar Comparison (No-VIF Only)
# ==================================================
# Bar chart with models dodged for direct comparison per metric.
p_dodge <- ggplot(metrics_long_novif, aes(x = Model, y = Value, fill = Model)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, color = "black") +
  facet_wrap(~Metric, scales = "free_y") + # Separate panels for each metric
  scale_fill_manual(values = model_colors) +
  labs(
    title = "Dodged Bar Comparison of Models",
    x = "Model", y = "Score", fill = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 0))

print(p_dodge)

ggsave(paste0("outputs/FinalModel_Dodged_Bar_Comparison_NoVIF_", timestamp, ".png"),
       plot = p_dodge, width = 14, height = 6, dpi = 300)

# ==================================================
# 9C. Side-by-Side Zoom vs Full (No-VIF Only)
# ==================================================
# Combine the full and zoomed overlapping plots using patchwork.
combined_plot_novif <- p_all_full + p_all_zoom + plot_layout(ncol = 2) +
  plot_annotation(title = "Model Evaluation Scores: Full vs. Zoomed View\nGLM Highlighted for Comparison")

print(combined_plot_novif)

ggsave(filename = paste0("outputs/FinalModel_Eval_Overlay_NoVIF_Combined_", timestamp, ".png"),
       plot = combined_plot_novif, width = 14, height = 6, dpi = 300)

# ... (Previous code remains the same until Section 10) ...

# ==================================================
# 10. Feature Importance Analysis & Visualization (No-VIF Dataset Only)
# ==================================================

# GLM: Use absolute value of standardized coefficients as importance
glm_importance <- summary(model_glm_novif)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("Feature") %>%
  filter(Feature != "(Intercept)") %>%
  mutate(Importance = abs(Estimate), type = "GLM") %>%
  select(Feature, Importance, type)

# Random Forest
rf_importance_raw <- importance(model_rf_novif$finalModel)
rf_importance <- tibble(
  Feature = names(rf_importance_raw),
  Importance = as.numeric(rf_importance_raw),
  type = "Random Forest"
)

# XGBoost
xgb_importance <- xgb.importance(model = model_xgb_novif$finalModel) %>%
  select(Feature, Gain) %>%
  rename(Importance = Gain) %>%
  mutate(Importance = as.numeric(Importance), type = "XGBoost")

# Combine all
combined_importance <- bind_rows(glm_importance, rf_importance, xgb_importance)

# Filter out low-importance values to clean plot
min_importance_threshold <- 0.001
filtered_combined_importance <- combined_importance %>%
  filter(Importance > min_importance_threshold)

# Get Top 10 per model
top_features_table <- filtered_combined_importance %>%
  group_by(type) %>%
  top_n(10, Importance) %>%
  arrange(type, desc(Importance)) %>%
  select(type, Feature, Importance) %>%
  ungroup()

# Save CSV and print
write_csv(top_features_table, paste0("outputs/Top_Features_NoVIF_", timestamp, ".csv"))
print(top_features_table)
grid.table(top_features_table)

# ==================================================
# 10.1 Refined Lollipop Plot
# ==================================================
# ==================================================
# Lollipop Plot with Overlapping Y-Axis Across Facets
# Preserves top 10 per model but aligns shared features
# ==================================================

# Step 1: Select top 10 per model
top10_by_model <- combined_importance %>%
  group_by(type) %>%
  top_n(10, Importance) %>%
  ungroup()

# Step 2: Create unified Feature levels for consistent y-axis across facets
# Combine all top features across models
shared_order <- top10_by_model %>%
  group_by(Feature) %>%
  summarise(MaxImp = max(Importance, na.rm = TRUE)) %>%
  arrange(desc(MaxImp)) %>%
  pull(Feature)

# Step 3: Apply shared factor levels to Feature
top10_by_model <- top10_by_model %>%
  mutate(
    type = factor(type, levels = c("GLM", "Random Forest", "XGBoost")),
    Feature = factor(Feature, levels = rev(shared_order))  # reverse for top-to-bottom
  )

# Step 4: Plot
p_lolli_overlap <- ggplot(top10_by_model, aes(x = Importance, y = Feature, color = type)) +
  geom_segment(aes(x = 0, xend = Importance, yend = Feature), linewidth = 0.6) +
  geom_point(size = 3) +
  geom_text(aes(label = round(Importance, 2)), hjust = -0.3, size = 3, show.legend = FALSE) +
  facet_wrap(~type, scales = "free_x") +
  scale_color_manual(values = c("GLM" = "#F4A6A6", "Random Forest" = "#8FD694", "XGBoost" = "#A4B9E5")) +
  labs(
    title = "Top 10 Features per Model",
    subtitle = "Lollipop plot with importance values and shared feature alignment",
    x = "Importance Score", y = "Feature"
  ) +
  scale_x_continuous(labels = scales::comma) +
  coord_cartesian(xlim = c(0, max(top10_by_model$Importance) * 1.15)) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Step 5: Save
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M")
ggsave(paste0("outputs/Top10_Lollipop_Aligned_", timestamp, ".png"),
       plot = p_lolli_overlap, width = 12, height = 8, dpi = 300)

# Optional: Display in console
print(p_lolli_overlap)


# ==================================================
# 10.2. Model-Agnostic Permutation Importance (using DALEX)
# ==================================================
# This section provides an alternative, more directly comparable measure of feature importance
# across different model types (GLM, RF, XGBoost) by using permutation importance.
# It quantifies how much model performance drops when a feature's values are shuffled.

# Create explainers for each trained model using the DALEX package.
# `data` should be the predictor variables, `y` the true outcome (numeric 0/1).
explainer_glm <- explain(model_glm_novif, data = novif_test %>% select(-adherence),
                         y = as.numeric(novif_test$adherence == "X1"), label = "GLM")
explainer_rf <- explain(model_rf_novif, data = novif_test %>% select(-adherence),
                        y = as.numeric(novif_test$adherence == "X1"), label = "Random Forest")
explainer_xgb <- explain(model_xgb_novif, data = novif_test %>% select(-adherence),
                         y = as.numeric(novif_test$adherence == "X1"), label = "XGBoost")

# Calculate permutation importance for each model.
# `loss_function = loss_one_minus_auc` specifies that the importance is measured
# by the drop in (1 - AUC) when a feature is permuted.
# Increased N to 100 for more robust estimates.
set.seed(123) # For reproducibility of permutation results
fi_glm_perm <- model_parts(explainer_glm, loss_function = loss_one_minus_auc, N = 100)
fi_rf_perm <- model_parts(explainer_rf, loss_function = loss_one_minus_auc, N = 100)
fi_xgb_perm <- model_parts(explainer_xgb, loss_function = loss_one_minus_auc, N = 100)

# This is crucial for understanding why previous plot was cluttered.
# Look for many features with identical or near-zero importance.
cat("\n--- Raw Random Forest Permutation Importance (top 20) ---\n")
print(fi_rf_perm %>% arrange(desc(dropout_loss)) %>% head(20))
cat("\n----------------------------------------------------------\n")

# Combine permutation importance results.
combined_perm_importance <- bind_rows(
  fi_glm_perm %>% mutate(model_type = "GLM"),
  fi_rf_perm %>% mutate(model_type = "Random Forest"),
  fi_xgb_perm %>% mutate(model_type = "XGBoost")
) %>%
  filter(variable != "_baseline_") %>% # Remove the baseline entry
  group_by(model_type, variable) %>%
  summarise(importance_mean = mean(dropout_loss), .groups = 'drop') %>% # Average across permutations
  rename(Feature = variable, Importance = importance_mean) %>%
  arrange(model_type, desc(Importance))

# Filter out features with negligible importance
# This addresses the issue of many features having very similar, near-zero importance
# which causes overplotting and unreadable labels. Adjust the threshold (e.g., 0.0001)
# based on the values you observe in the raw importance printout above.
filtered_perm_importance <- combined_perm_importance %>%
  filter(Importance > 0.0001) # <-- Adjust this threshold as needed based on your data

# Select top N features for visualization (e.g., top 10) from the *filtered* data.
top_perm_features <- filtered_perm_importance %>% # Use the filtered data here
  group_by(model_type) %>%
  top_n(10, Importance) %>%
  ungroup() # Ungroup for reordering in ggplot

# Visualize Permutation Importance using a grouped bar plot (similar to Section 10).
p_perm_bar <- ggplot(top_perm_features, aes(x = Importance, y = reorder_within(Feature, Importance, model_type), fill = model_type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~model_type, scales = "free_y") +
  scale_y_reordered() +
  scale_fill_manual(values = c("GLM" = "#F4A6A6", "Random Forest" = "#8FD694", "XGBoost" = "#A4B9E5")) +
  labs(
    title = "Top 10 Permutation Importance Features by Model",
    subtitle = "Importance measured by mean drop in (1 - AUC) after permutation",
    x = "Mean Drop in (1 - AUC)", y = "Feature"
  ) +
  theme_minimal(base_size = 14)

print(p_perm_bar)
# Increased width and height for better readability
ggsave(paste0("outputs/Top_Permutation_Features_BarPlot_NoVIF_", timestamp, ".png"),
       plot = p_perm_bar, width = 12, height = 8, dpi = 300)

# ==================================================
# 11. Health Board–Specific Prediction and Visualization (No-VIF Models)
# ==================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
library(yardstick)

# 1. Load and subset No-VIF dataset
noVIF_data <- read.csv("wideMonthlyAE_ForModeling_novif.csv") %>%
  mutate(adherence = factor(adherence_binary, levels = c(0, 1), labels = c("X0", "X1")))

sample_data <- noVIF_data %>%
  filter(health_board %in% c("G", "S")) %>%
  mutate(adherence = factor(adherence, levels = c("X0", "X1")),
         month_plot = as.Date(month))  # for plotting only

# 2. Predict using FinalModel trained on No-VIF
pred_glm <- predict(model_glm_novif, sample_data, type = "prob")[,2]
pred_rf  <- predict(model_rf_novif,  sample_data, type = "prob")[,2]
pred_xgb <- predict(model_xgb_novif, sample_data, type = "prob")[,2]

# 3. Combine predictions for plotting
viz_df <- sample_data %>%
  select(month, month_plot, health_board, adherence) %>%
  mutate(glm_pred = pred_glm,
         rf_pred  = pred_rf,
         xgb_pred = pred_xgb) %>%
  pivot_longer(cols = c("glm_pred", "rf_pred", "xgb_pred"),
               names_to = "model", values_to = "predicted")

# 4. Compute 95% Confidence Intervals via Bootstrapping
set.seed(123)
bootstrap_cis <- function(df, model_name) {
  boot_samples <- 100
  ci_list <- replicate(boot_samples, {
    samp <- df[sample(1:nrow(df), replace = TRUE), ]
    mean(samp$predicted - as.numeric(samp$adherence == "X1"))
  })
  error <- sd(ci_list)
  df %>%
    group_by(month_plot, health_board) %>%
    summarise(
      predicted_mean = mean(predicted),
      adherence = mean(as.numeric(adherence == "X1")),
      lower = predicted_mean - 1.96 * error,
      upper = predicted_mean + 1.96 * error,
      .groups = 'drop'
    ) %>%
    mutate(model = model_name)
}

ci_glm <- bootstrap_cis(viz_df %>% filter(model == "glm_pred"), "GLM")
ci_rf  <- bootstrap_cis(viz_df %>% filter(model == "rf_pred"), "Random Forest")
ci_xgb <- bootstrap_cis(viz_df %>% filter(model == "xgb_pred"), "XGBoost")

viz_ci <- bind_rows(ci_glm, ci_rf, ci_xgb) %>%
  mutate(health_board = dplyr::recode(health_board,
                                      "G" = "NHS Greater Glasgow and Clyde",
                                      "S" = "NHS Lothian"))

# 5. Plot: Predicted vs Actual with CI and actual baseline
p1 <- ggplot(viz_ci, aes(x = month_plot)) +
  geom_line(aes(y = adherence), color = "black", linewidth = 1.2) +  # black baseline
  geom_line(aes(y = predicted_mean, color = model), linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = model), alpha = 0.2) +
  facet_wrap(~ health_board, scales = "free_x") +
  scale_color_manual(values = c("GLM" = "#1b9e77", 
                                "Random Forest" = "#d95f02", 
                                "XGBoost" = "#7570b3")) +
  scale_fill_manual(values = c("GLM" = "#1b9e77", 
                               "Random Forest" = "#d95f02", 
                               "XGBoost" = "#7570b3")) +
  labs(title = "Predicted vs. Actual A&E 4-Hour Adherence",
       x = "Month", y = "Adherence Rate",
       color = "Model", fill = "95% CI") +
  theme_minimal(base_size = 13) +
  scale_y_continuous(labels = percent_format(accuracy = 1))

print(p1)
ggsave("outputs/FinalModel_HealthBoard_PredVsActual.png", p1, width = 12, height = 6, dpi = 300)

# 6. Residual plot by model and board
viz_df <- viz_df %>%
  mutate(actual_numeric = as.numeric(adherence == "X1"),
         residual = predicted - actual_numeric,
         model = case_when(
           model == "glm_pred" ~ "GLM",
           model == "rf_pred" ~ "Random Forest",
           model == "xgb_pred" ~ "XGBoost"
         ),
         health_board = dplyr::recode(health_board,
                                      "G" = "NHS Greater Glasgow and Clyde",
                                      "S" = "NHS Lothian"))

p2 <- ggplot(viz_df, aes(x = actual_numeric, y = residual, color = model)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ health_board) +
  labs(title = "Prediction Residuals by Model and Health Board",
       x = "Actual Adherence", y = "Residual (Predicted - Actual)") +
  theme_light(base_size = 13)

print(p2)
ggsave("outputs/FinalModel_HealthBoard_Residuals.png", p2, width = 10, height = 6, dpi = 300)

# 7. Patchwork Panel
combined_pane <- p1 + p2 +
  plot_annotation(
    title = "Predicted vs Actual and Residuals by Health Board",
    subtitle = "Sample: NHS Greater Glasgow and Clyde | NHS Lothian",
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(size = 12))
  )

print(combined_pane)
ggsave("outputs/FinalModel_HealthBoard_Combined_Patchwork.png",
       combined_pane, width = 14, height = 6, dpi = 300)

# 8. Model performance summary (RMSE and correlation)
summary_metrics <- viz_df %>%
  group_by(model, health_board) %>%
  summarise(
    RMSE = rmse_vec(truth = actual_numeric, estimate = predicted),
    Cor  = cor(actual_numeric, predicted),
    .groups = "drop"
  )

write.csv(summary_metrics, "outputs/FinalModel_HealthBoard_SummaryMetrics.csv", row.names = FALSE)

# ==================================================
# 12. Stop Parallel Backend
# ==================================================
# Gracefully shut down parallel workers (if used) to free up resources.
if (exists("cl")) stopCluster(cl)

# Final message
cat("\n✅ Model training and analysis complete. All files saved in /outputs/\n")

# ==================================================
# DONE
# ==================================================
